# helpers.R

test_geom <- function() {
  sf::st_sfc(
    sf::st_point(c(13.5, 51.0)),  # Sachsen
    sf::st_point(c(11.5, 48.5))   # Bayern
  )
}

test_pts <- function(seq = FALSE) {
  sf::st_sf(
    date = if (seq)
      seq(as_date("2014-08-01"), as_date("2014-08-02"), by = "1 day")
    else
      as_date(c("2014-08-01", "2014-08-01")),
    geometry = test_geom(),
    crs = 4326
  )
}

test_pts_dwd <- function(seq = FALSE) {
  sf::st_sf(
    date = if (seq)
      seq(as_date("2024-01-01"), as_date("2024-01-02"), by = "1 day")
    else
      as_date(c("2024-01-01", "2024-01-01")),
    geometry = sf::st_sfc(
      sf::st_point(c(13.5, 51.5)),  # Sachsen
      sf::st_point(c(11.5, 51.0))   # Thüringen
    ),
    crs = 4326
  )
}

test_cache <- function() {
  test_path(file.path("fixtures"))
}

# Updates the stash index to use absolute paths for the current environment,
# and restores the original index on exit
local_test_index <- function(cache, service = "ecmwfr", .envir = parent.frame()) {
  cache_sub <- file.path(cache, if (service == "dwd") "dwd" else "era5")
  stash     <- new_stash(cache_sub, service = service)
  old       <- stash$get()
  new       <- lapply(old, function(paths) {
    sapply(paths, function(x) {
      normalizePath(
        file.path(cache_sub, basename(x)),
        mustWork = FALSE
      )
    }, USE.NAMES = FALSE)
  })
  stash$write(new)
  do.call(
    on.exit,
    list(bquote({
      stash <- .(stash)
      stash$write(.(old))
    })),
    envir = .envir
  )
}

# Strips absolute paths in the index down to filenames only for portability
reset_test_index <- function(cache, service = "ecmwfr") {
  stash <- new_stash(cache, service = service)
  old   <- stash$get()
  new   <- lapply(old, function(paths) {
    sapply(paths, basename, USE.NAMES = FALSE)
  })
  stash$write(new)
}

# Direct extraction from cached observation raster using same request hash
.extract_direct <- function(cache, indicator, catalogue,
                            extent, years, months, days = NULL,
                            pts, layer = 1,
                            service      = "ecmwfr",
                            prefix       = "observation",
                            product_type = "monthly_averaged_reanalysis",
                            request_time = "00:00",
                            statistic    = "daily_mean",
                            time_zone    = "utc+00:00") {
  if (is.null(days)) {
    request         <- gxc:::.build_era5_monthly_request(
      indicator    = indicator,
      catalogue    = catalogue,
      extent       = extent,
      years        = years,
      months       = months,
      prefix       = prefix,
      product_type = product_type,
      request_time = request_time
    )
    expected_length <- length(unique(paste(years, months)))
  } else {
    request         <- gxc:::.build_era5_daily_request(
      indicator = indicator,
      catalogue = catalogue,
      extent    = extent,
      years     = years,
      months    = months,
      days      = days,
      prefix    = prefix,
      statistic = statistic,
      time_zone = time_zone
    )
    expected_length <- length(unique(paste(years, months, days)))
  }

  cache_sub <- file.path(cache, if (service == "dwd") "dwd" else "era5")
  stash    <- new_stash(cache_sub, service = service)
  restored <- stash$restore(request, expected_length)

  if (is.null(restored)) {
    cli::cli_abort("Observation files not found in cache.")
  }

  obs_raster <- gxc:::.safe_rast(restored)
  pts_r      <- sf::st_transform(pts, terra::crs(obs_raster))
  # FIX: .extract_values() (mit .drop_heavy_columns()) statt terra::extract()
  # direkt -- dieselbe Dispatch-Logik (Punkt/Polygon) wie im
  # Produktionscode (extract.R). Bei `pts` hier zwar meist ueberfluessig
  # (schlankes Test-Fixture, keine schweren Spalten), aber konsistent mit
  # dem Rest von gxc.
  .extract_values(obs_raster[[layer]], .drop_heavy_columns(pts_r))[[1]]
}

# Direct extraction from cached baseline rasters using same request hash
.extract_baseline_direct <- function(cache, indicator, catalogue,
                                     extent, years, months, days = NULL,
                                     pts, baseline_fun = mean,
                                     service      = "ecmwfr",
                                     product_type = "monthly_averaged_reanalysis",
                                     request_time = "00:00",
                                     statistic    = "daily_mean",
                                     time_zone    = "utc+00:00") {
  if (is.null(days)) {
    request         <- gxc:::.build_era5_monthly_request(
      indicator    = indicator,
      catalogue    = catalogue,
      extent       = extent,
      years        = years,
      months       = months,
      prefix       = "baseline",
      product_type = product_type,
      request_time = request_time
    )
    expected_length <- length(unique(paste(years, months)))
  } else {
    request         <- gxc:::.build_era5_daily_request(
      indicator = indicator,
      catalogue = catalogue,
      extent    = extent,
      years     = years,
      months    = months,
      days      = days,
      prefix    = "baseline",
      statistic = statistic,
      time_zone = time_zone
    )
    expected_length <- length(unique(paste(years, months, days)))
  }

  cache_sub <- file.path(cache, if (service == "dwd") "dwd" else "era5")
  stash    <- new_stash(cache_sub, service = service)
  restored <- stash$restore(request, expected_length)

  if (is.null(restored)) {
    cli::cli_abort("Baseline files not found in cache.")
  }

  rasters <- lapply(restored, function(f) gxc:::.safe_rast(f)[[1]])
  values  <- sapply(rasters, function(r) {
    pts_r <- sf::st_transform(pts, terra::crs(r))
    # FIX: gleiche Aenderung wie oben
    .extract_values(r, .drop_heavy_columns(pts_r))[[1]]
  })

  if (is.null(dim(values))) {
    values <- matrix(values, nrow = length(pts[[1]]))
  }
  apply(values, 1, baseline_fun)
}

# Returns the extent of a set of points in WGS84
.test_extent <- function(pts, buffer = 0) {
  prepared <- sf::st_transform(pts, 4326)
  # Keep in sync with the buffer=0 fix in link_daily.sf()/link_monthly.sf():
  # sf::st_buffer(x, 0) does NOT return x unchanged, it converts points to
  # (near-zero-area) polygons, which can shift the computed extent by a
  # tiny floating-point amount -- enough to change the request hash and
  # cause cache lookups here to miss requests that link_daily()/
  # link_monthly() itself found fine (since those also now skip buffering
  # entirely when buffer = 0).
  if (buffer > 0) {
    prepared <- sf::st_buffer(prepared, buffer)
  }
  gxc:::.get_extent(prepared)
}

fail_on_request <- function() {
  old <- do.call(options, as.list(c(".__gxc_fail_on_request__." = TRUE)))
  do.call(options, list(".__gxc_fail_on_forbidden__." = TRUE), envir = parent.frame())
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = parent.frame())
}

go_on_request <- function() {
  options(".__gxc_fail_on_request__." = NULL)
}

get_metag <- function(raster, name) {
  meta <- terra::metags(raster)
  meta[meta[, "name"] == name, "value"]
}

temp_file <- function(...) {
  file <- tempfile(...)
  file.create(file)
  normalizePath(file)
}

local_key <- function(service = "ecmwfr", user = NULL, key = "test",
                      .envir = parent.frame()) {
  user <- user %||% service
  keys <- keyring::key_list(service = service)
  if (user %in% keys$username) return(NULL)
  keyring::key_set_with_value(service, user = user, password = key)
  do.call(
    on.exit,
    list(bquote(
      keyring::key_delete(
        service = .(service),
        user    = .(user)
      )
    )),
    envir = .envir
  )
}
