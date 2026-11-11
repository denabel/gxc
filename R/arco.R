# Experimental: daily statistics from ERA5-Land hourly data in the ECMWF
# Analysis Ready Cloud Optimised (ARCO) Zarr stores. Chunks are downloaded
# in parallel over HTTP, decoded (Blosc) and aggregated to daily values
# locally, without the CDS queue and without GDAL.
#
# Each indicator is available in two layouts:
# - geo-chunked:  long time series for small areas (chunks of ~3.9 years x 4 x 8 cells)
# - time-chunked: short periods for large areas (chunks of 1 hour x 1024 x 1024 cells)
# Whole chunks are always transferred, so the cheaper layout is chosen per
# group of requested days based on the estimated transfer size.

.arco_state      <- new.env(parent = emptyenv())
.arco_state$meta <- list()

# Typical compressed chunk sizes in MB, measured for t2m over Germany
# (2026-11). Only used to compare the two layouts, not as exact sizes.
.arco_chunk_mb <- c(geo = 2.3, time = 0.77)

# Maximum number of simultaneous HTTP connections
.arco_max_con <- 16L


# Requests daily ARCO data for the given dates. Returns one .tif per date,
# in the order of the input dates.
.request_arco_daily <- function(indicator,
                                extent,
                                years,
                                months,
                                days,
                                statistic = "daily_mean",
                                time_zone = "utc+00:00",
                                cache     = TRUE,
                                path      = NULL,
                                verbose   = TRUE) {
  .arco_notice()

  path  <- path %||% .default_download_dir(cache, service = "arco")
  dates <- as.Date(paste(years, months, days, sep = "-"))

  # Extent, statistic and time zone change the content, so they key the cache
  # The name prefix is truncated, so the full indicator goes into the key
  # along with everything else that changes the content
  key   <-
    substr(rlang::hash(list(indicator, extent, statistic, time_zone)), 1, 8)
  files <- file.path(
    path, "daily",
    paste0(substr(indicator, 1, 20), "_", key, "_", format(dates, "%Y%m%d"), ".tif")
  )

  missing <- sort(unique(dates[!file.exists(files)]))
  if (length(missing) == 0) return(files)

  rlang::check_installed(c("curl", "blosc", "jsonlite"), reason = "to use the ARCO catalogue.")
  fail_if_test()
  dir.create(file.path(path, "daily"), showWarnings = FALSE, recursive = TRUE)

  offset   <- .tz_offset_hours(time_zone)
  plan     <- .arco_plan(indicator, extent, missing, offset)
  n_chunks <- sum(vapply(plan, function(p) p$n_chunks, numeric(1)))

  info(
    "Reading {indicator} from ERA5-Land ARCO ({length(missing)} day{?s}, \\
    {n_chunks} chunk{?s})...",
    msg_done   = "Successfully read {indicator} from ERA5-Land ARCO.",
    msg_failed = "Failed to read {indicator} from ERA5-Land ARCO.",
    level      = "step"
  )

  for (p in plan) {
    daily <- .arco_read_daily(indicator, extent, p$days, statistic, offset, p$layout)
    for (d in seq_along(p$days)) {
      .write_raster_atomic(daily[[d]], files[match(p$days[d], dates)])
    }
  }

  files
}


# Groups the missing days by geo time chunk and picks the cheaper layout per
# group. Returns a list of list(days, layout, n_chunks).
.arco_plan <- function(indicator, extent, missing, offset) {
  geo  <- .arco_store_meta(indicator, "geo")
  time <- .arco_store_meta(indicator, "time")

  # Days not (yet) in the store form their own group, so the read aborts
  # with a clear message instead of skipping them
  ti     <- match(.arco_hours(missing, offset), geo$time) - 1L
  groups <- split(missing, ifelse(is.na(ti), -1, ti %/% geo$chunks[1]))

  lapply(unname(groups), function(days) {
    hours  <- .arco_day_hours(days, offset)
    n_geo  <- .arco_n_chunks(geo, extent, hours)
    n_time <- .arco_n_chunks(time, extent, hours)

    if (n_geo * .arco_chunk_mb[["geo"]] <= n_time * .arco_chunk_mb[["time"]]) {
      list(days = days, layout = "geo", n_chunks = n_geo)
    } else {
      list(days = days, layout = "time", n_chunks = n_time)
    }
  })
}


# Reads the hours of `days` from one store layout and aggregates them to a
# daily SpatRaster (one layer per day). Chunks are processed as they
# arrive, so memory stays bounded by the output size.
.arco_read_daily <- function(indicator, extent, days, statistic, offset, layout) {
  store <- .arco_stores[[indicator]]
  meta  <- .arco_store_meta(indicator, layout)
  cb    <- meta$chunks
  cells <- .arco_cells(meta, extent)

  # Output grid north to south, west to east
  rows_out <- cells$rows[order(meta$lat[cells$rows + 1], decreasing = TRUE)]
  cols_out <- cells$cols[order(meta$lon[cells$cols + 1])]
  n_cell   <- length(rows_out) * length(cols_out)

  hours       <- .arco_day_hours(days, offset)
  ti          <- match(hours, meta$time) - 1L
  day_of_hour <- rep(seq_along(days), each = 24)
  avail       <- !is.na(ti)

  n_per_day <- tabulate(day_of_hour[avail], nbins = length(days))
  if (any(n_per_day < 24)) {
    first_missing <- days[which(n_per_day < 24)[1]]
    cli::cli_abort(c(
      "ERA5-Land ARCO data are not available for all requested days.",
      "x" = "First incomplete day: {.val {first_missing}}.",
      "i" = "ARCO lags a few days behind real time; recent days may be \\
             available from {.val derived-era5-land-daily-statistics}."
    ), call = NULL)
  }

  fun  <- .arco_daily_fun(statistic)
  init <- switch(fun, max = -Inf, min = Inf, 0)
  acc  <- matrix(init, n_cell, length(days))

  tc   <- ti %/% cb[1]
  rc   <- cells$rows %/% cb[2]
  cc   <- cells$cols %/% cb[3]
  keys <- expand.grid(t = unique(tc), r = unique(rc), c = unique(cc))
  urls <- sprintf("%s/%s/%d.%d.%d", store[[layout]], store$array, keys$t, keys$r, keys$c)

  add_chunk <- function(i, content) {
    k <- keys[i, ]
    v <- if (is.null(content)) {
      rep(NaN, prod(cb))  # missing chunk = fill value
    } else {
      .arco_decode(content, meta$dtype, prod(cb), meta$compressor)
    }
    x <- array(v, dim = rev(cb))  # C order (time, lat, lon) -> [lon, lat, time]

    t_sel <- which(tc == k$t)
    r_sel <- cells$rows[rc == k$r]
    c_sel <- cells$cols[cc == k$c]

    m <- matrix(
      x[c_sel - k$c * cb[3] + 1, r_sel - k$r * cb[2] + 1, ti[t_sel] - k$t * cb[1] + 1, drop = FALSE],
      ncol = length(t_sel)
    )
    out_cell <- (rep(match(r_sel, rows_out), each = length(c_sel)) - 1) * length(cols_out) +
      rep(match(c_sel, cols_out), times = length(r_sel))
    d <- day_of_hour[t_sel]

    if (fun %in% c("mean", "sum")) {
      s  <- rowsum(t(m), d)
      dd <- as.integer(rownames(s))
      acc[out_cell, dd] <<- acc[out_cell, dd] + t(s)
    } else {
      agg <- if (fun == "max") pmax else pmin
      for (dd in unique(d)) {
        part <- apply(m[, d == dd, drop = FALSE], 1, fun)
        acc[out_cell, dd] <<- agg(acc[out_cell, dd], part)
      }
    }
  }

  .arco_fetch(urls, add_chunk)
  if (fun == "mean") acc <- acc / 24

  lat_out <- meta$lat[rows_out + 1]
  lon_out <- meta$lon[cols_out + 1]
  half    <- 0.05

  r <- terra::rast(
    nrows = length(rows_out), ncols = length(cols_out), nlyrs = length(days),
    xmin  = round(min(lon_out) - half, 6), xmax = round(max(lon_out) + half, 6),
    ymin  = round(min(lat_out) - half, 6), ymax = round(max(lat_out) + half, 6),
    crs   = "EPSG:4326"
  )
  terra::values(r) <- acc
  terra::time(r)   <- days
  r
}


# Zero-based grid rows and columns of a store inside the extent c(N, W, S, E).
.arco_cells <- function(meta, extent) {
  eps <- 1e-6
  list(
    rows = which(meta$lat >= extent[3] - eps & meta$lat <= extent[1] + eps) - 1L,
    cols = which(meta$lon >= extent[2] - eps & meta$lon <= extent[4] + eps) - 1L
  )
}


# Number of chunks a store needs for the given hours and extent.
.arco_n_chunks <- function(meta, extent, hours) {
  cells <- .arco_cells(meta, extent)
  ti    <- stats::na.omit(match(hours, meta$time) - 1L)
  length(unique(ti %/% meta$chunks[1])) *
    length(unique(cells$rows %/% meta$chunks[2])) *
    length(unique(cells$cols %/% meta$chunks[3]))
}


# Downloads chunks in parallel and passes each to on_chunk(index, content)
# as it arrives; content is NULL for chunks that do not exist (404). Only
# max_con downloads are queued at a time, so none of them times out while
# waiting for a free connection. Failed downloads are retried.
.arco_fetch <- function(urls, on_chunk, attempts = 5) {
  auth <- paste("Bearer", .ecmwfr_get_key())
  todo <- seq_along(urls)

  for (attempt in seq_len(attempts)) {
    failed  <- integer(0)
    reasons <- character(0)
    pool    <- curl::new_pool(total_con = .arco_max_con, host_con = .arco_max_con)
    queue   <- todo

    add_next <- function() {
      if (length(queue) == 0) return(invisible())
      ii    <- queue[1]
      queue <<- queue[-1]
      h <- curl::new_handle(url = urls[ii], timeout = 300, connecttimeout = 30)
      curl::handle_setheaders(h, Authorization = auth)
      curl::multi_add(
        h, pool = pool,
        done = function(r) {
          if (r$status_code == 200) {
            on_chunk(ii, r$content)
          } else if (r$status_code == 404) {
            on_chunk(ii, NULL)
          } else {
            failed  <<- c(failed, ii)
            reasons <<- c(reasons, paste("HTTP", r$status_code))
          }
          add_next()
        },
        fail = function(msg) {
          failed  <<- c(failed, ii)
          reasons <<- c(reasons, msg)
          add_next()
        }
      )
    }

    for (k in seq_len(min(.arco_max_con, length(queue)))) add_next()
    curl::multi_run(pool = pool)

    if (length(failed) == 0) return(invisible())
    todo <- failed
    Sys.sleep(2^attempt)
  }

  top <- names(sort(table(reasons), decreasing = TRUE))
  top <- top[seq_len(min(3, length(top)))]
  cli::cli_abort(c(
    "Failed to download {length(todo)} ERA5-Land ARCO chunk{?s} after {attempts} attempts.",
    "x" = "Most frequent reason{?s}: {top}."
  ), call = NULL)
}


# Single authenticated GET, used for metadata.
.arco_get <- function(url) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste("Bearer", .ecmwfr_get_key()))
  r <- curl::curl_fetch_memory(url, handle = h)
  if (r$status_code != 200) {
    cli::cli_abort("ARCO request failed with status {r$status_code}: {.url {url}}", call = NULL)
  }
  r$content
}


# Decodes a Zarr v2 chunk. Int64 is read as pairs of Int32 and the low word
# kept, which is exact for the coordinate values in these stores.
.arco_decode <- function(content, dtype, n, compressor) {
  raw <- if (is.null(compressor)) content else blosc::blosc_decompress(content)
  switch(
    dtype,
    "<f4" = readBin(raw, "numeric", n = n, size = 4, endian = "little"),
    "<f8" = readBin(raw, "numeric", n = n, size = 8, endian = "little"),
    "<i8" = readBin(raw, "integer", n = 2 * n, size = 4, endian = "little")[c(TRUE, FALSE)],
    cli::cli_abort("Unsupported Zarr dtype {.val {dtype}}.", call = NULL)
  )
}


.arco_zarray <- function(url, name) {
  jsonlite::fromJSON(rawToChar(.arco_get(paste0(url, "/", name, "/.zarray"))))
}


# Reads a complete 1-D coordinate array (latitude, longitude, time). A
# finely chunked time axis is reconstructed from its first chunk as a
# regular hourly sequence instead of downloading every chunk.
.arco_read_coord <- function(url, name) {
  za       <- .arco_zarray(url, name)
  n_chunks <- ceiling(za$shape / za$chunks)

  if (name == "time" && n_chunks > 50) {
    first <- .arco_decode(.arco_get(paste0(url, "/time/0")), za$dtype, za$chunks, za$compressor)
    return(first[1] + seq_len(za$shape) - 1)
  }

  parts <- vector("list", n_chunks)

  .arco_fetch(
    paste0(url, "/", name, "/", seq_len(n_chunks) - 1),
    function(i, content) {
      parts[[i]] <<- .arco_decode(content, za$dtype, za$chunks, za$compressor)
    }
  )
  unlist(parts)[seq_len(za$shape)]
}


# Grid and chunking of a store, read once per session: chunk sizes along
# (time, lat, lon), data type, compressor and the coordinate values. Time
# is in hours since 1970.
.arco_store_meta <- function(indicator, layout) {
  store <- .arco_stores[[indicator]]
  url   <- store[[layout]]
  if (!is.null(.arco_state$meta[[url]])) return(.arco_state$meta[[url]])

  za   <- .arco_zarray(url, store$array)
  meta <- list(
    chunks     = za$chunks,
    dtype      = za$dtype,
    compressor = za$compressor,
    lat        = .arco_read_coord(url, "latitude"),
    lon        = .arco_read_coord(url, "longitude"),
    time       = .arco_read_coord(url, "time")
  )
  .arco_state$meta[[url]] <- meta
  meta
}


# Maps CDS daily statistic names to aggregation functions.
.arco_daily_fun <- function(statistic) {
  switch(
    statistic,
    daily_mean    = "mean",
    daily_maximum = "max",
    daily_minimum = "min",
    daily_sum     = "sum"
  )
}


# UTC hour (hours since 1970) at which local day `date` starts.
.arco_hours <- function(date, offset) {
  as.numeric(as.POSIXct(paste(date, "00:00"), tz = "UTC")) / 3600 - offset
}


# All 24 UTC hours of each local day, day by day.
.arco_day_hours <- function(days, offset) {
  as.vector(outer(0:23, .arco_hours(days, offset), `+`))
}


# "utc+02:00" -> 2
.tz_offset_hours <- function(time_zone) {
  as.integer(sub("^utc([+-]\\d{2}):00$", "\\1", time_zone))
}


# Informs once per session that the ARCO catalogue is experimental.
.arco_notice <- function() {
  if (isTRUE(.arco_state$notified)) return(invisible())
  .arco_state$notified <- TRUE
  cli::cli_inform(c(
    "!" = "The {.val arco-era5-land-daily} catalogue is experimental.",
    "i" = "Data are read from ECMWF's ARCO store and aggregated locally; \\
           interface and caching may change."
  ))
}
