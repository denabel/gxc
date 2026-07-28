#' Link with daily indicators
#'
#' @description Augments spatio-temporal data with daily indicators from the
#' Copernicus earth observation database (ERA5) or the German Weather Service
#' (DWD).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download daily statistics from the data source}
#'  \item{Link raster statistics back to input}
#'  \item{Optionally, add comparative statistics based on a baseline period}
#' }
#'
#' This function interfaces the daily means of ERA5 and DWD indicators. For
#' monthly means see \code{\link{link_monthly}}.
#'
#' @param .data An `sf` object containing the spatial data (polygons or points).
#' @param indicator Character string specifying the indicator to download
#'   (e.g., `"2m_temperature"` for ERA5 or `"air_temperature_mean"` for DWD).
#'   Allowed indicators differ by catalogue. See the **Details** section for
#'   available indicators.
#' @param date_var Character string specifying the name of the date variable
#'   in `.data`. Defaults to `"date"`.
#' @param time_span Integer specifying the time span in days for averaging the
#'   climate indicator values prior to linking with the spatial data. A value
#'   of `0` (default) uses only the exact date; values greater than `0`
#'   aggregate over a rolling window of that many days before the date.
#' @param time_lag Integer specifying the time lag in days to shift the
#'   `date_var` backward before extraction. Default is `0`.
#' @param buffer Numeric value specifying the buffer radius in metres to
#'   be applied around each geometry. The default is `0`, corresponding to a
#'   direct cell match; values greater than 0 generate a spatial buffer
#'   around each point for aggregated extraction.
#' @param baseline Either `FALSE` (default) or a character vector of length 2
#'   specifying the baseline period as start and end year. For example,
#'   `baseline = c("1980", "2010")` uses the years 1980 to 2010 as the
#'   baseline. If `FALSE`, no baseline calculation is performed.
#' @param baseline_fun Character string or function specifying how baseline
#'   layers are collapsed to a single reference value. Accepts one of
#'   `"mean"` (default), `"median"`, `"min"`, `"max"`, `"sd"`, or percentiles
#'   `"p05"`, `"p10"`, `"p20"`, `"p80"`, `"p90"`, `"p95"`. A custom function
#'   is also accepted but will be labelled `"custom"` in output metadata.
#'   Ignored when `baseline = FALSE`.
#' @param study_fun Character string or function specifying how focal period
#'   layers are collapsed to a single summary value. Same options as
#'   `baseline_fun`. Only relevant when `time_span > 0`. Default is `"mean"`.
#' @param stat_wrangling Character string specifying how the focal value is
#'   compared to the baseline reference value. One of:
#'   \describe{
#'     \item{`"deviation"`}{Difference between focal value and baseline
#'       reference value (default).}
#'     \item{`"sd_deviation"`}{Deviation expressed in units of the baseline
#'       standard deviation (z-score).}
#'     \item{`"count_above"`}{Number of days in the focal period that exceed
#'       the baseline reference value. Requires `time_span > 0`.}
#'     \item{`"count_below"`}{Number of days in the focal period that fall
#'       below the baseline reference value. Requires `time_span > 0`.}
#'   }
#'   Ignored when `baseline = FALSE`.
#' @param prefix Character string appended to all output column names
#'   (e.g. `prefix = "temp_7d"` produces `.study_temp_7d`, `.result_temp_7d`
#'   etc.). Useful when calling `link_daily()` multiple times on the same
#'   dataset. Default is `NULL` (no prefix).
#' @param catalogue Character string specifying which catalogue to use.
#'   Options are `"derived-era5-land-daily-statistics"` (default),
#'   `"derived-era5-single-levels-daily-statistics"`, or `"dwd-hyras-daily"`.
#' @param statistic Character string specifying the type of daily statistic
#'   to download. ERA5 only. Options are `"daily_mean"` (default),
#'   `"daily_maximum"`, and `"daily_minimum"`.
#' @param time_zone Character string specifying the time zone to use for
#'   daily aggregation. ERA5 only. Default is `"utc+00:00"`.
#' @param method Character string specifying the resampling method to use
#'   when aligning the downloaded raster with the input grid. SpatRaster
#'   input only. Options include `"bilinear"` (default), `"near"`, `"cubic"`,
#'   etc. See \code{\link[terra]{resample}} for details.
#' @param cache Logical indicating whether to cache downloaded files and
#'   restore them on repeated calls with the same parameters. Enabling
#'   caching significantly speeds up repeated calls. If `FALSE`, raw files
#'   are deleted after extraction. Default is `TRUE`.
#' @param path Character string specifying the directory for downloads and
#'   cache. If `NULL` (default), uses a temporary directory when
#'   `cache = FALSE` or the user data directory
#'   (\code{\link[tools]{R_user_dir}}) when `cache = TRUE`.
#' @param parallel Logical indicating whether to use parallel processing.
#'   See section **Parallel processing** for details. Default is `FALSE`.
#' @param chunk_size Integer specifying the number of observations per chunk
#'   when parallelising. Default is `50`.
#' @param verbose Logical specifying whether to show informative status
#'   messages. Default is `TRUE`.
#' @param ... Arguments passed to methods.
#'
#' @details
#' For ERA5 catalogues, this function interacts with the Copernicus Climate
#' Data Store (CDS) API to download daily reanalysis data. Requests are
#' submitted as individual per-day calls which is the approach recommended
#' by the CDS. For DWD catalogues, yearly HYRAS files are downloaded from
#' the DWD open data server, sliced into per-day cache files, and the raw
#' year files are deleted afterwards to save disk space.
#'
#' The input spatial points are first optionally buffered (if `buffer > 0`),
#' then processed to determine the geographic extent. The time dimension is
#' adjusted using `time_lag` and `time_span` (both in days). If a baseline
#' period is provided, baseline statistics are downloaded and appended as
#' additional columns.
#'
#' Output columns are consistently structured regardless of whether a baseline
#' is requested — calls without a baseline still produce all columns, with
#' `NA` in baseline-specific ones, making `rbind()` across specifications
#' straightforward. See \code{\link{link_monthly}} for the monthly equivalent.
#'
#' The following indicators are currently supported:
#'
#' `r rd_indicators("link_daily")`
#'
#' @note Users must have a CDS account and have their API key configured for
#'   `ecmwfr` when using ERA5 catalogues. DWD catalogues require no
#'   authentication.
#'
#' @section Parallel processing:
#' When `parallel = TRUE`, chunked parallel processing is performed via
#' \code{\link[future.apply]{future_lapply}}. Set a parallel plan before
#' calling this function, for example:
#' \code{future::plan(multisession, workers = 4)}.
#' If no plan is set, execution falls back to sequential processing through
#' the chunks, which is likely slower than `parallel = FALSE`.
#'
#' @return An object of the same class as `.data` with additional columns
#'   (sf input) or layers (SpatRaster input) containing the linked indicator
#'   values and associated metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' pts <- data.frame(
#'   lon  = c(13.4, 11.6),
#'   lat  = c(52.5, 51.3),
#'   date = c("2014-08-01", "2014-08-01")
#' )
#' pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
#'
#' # Simple extraction
#' result1 <- link_daily(pts_sf, indicator = "2m_temperature")
#'
#' # With buffer and baseline
#' result2 <- link_daily(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer    = 5000,
#'   baseline  = c("1980", "2010")
#' )
#'
#' # DWD HYRAS daily data
#' result3 <- link_daily(
#'   pts_sf,
#'   indicator = "air_temperature_mean",
#'   catalogue = "dwd-hyras-daily"
#' )
#'
#' # SpatRaster input
#' grid <- terra::rast(nrows = 10, ncols = 10,
#'                     xmin = 5, xmax = 16, ymin = 47, ymax = 55)
#' terra::time(grid) <- as.Date("2014-08-01")
#' link_daily(grid, indicator = "2m_temperature")}
link_daily <- function(.data,
                       indicator,
                       ...,
                       cache      = TRUE,
                       path       = NULL,
                       parallel   = FALSE,
                       chunk_size = 50,
                       verbose    = TRUE) {
  UseMethod("link_daily")
}


#' @rdname link_daily
#' @export
link_daily.sf <- function(.data,
                          indicator,
                          ...,
                          date_var       = "date",
                          time_span      = 0,
                          time_lag       = 0,
                          buffer         = 0,
                          baseline       = FALSE,
                          baseline_fun   = c("mean", "median", "min", "max", "sd",
                                             "p05", "p10", "p20", "p80", "p90", "p95"),
                          study_fun      = c("mean", "median", "min", "max", "sd",
                                             "p05", "p10", "p20", "p80", "p90", "p95"),
                          stat_wrangling = c("deviation", "sd_deviation",
                                             "count_above", "count_below"),
                          prefix         = NULL,
                          catalogue      = "derived-era5-land-daily-statistics",
                          statistic      = "daily_mean",
                          time_zone      = "utc+00:00",
                          cache          = TRUE,
                          path           = NULL,
                          parallel       = FALSE,
                          chunk_size     = 50,
                          verbose        = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "daily")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_statistic(statistic)
  .check_valid_time_zone(time_zone)
  .check_baseline(baseline)
  .check_parallel(parallel)
  .check_column(.data, date_var)
  .check_api_key_if_needed(catalogue)
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  stat_wrangling <- match.arg(stat_wrangling)

  if (stat_wrangling %in% c("count_above", "count_below") && time_span == 0) {
    cli::cli_abort(c(
      "{.val {stat_wrangling}} requires {.arg time_span} > 0.",
      "i" = "With {.arg time_span = 0} there is only one focal day to compare against the baseline."
    ))
  }

  baseline_resolved <- .resolve_baseline_fun(baseline_fun, arg = "baseline_fun")
  baseline_fun_name <- baseline_resolved$name
  baseline_fun      <- baseline_resolved$fun

  study_resolved <- .resolve_baseline_fun(study_fun, arg = "study_fun")
  study_fun_name <- study_resolved$name
  study_fun      <- study_resolved$fun

  if (chunk_size > nrow(.data) && isTRUE(parallel)) {
    info("Chunk size is higher than number of input rows. Disabling parallelization.")
    parallel <- FALSE
  }

  if (verbose) {
    cli::cli_rule(left = "Link with daily indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
      "Catalogue"         = "{.val {catalogue}}",
      "Time span"         = "{.val {time_span}}",
      "Time lag"          = "{.val {time_lag}}",
      "Baseline"          =
        "{.val {if (isFALSE(baseline)) 'none' else paste0(baseline[1], '-', baseline[2])}}",
      "Baseline function" = "{.val {baseline_fun_name}}",
      "Study function"    = "{.val {study_fun_name}}",
      "Stat wrangling"    = "{.val {stat_wrangling}}",
      "Prefix"            = "{.val {prefix %||% '(none)'}}",
      "Observations"      = "{.val {nrow(.data)}}",
      "Buffer"            = "{.val {buffer} m}",
      "Caching enabled"   = "{.val {cache}}",
      "Storage path"      = "{.path {path}}"
    ))
    cli::cli_text("")
  }

  crs_data <- terra::crs(.data)
  old_geom <- sf::st_geometry(.data)

  # split() below groups rows by date_var, which reorders them (sorted by
  # unique date values) relative to the input. .row_id records the original
  # position so the output can be restored to input order before old_geom
  # (which is still in input order) is written back -- otherwise geometries
  # get silently mismatched to the wrong rows whenever .data isn't already
  # sorted by date_var. Carried through unchanged by .transform_time() (only
  # adds columns) and rbind() (preserves all columns).
  .data$.row_id <- seq_len(nrow(.data))

  prepared <- sf::st_transform(.data, 4326)
  if (buffer > 0) { prepared <- sf::st_buffer(prepared, buffer) }

  splits   <- split(prepared, prepared[[date_var]])
  n_splits <- length(splits)
  result   <- vector("list", n_splits)

  if (verbose) {
    cli::cli_alert_info("Observations clustered across {n_splits} unique date{?s}.")
  }

  # -------------------------------------------------------------------------
  # Phase 1: Submit all requests upfront as a single batch
  # -------------------------------------------------------------------------

  global_extent <- .get_extent(prepared)

  all_spans <- lapply(splits, function(splitted) {
    p <- .transform_time(
      splitted,
      date_var  = date_var,
      time_span = time_span,
      time_lag  = time_lag
    )
    sort(unique(as_date(unlist(p$time_span_seq))))
  })
  all_obs_span <- sort(unique(do.call(c, all_spans)))

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} day{?s})..."
    )
  }

  obs_path <- .request_climate_daily(
    indicator = indicator,
    catalogue = catalogue,
    extent    = global_extent,
    years     = format(all_obs_span, "%Y"),
    months    = format(all_obs_span, "%m"),
    days      = format(all_obs_span, "%d"),
    prefix    = "observation",
    cache     = cache,
    path      = path,
    statistic = statistic,
    time_zone = time_zone,
    verbose   = verbose
  )

  obs_raster <- .load_climate_raster(obs_path, all_obs_span, daily = TRUE)

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    all_baseline_span <- .compute_baseline_span(baseline, all_obs_span, daily = TRUE)

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} day{?s})..."
      )
    }

    baseline_path   <- .request_climate_daily(
      indicator = indicator,
      catalogue = catalogue,
      extent    = global_extent,
      years     = format(all_baseline_span, "%Y"),
      months    = format(all_baseline_span, "%m"),
      days      = format(all_baseline_span, "%d"),
      prefix    = "baseline",
      cache     = cache,
      path      = path,
      statistic = statistic,
      time_zone = time_zone,
      verbose   = verbose
    )
    baseline_raster <- .load_climate_raster(baseline_path, all_baseline_span, daily = TRUE)
  }

  # -------------------------------------------------------------------------
  # Phase 2: Extract — rasters already loaded
  # -------------------------------------------------------------------------

  t_start <- proc.time()[["elapsed"]]

  if (verbose) {
    pb <- cli::cli_progress_bar(
      name   = "Processing dates",
      total  = n_splits,
      format = paste0(
        "{cli::pb_spin} {cli::pb_name} [{cli::pb_current}/{cli::pb_total}]",
        " \u00b7 {.val {current_date}} {cli::pb_bar} {cli::pb_percent}"
      )
    )
    current_date <- splits[[1]][[date_var]][1]
  }

  for (i in seq_along(splits)) {
    if (verbose) {
      current_date <- splits[[i]][[date_var]][1]
      cli::cli_progress_update(id = pb)
    }

    splitted <- splits[[i]]

    result[[i]] <- {
      prepared <- .transform_time(
        splitted,
        date_var  = date_var,
        time_span = time_span,
        time_lag  = time_lag
      )

      prepared <- .align_crs_vector(prepared, obs_raster)

      raster_values <- .toi_extract(
        prepared,
        obs_raster,
        obs_path,
        time_span      = time_span,
        parallel       = parallel,
        chunk_size     = chunk_size,
        baseline_fun   = baseline_fun,
        stat_wrangling = stat_wrangling
      )

      prepared[[.col("study",    prefix)]] <-
        .extract_study_values(raster_values, study_fun)
      prepared[[.col("baseline", prefix)]] <- NA_real_
      prepared[[.col("result",   prefix)]] <- NA_real_
      prepared$.linked <- prepared[[.col("study", prefix)]]

      if (!isFALSE(baseline)) {
        prepared <- .add_baseline(
          prepared,
          baseline          = baseline,
          baseline_fun      = baseline_fun,
          baseline_fun_name = baseline_fun_name,
          indicator         = indicator,
          stat_wrangling    = stat_wrangling,
          focal_values      = raster_values,
          prefix            = prefix,
          obs_raster        = obs_raster,
          baseline_raster   = baseline_raster,
          cache             = cache,
          path              = path,
          parallel          = parallel,
          chunk_size        = chunk_size,
          verbose           = verbose
        )
      }

      prepared$.linked <- NULL

      prepared <- .write_metadata_sf(
        prepared,
        prefix            = prefix,
        indicator         = indicator,
        catalogue         = catalogue,
        baseline          = baseline,
        stat_wrangling    = stat_wrangling,
        study_fun_name    = study_fun_name,
        baseline_fun_name = baseline_fun_name,
        time_span         = time_span,
        time_lag          = time_lag,
        buffer            = buffer,
        time_unit         = "days"
      )

      if (!cache) unlink(obs_path)
      prepared
    }
  }

  if (verbose) {
    cli::cli_progress_done(id = pb)
    elapsed <- round(proc.time()[["elapsed"]] - t_start, 1)
    cli::cli_alert_success(
      "Done. {nrow(.data)} observation{?s} across {n_splits} date{?s} linked in {elapsed}s."
    )
  }

  prepared <- do.call(rbind, result)

  # Restore original input order (split() above grouped rows by date_var,
  # sorted by unique date values) before old_geom -- which is still in
  # original input order -- gets written back below.
  prepared <- prepared[order(prepared$.row_id), ]

  prepared <- sf::st_transform(prepared, crs = crs_data)
  prepared[c("link_date", "link_date_end", "time_span_seq", ".row_id")] <- NULL
  prepared <- move_to_back(prepared, attr(prepared, "sf_column"))
  sf::st_geometry(prepared) <- old_geom
  as_sf_tibble(prepared)
}


#' @rdname link_daily
#' @export
link_daily.SpatRaster <- function(.data,
                                  indicator,
                                  ...,
                                  time_span      = 0,
                                  time_lag       = 0,
                                  baseline       = FALSE,
                                  baseline_fun   = c("mean", "median", "min", "max", "sd",
                                                     "p05", "p10", "p20", "p80", "p90", "p95"),
                                  study_fun      = c("mean", "median", "min", "max", "sd",
                                                     "p05", "p10", "p20", "p80", "p90", "p95"),
                                  stat_wrangling = c("deviation", "sd_deviation",
                                                     "count_above", "count_below"),
                                  prefix         = NULL,
                                  method         = "bilinear",
                                  catalogue      = "derived-era5-land-daily-statistics",
                                  statistic      = "daily_mean",
                                  time_zone      = "utc+00:00",
                                  cache          = TRUE,
                                  path           = NULL,
                                  parallel       = FALSE,
                                  chunk_size     = 5000,
                                  verbose        = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "daily")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_statistic(statistic)
  .check_valid_time_zone(time_zone)
  .check_baseline(baseline)
  .check_parallel(parallel)
  .check_terra_time(.data)
  .check_api_key_if_needed(catalogue)
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  stat_wrangling <- match.arg(stat_wrangling)

  if (stat_wrangling %in% c("count_above", "count_below") && time_span == 0) {
    cli::cli_abort(c(
      "{.val {stat_wrangling}} requires {.arg time_span} > 0.",
      "i" = "With {.arg time_span = 0} there is only one focal day to compare against the baseline."
    ))
  }

  baseline_resolved <- .resolve_baseline_fun(baseline_fun, arg = "baseline_fun")
  baseline_fun_name <- baseline_resolved$name
  baseline_fun      <- baseline_resolved$fun

  study_resolved <- .resolve_baseline_fun(study_fun, arg = "study_fun")
  study_fun_name <- study_resolved$name
  study_fun      <- study_resolved$fun

  # Store original CRS and project to WGS84
  crs_data <- terra::crs(.data)
  prepared <- terra::project(.data, "EPSG:4326")

  temporals    <- .transform_time(prepared, time_span = time_span, time_lag = time_lag)
  global_extent <- .get_extent(prepared)
  all_obs_span  <- sort(unique(do.call(c, lapply(temporals$time_span_seq, as_date))))

  if (verbose) {
    cli::cli_rule(left = "Link with daily indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
      "Catalogue"         = "{.val {catalogue}}",
      "Time span"         = "{.val {time_span}}",
      "Time lag"          = "{.val {time_lag}}",
      "Baseline"          =
        "{.val {if (isFALSE(baseline)) 'none' else paste0(baseline[1], '-', baseline[2])}}",
      "Baseline function" = "{.val {baseline_fun_name}}",
      "Study function"    = "{.val {study_fun_name}}",
      "Stat wrangling"    = "{.val {stat_wrangling}}",
      "Prefix"            = "{.val {prefix %||% '(none)'}}",
      "Caching enabled"   = "{.val {cache}}",
      "Storage path"      = "{.path {path}}"
    ))
    cli::cli_text("")
  }

  # -------------------------------------------------------------------------
  # Phase 1: Submit all requests upfront as a single batch
  # -------------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} day{?s})..."
    )
  }

  obs_path   <- .request_climate_daily(
    indicator = indicator,
    catalogue = catalogue,
    extent    = global_extent,
    years     = format(all_obs_span, "%Y"),
    months    = format(all_obs_span, "%m"),
    days      = format(all_obs_span, "%d"),
    prefix    = "observation",
    cache     = cache,
    path      = path,
    statistic = statistic,
    time_zone = time_zone,
    verbose   = verbose
  )
  obs_raster <- .load_climate_raster(obs_path, all_obs_span, daily = TRUE)
  obs_raster <- .align_crs_raster(.data, obs_raster)

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    all_baseline_span <- .compute_baseline_span(baseline, all_obs_span, daily = TRUE)

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} day{?s})..."
      )
    }

    baseline_path   <- .request_climate_daily(
      indicator = indicator,
      catalogue = catalogue,
      extent    = global_extent,
      years     = format(all_baseline_span, "%Y"),
      months    = format(all_baseline_span, "%m"),
      days      = format(all_baseline_span, "%d"),
      prefix    = "baseline",
      cache     = cache,
      path      = path,
      statistic = statistic,
      time_zone = time_zone,
      verbose   = verbose
    )
    baseline_raster <- .load_climate_raster(baseline_path, all_baseline_span, daily = TRUE)
    baseline_raster <- .align_crs_raster(.data, baseline_raster)
  }

  # -------------------------------------------------------------------------
  # Phase 2: Extract
  # -------------------------------------------------------------------------

  info(
    "Extracting values from raster",
    msg_done   = "Extracted values from raster.",
    msg_failed = "Failed to extract values from raster.",
    level      = "step"
  )

  extracted <- .toi_extract_grid(
    .data,
    obs_raster,
    temporals,
    agg            = time_span > 0,
    stat_wrangling = stat_wrangling,
    parallel       = parallel,
    chunk_size     = chunk_size,
    method         = method
  )

  # Collapse to single layer if count mode returned a stack
  study_layer <- if (terra::nlyr(extracted) > 1) {
    terra::app(extracted, study_fun)
  } else {
    extracted
  }
  names(study_layer) <- .col("study", prefix)
  .data <- c(.data, study_layer, warn = FALSE)

  # Placeholder layers so column order is consistent before .add_baseline
  placeholder                <- study_layer
  terra::values(placeholder) <- NA_real_
  baseline_ph                <- placeholder
  result_ph                  <- placeholder
  names(baseline_ph)         <- .col("baseline", prefix)
  names(result_ph)           <- .col("result",   prefix)
  .data <- c(.data, baseline_ph, warn = FALSE)
  .data <- c(.data, result_ph,   warn = FALSE)

  .data[[".linked"]] <- study_layer

  if (!isFALSE(baseline)) {
    .data <- .add_baseline(
      .data,
      baseline          = baseline,
      baseline_fun      = baseline_fun,
      baseline_fun_name = baseline_fun_name,
      indicator         = indicator,
      stat_wrangling    = stat_wrangling,
      prefix            = prefix,
      obs_raster        = obs_raster,
      baseline_raster   = baseline_raster,
      cache             = cache,
      path              = path,
      parallel          = parallel,
      chunk_size        = chunk_size,
      verbose           = verbose
    )
  }

  .data[[".linked"]] <- NULL

  terra::metags(.data) <- c(
    indicator      = indicator,
    unit           = .indicator_units[[indicator]] %||% NA_character_,
    resolution     = .catalogue_resolution[[catalogue]] %||% NA_character_,
    time_unit      = "days",
    result_unit    = if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator),
    study_fun      = study_fun_name,
    baseline_fun   = if (isFALSE(baseline)) NA_character_ else baseline_fun_name,
    baseline_years = if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2]),
    time_span      = as.character(time_span),
    time_lag       = as.character(time_lag),
    prefix         = prefix %||% "",
    source         = .catalogue_citation(catalogue, indicator)
  )

  if (!cache) unlink(obs_path)
  .data
}


#' @rdname link_daily
#' @export
link_daily.stars <- function(.data, indicator, ...) {
  .data <- terra::rast(.data)
  stars::st_as_stars(link_daily(.data, indicator, ...))
}


#' @rdname link_daily
#' @export
link_daily.SpatVector <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  terra::vect(link_daily(.data, indicator, ...))
}


#' @rdname link_daily
#' @export
link_daily.Spatial <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  sf::as_Spatial(link_daily(.data, indicator, ...))
}
