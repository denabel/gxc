#' Link with monthly indicators
#'
#' @description Augments spatio-temporal data with monthly indicators from the
#' Copernicus earth observation database (ERA5) or the German Weather Service
#' (DWD).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download monthly statistics from the data source}
#'  \item{Link raster statistics back to input}
#'  \item{Optionally, add comparative statistics based on a baseline period}
#' }
#'
#' This function interfaces the monthly means of ERA5 and DWD indicators. For
#' daily statistics see \code{\link{link_daily}}.
#'
#' @param catalogue Character string specifying which catalogue to use.
#'   Options are `"reanalysis-era5-land-monthly-means"` (default),
#'   `"reanalysis-era5-single-levels-monthly-means"`, or `"dwd-monthly"`.
#' @param by_hour Logical or character specifying whether to use
#'   hourly-based monthly averages. ERA5 only. If `FALSE` (default), monthly
#'   averages are derived from the entire day. If a character string giving a
#'   full hour (e.g. `"03:00"`), only values from that hour of the day are
#'   included. Not applicable for DWD catalogues.
#' @param months Optional integer vector specifying explicit months to use as
#'   the study period (e.g. `c(3, 4, 5)` for spring). If the input date falls
#'   within one of the specified months, the window is automatically shifted
#'   one year back to avoid using incomplete data. Cannot be combined with
#'   `time_span`.
#' @inherit link_daily
#'
#' @details
#' For ERA5 catalogues, this function interacts with the Copernicus Climate
#' Data Store (CDS) API to download monthly reanalysis data. Requests are
#' submitted as individual per-month calls. For DWD catalogues, monthly
#' gridded data is downloaded directly from the DWD open data server as
#' compressed ASCII grid files and cached as `.tif` files.
#'
#' The input spatial points are first optionally buffered (if `buffer > 0`),
#' then processed to determine the geographic extent. The time dimension is
#' adjusted using `time_lag` and `time_span` (both in months). If `months`
#' is specified, an explicit seasonal window is used instead of a rolling
#' time span. If a baseline period is provided, baseline statistics are
#' downloaded and appended as additional columns.
#'
#' Output columns are consistently structured regardless of whether a baseline
#' is requested — calls without a baseline still produce all columns, with
#' `NA` in baseline-specific ones, making `rbind()` across specifications
#' straightforward. See \code{\link{link_daily}} for the daily equivalent.
#'
#' The following indicators are currently supported:
#'
#' `r rd_indicators("link_monthly")`
#'
#' @note Users must have a CDS account and have their API key configured for
#'   `ecmwfr` when using ERA5 catalogues. DWD catalogues require no
#'   authentication.
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
#' result1 <- link_monthly(pts_sf, indicator = "2m_temperature")
#'
#' # With buffer and baseline
#' result2 <- link_monthly(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer    = 5000,
#'   baseline  = c("1980", "2010")
#' )
#'
#' # Explicit spring months with baseline deviation
#' result3 <- link_monthly(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   months    = c(3, 4, 5),
#'   baseline  = c("1980", "2010")
#' )
#'
#' # DWD monthly data
#' result4 <- link_monthly(
#'   pts_sf,
#'   indicator = "air_temperature_mean",
#'   catalogue = "dwd-monthly"
#' )}
link_monthly <- function(.data,
                         indicator,
                         ...,
                         cache      = TRUE,
                         path       = NULL,
                         parallel   = FALSE,
                         chunk_size = 50,
                         verbose    = TRUE) {
  UseMethod("link_monthly")
}


#' @rdname link_monthly
#' @export
link_monthly.sf <- function(.data,
                            indicator,
                            ...,
                            date_var       = "date",
                            time_span      = 0,
                            time_lag       = 0,
                            months         = NULL,
                            buffer         = 0,
                            baseline       = FALSE,
                            baseline_fun   = c("mean", "median", "min", "max", "sd",
                                               "p05", "p10", "p20", "p80", "p90", "p95"),
                            study_fun      = c("mean", "median", "min", "max", "sd",
                                               "p05", "p10", "p20", "p80", "p90", "p95"),
                            stat_wrangling = c("deviation", "sd_deviation",
                                               "count_above", "count_below"),
                            prefix         = NULL,
                            catalogue      = "reanalysis-era5-land-monthly-means",
                            by_hour        = FALSE,
                            cache          = TRUE,
                            path           = NULL,
                            parallel       = FALSE,
                            chunk_size     = 50,
                            verbose        = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "monthly")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_by_hour(by_hour)
  .check_baseline(baseline)
  .check_parallel(parallel)
  .check_column(.data, date_var)
  .check_api_key_if_needed(catalogue)
  .check_months_time_span(months, time_span)
  .check_valid_months(months)
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  stat_wrangling <- match.arg(stat_wrangling)

  if (stat_wrangling %in% c("count_above", "count_below") &&
      time_span == 0 && is.null(months)) {
    cli::cli_abort(c(
      "{.val {stat_wrangling}} requires {.arg time_span} > 0 or {.arg months} to be specified.",
      "i" = "With a single month there is only one value to compare against the baseline."
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

  if (isFALSE(by_hour)) {
    product_type <- "monthly_averaged_reanalysis"
    request_time <- "00:00"
  } else {
    product_type <- "monthly_averaged_reanalysis_by_hour_of_day"
    request_time <- by_hour
  }

  if (verbose) {
    cli::cli_rule(left = "Link with monthly indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
      "Catalogue"         = "{.val {catalogue}}",
      "Time span"         = "{.val {if (is.null(months)) time_span else 'via months'}}",
      "Months"            = "{.val {if (is.null(months)) '(none)' else paste(months, collapse = ', ')}}",
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
  prepared <- sf::st_transform(.data, 4326)
  prepared <- sf::st_buffer(prepared, buffer)

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
      time_lag  = time_lag,
      months    = months,
      by        = "1 month"
    )
    sort(unique(as_date(unlist(p$time_span_seq))))
  })
  all_obs_span <- sort(unique(as.Date(format(do.call(c, all_spans), "%Y-%m-01"))))

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} month{?s})..."
    )
  }

  obs_path <- .request_climate_monthly(
    indicator    = indicator,
    catalogue    = catalogue,
    extent       = global_extent,
    years        = format(all_obs_span, "%Y"),
    months       = format(all_obs_span, "%m"),
    prefix       = "observation",
    cache        = cache,
    path         = path,
    product_type = product_type,
    request_time = request_time,
    verbose      = verbose
  )

  obs_raster <- .load_climate_raster(obs_path, all_obs_span, daily = FALSE)

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    all_baseline_span <- .compute_baseline_span(baseline, all_obs_span, daily = FALSE)

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} month{?s})..."
      )
    }

    baseline_path   <- .request_climate_monthly(
      indicator    = indicator,
      catalogue    = catalogue,
      extent       = global_extent,
      years        = format(all_baseline_span, "%Y"),
      months       = format(all_baseline_span, "%m"),
      prefix       = "baseline",
      cache        = cache,
      path         = path,
      product_type = product_type,
      request_time = request_time,
      verbose      = verbose
    )
    baseline_raster <- .load_climate_raster(baseline_path, all_baseline_span, daily = FALSE)
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
        time_lag  = time_lag,
        months    = months,
        by        = "1 month"
      )

      prepared <- .align_crs_vector(prepared, obs_raster)

      raster_values <- .toi_extract(
        prepared,
        obs_raster,
        obs_path,
        time_span      = if (!is.null(months)) 1L else time_span,
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
        time_unit         = "months",
        months            = months
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
  prepared <- sf::st_transform(prepared, crs = crs_data)
  prepared[c("link_date", "link_date_end", "time_span_seq")] <- NULL
  prepared <- move_to_back(prepared, attr(prepared, "sf_column"))
  sf::st_geometry(prepared) <- old_geom
  as_sf_tibble(prepared)
}


#' @rdname link_monthly
#' @export
link_monthly.SpatRaster <- function(.data,
                                    indicator,
                                    ...,
                                    time_span      = 0,
                                    time_lag       = 0,
                                    months         = NULL,
                                    baseline       = FALSE,
                                    baseline_fun   = c("mean", "median", "min", "max", "sd",
                                                       "p05", "p10", "p20", "p80", "p90", "p95"),
                                    study_fun      = c("mean", "median", "min", "max", "sd",
                                                       "p05", "p10", "p20", "p80", "p90", "p95"),
                                    stat_wrangling = c("deviation", "sd_deviation",
                                                       "count_above", "count_below"),
                                    prefix         = NULL,
                                    method         = "bilinear",
                                    catalogue      = "reanalysis-era5-land-monthly-means",
                                    by_hour        = FALSE,
                                    cache          = TRUE,
                                    path           = NULL,
                                    parallel       = FALSE,
                                    chunk_size     = 50,
                                    verbose        = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "monthly")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_by_hour(by_hour)
  .check_baseline(baseline)
  .check_parallel(parallel)
  .check_terra_time(.data)
  .check_api_key_if_needed(catalogue)
  .check_months_time_span(months, time_span)
  .check_valid_months(months)
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  stat_wrangling <- match.arg(stat_wrangling)

  if (stat_wrangling %in% c("count_above", "count_below") &&
      time_span == 0 && is.null(months)) {
    cli::cli_abort(c(
      "{.val {stat_wrangling}} requires {.arg time_span} > 0 or {.arg months} to be specified.",
      "i" = "With a single month there is only one value to compare against the baseline."
    ))
  }

  baseline_resolved <- .resolve_baseline_fun(baseline_fun, arg = "baseline_fun")
  baseline_fun_name <- baseline_resolved$name
  baseline_fun      <- baseline_resolved$fun

  study_resolved <- .resolve_baseline_fun(study_fun, arg = "study_fun")
  study_fun_name <- study_resolved$name
  study_fun      <- study_resolved$fun

  if (isFALSE(by_hour)) {
    product_type <- "monthly_averaged_reanalysis"
    request_time <- "00:00"
  } else {
    product_type <- "monthly_averaged_reanalysis_by_hour_of_day"
    request_time <- by_hour
  }

  # Store original CRS and project to WGS84
  crs_data <- terra::crs(.data)
  prepared <- terra::project(.data, "EPSG:4326")

  temporals     <- .transform_time(prepared, time_span = time_span, time_lag = time_lag,
                                   months = months, by = "1 month")
  global_extent <- .get_extent(prepared)
  all_obs_span  <- sort(unique(as.Date(format(
    do.call(c, lapply(temporals$time_span_seq, as_date)), "%Y-%m-01"
  ))))

  if (verbose) {
    cli::cli_rule(left = "Link with monthly indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
      "Catalogue"         = "{.val {catalogue}}",
      "Time span"         = "{.val {if (is.null(months)) time_span else 'via months'}}",
      "Months"            = "{.val {if (is.null(months)) '(none)' else paste(months, collapse = ', ')}}",
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
      "Submitting observation requests ({length(all_obs_span)} month{?s})..."
    )
  }

  obs_path   <- .request_climate_monthly(
    indicator    = indicator,
    catalogue    = catalogue,
    extent       = global_extent,
    years        = format(all_obs_span, "%Y"),
    months       = format(all_obs_span, "%m"),
    prefix       = "observation",
    cache        = cache,
    path         = path,
    product_type = product_type,
    request_time = request_time,
    verbose      = verbose
  )
  obs_raster <- .load_climate_raster(obs_path, all_obs_span, daily = FALSE)
  obs_raster <- .align_crs_raster(.data, obs_raster)

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    all_baseline_span <- .compute_baseline_span(baseline, all_obs_span, daily = FALSE)

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} month{?s})..."
      )
    }

    baseline_path   <- .request_climate_monthly(
      indicator    = indicator,
      catalogue    = catalogue,
      extent       = global_extent,
      years        = format(all_baseline_span, "%Y"),
      months       = format(all_baseline_span, "%m"),
      prefix       = "baseline",
      cache        = cache,
      path         = path,
      product_type = product_type,
      request_time = request_time,
      verbose      = verbose
    )
    baseline_raster <- .load_climate_raster(baseline_path, all_baseline_span, daily = FALSE)
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
    agg            = time_span > 0 || !is.null(months),
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
    time_unit      = "months",
    result_unit    = if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator),
    study_fun      = study_fun_name,
    baseline_fun   = if (isFALSE(baseline)) NA_character_ else baseline_fun_name,
    baseline_years = if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2]),
    time_span      = as.character(time_span),
    months         = if (is.null(months)) "" else paste(months, collapse = ","),
    time_lag       = as.character(time_lag),
    prefix         = prefix %||% "",
    source         = .catalogue_citation(catalogue, indicator)
  )

  if (!cache) unlink(obs_path)
  .data
}


#' @rdname link_monthly
#' @export
link_monthly.stars <- function(.data, indicator, ...) {
  .data <- terra::rast(.data)
  stars::st_as_stars(link_monthly(.data, indicator, ...))
}


#' @rdname link_monthly
#' @export
link_monthly.SpatVector <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  terra::vect(link_monthly(.data, indicator, ...))
}


#' @rdname link_monthly
#' @export
link_monthly.Spatial <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  sf::as_Spatial(link_monthly(.data, indicator, ...))
}
