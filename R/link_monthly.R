#' Link with ERA5 monthly indicators
#'
#' @description Augments spatio-temporal data with indicators from the
#' Copernicus earth observation database (ERA5).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download monthly statistics from Copernicus database}
#'  \item{Link raster statistics back to input}
#'  \item{Optionally, add comparative statistics based on a baseline period}
#' }
#'
#' This function interfaces the monthly means of ERA5 indicators. For daily
#' statistics see \code{\link{link_daily}}.
#'
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"reanalysis-era5-land-monthly-means"`
#'   or `"reanalysis-era5-single-levels-monthly-means"`. The first provides
#'   higher spatial resolution at 0.1x0.1 degrees but is only available from
#'   1950 onwards. If you need data before 1950 or if you are working with large
#'   spatial extents where finer resolution is not required, you can switch to
#'   the latter.
#' @param by_hour Logical or character. If `FALSE` (default), the monthly
#'   averaged values are derived from the entire day
#'   (`"monthly_averaged_reanalysis"`). If a character string specifying an
#'   hour (e.g., `"03:00"`), then the dataset
#'   `"monthly_averaged_reanalysis_by_hour_of_day"` is used, and only values
#'   from that hour of the day are included.
#' @param months Optional integer vector specifying explicit months to use as
#'   the study period (e.g. `c(3, 4, 5)` for spring). If the input date falls
#'   within one of the specified months, the window is shifted one year back.
#'   Cannot be combined with `time_span`.
#' @inherit link_daily
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to
#' download ERA5 monthly reanalysis data for a specified climate indicator and
#' time period. The input spatial points (an sf object) are first optionally
#' buffered (using the `buffer` argument) to expand the extraction area. The
#' function then determines the geographic extent from the (possibly buffered)
#' points and adjusts the time dimension based on the specified `date_var`,
#' `time_lag`, and `time_span` (all in months). Monthly time sequences are
#' constructed assuming that dates correspond to the first day of each month.
#' The function downloads the corresponding monthly data (or hourly-based
#' monthly averages if `by_hour` is specified) and extracts these values for
#' each point—using a direct cell match when `buffer = 0` or aggregating over
#' the buffer area when `buffer > 0`. If a baseline period is provided (e.g.,
#' `baseline = c("1980", "2010")`), baseline monthly statistics are downloaded
#' for the specified period and appended as an additional attribute.
#' Optionally, deviations between the focal and baseline values may be
#' computed.
#'
#' The following indicators are currently supported:
#'
#' `r rd_indicators("link_monthly")`
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create sample point data (sf object)
#' pts <- data.frame(
#'   lon = c(13.4, 11.6, 9.9),
#'   lat = c(52.5, 51.3, 50.1),
#'   date = c("2014-08-01", "2014-08-01", "2014-08-01")
#' )
#' pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
#'
#' # Example 1: Direct extraction (buffer = 0)
#' result1 <- link_monthly(pts_sf, indicator = "2m_temperature")
#'
#' # Example 2: Aggregated extraction with a 5 km buffer and a baseline period
#' result2 <- link_monthly(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer = 5,
#'   baseline = c("1980", "2010")
#' )
#'
#' # Example 3: Explicit spring months with baseline deviation
#' result3 <- link_monthly(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   months = c(3, 4, 5),
#'   baseline = c("1980", "2010")
#' )
#'
#' # View the results:
#' head(result1)
#' head(result2)}
#'
#' @export
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
  .check_api_key("ecmwfr")
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

  crs_data <- terra::crs(.data)
  old_geom <- sf::st_geometry(.data)
  prepared <- sf::st_transform(.data, 4326)
  prepared <- sf::st_buffer(prepared, buffer)

  splits   <- split(prepared, prepared[[date_var]])
  n_splits <- length(splits)
  result   <- vector("list", n_splits)

  if (verbose) {
    cli::cli_rule(left = "Link with ERA5 monthly indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
      "Time span"         = "{.val {if (is.null(months)) time_span else 'via months'}}",
      "Months"            = "{.val {if (is.null(months)) '(none)' else paste(months, collapse = ', ')}}",
      "Time lag"          = "{.val {time_lag}}",
      "Baseline"          =
        "{.val {if (isFALSE(baseline)) 'none' else paste0(baseline[1], '-', baseline[2])}}",
      "Baseline function" = "{.val {baseline_fun_name}}",
      "Study function"    = "{.val {study_fun_name}}",
      "Stat wrangling"    = "{.val {stat_wrangling}}",
      "Prefix"            = "{.val {prefix %||% '(none)'}}",
      "Observations"      =
        "{.val {nrow(.data)} clustered across {n_splits} unique date{?s}}",
      "Buffer"            = "{.val {buffer} m}",
      "Caching enabled"   = "{.val {cache}}",
      "Storage path"      = "{.path {path}}"
    ))
    cli::cli_text("")
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
  all_obs_span <-
    sort(unique(as.Date(format(do.call(c, all_spans), "%Y-%m-01"))))

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} month{?s})..."
    )
  }

  obs_path <- .request_era5_monthly(
    indicator,
    catalogue    = catalogue,
    extent       = global_extent,
    years        = format(all_obs_span, "%Y"),
    months       = format(all_obs_span, "%m"),
    cache        = cache,
    path         = path,
    prefix       = "observation",
    product_type = product_type,
    request_time = request_time,
    verbose      = verbose
  )

  obs_raster <- terra::rast(obs_path)
  if (!inherits(terra::time(obs_raster), "POSIXt")) {
    obs_raster <- raster_timestamp(
      obs_raster,
      days   = "01",
      months = format(all_obs_span, "%m"),
      years  = format(all_obs_span, "%Y"),
      span   = all_obs_span
    )
  }

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    baseline_years <- format(
      make_dates(seq(baseline[1], baseline[2]), months = 1, days = 1),
      "%Y"
    )
    all_baseline_span <- sort(unique(do.call(c, lapply(baseline_years, function(y) {
      as.Date(paste(y, format(all_obs_span, "%m"), "01", sep = "-"))
    }))))

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} month{?s})..."
      )
    }

    baseline_path <- .request_era5_monthly(
      indicator,
      catalogue    = catalogue,
      extent       = global_extent,
      years        = format(all_baseline_span, "%Y"),
      months       = format(all_baseline_span, "%m"),
      cache        = cache,
      path         = path,
      prefix       = "baseline",
      product_type = product_type,
      request_time = request_time,
      verbose      = verbose
    )

    baseline_raster <- terra::rast(baseline_path)
    if (!inherits(terra::time(baseline_raster), "POSIXt")) {
      baseline_raster <- raster_timestamp(
        baseline_raster,
        days   = "01",
        months = format(all_baseline_span, "%m"),
        years  = format(all_baseline_span, "%Y"),
        span   = all_baseline_span
      )
    }
  }

  # -------------------------------------------------------------------------
  # Phase 2: Extract — rasters already loaded
  # -------------------------------------------------------------------------

  for (i in seq_along(splits)) {
    if (verbose) {
      if (i > 1) cli::cli_text("")
      cli::cli_rule(left = "Date {i}/{n_splits} ({splits[[i]][[date_var]][1]})")
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

      info(
        "Extracting values from raster",
        msg_done   = "Extracted values from raster.",
        msg_failed = "Failed to extract values from raster.",
        level      = "step"
      )

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

      # Write primary result columns in order
      prepared[[.col("study",    prefix)]] <- sapply(raster_values, function(x) {
        if (is.numeric(x) && length(x) > 1) study_fun(x)
        else if (is.data.frame(x)) x[1, 1]
        else as.numeric(x)
      })
      prepared[[.col("baseline", prefix)]] <- NA_real_
      prepared[[.col("result",   prefix)]] <- NA_real_

      # Keep .linked for internal baseline comparison
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

      # Remove internal .linked column
      prepared$.linked <- NULL

      # Write metadata columns
      prepared[[.col("indicator",      prefix)]] <- indicator
      prepared[[.col("unit",           prefix)]] <-
        if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator)
      prepared[[.col("study_fun",      prefix)]] <- study_fun_name
      prepared[[.col("baseline_fun",   prefix)]] <-
        if (isFALSE(baseline)) NA_character_ else baseline_fun_name
      prepared[[.col("baseline_years", prefix)]] <-
        if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2])
      prepared[[.col("time_span",      prefix)]] <- time_span
      prepared[[.col("months",         prefix)]] <-
        if (is.null(months)) NA_character_ else paste(months, collapse = ",")
      prepared[[.col("time_lag",       prefix)]] <- time_lag
      prepared[[.col("buffer",         prefix)]] <- buffer

      if (!cache) unlink(obs_path)

      prepared
    }
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
  .check_api_key("ecmwfr")
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

  # Store original CRS and project to WGS84 — analog to link_daily.SpatRaster
  crs_data <- terra::crs(.data)
  prepared <- terra::project(.data, "EPSG:4326")

  temporals <- .transform_time(
    prepared,
    time_span = time_span,
    time_lag  = time_lag,
    months    = months,
    by        = "1 month"
  )

  global_extent <- .get_extent(prepared)

  if (verbose) {
    cli::cli_rule(left = "Link with ERA5 monthly indicators")
    cli::cli_dl(c(
      "Indicator"         = "{.val {indicator}}",
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

  all_obs_span <- sort(unique(as.Date(format(
    do.call(c, lapply(temporals$time_span_seq, as_date)),
    "%Y-%m-01"
  ))))

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} month{?s})..."
    )
  }

  obs_path <- .request_era5_monthly(
    indicator,
    catalogue    = catalogue,
    extent       = global_extent,
    years        = format(all_obs_span, "%Y"),
    months       = format(all_obs_span, "%m"),
    cache        = cache,
    path         = path,
    prefix       = "observation",
    product_type = product_type,
    request_time = request_time,
    verbose      = verbose
  )

  obs_raster <- terra::rast(obs_path)
  if (!inherits(terra::time(obs_raster), "POSIXt")) {
    obs_raster <- raster_timestamp(
      obs_raster,
      days   = "01",
      months = format(all_obs_span, "%m"),
      years  = format(all_obs_span, "%Y"),
      span   = all_obs_span
    )
  }
  obs_raster <- .align_crs_raster(.data, obs_raster)

  baseline_raster <- NULL
  if (!isFALSE(baseline)) {
    baseline_years <- format(
      make_dates(seq(baseline[1], baseline[2]), months = 1, days = 1),
      "%Y"
    )
    all_baseline_span <- sort(unique(do.call(c, lapply(baseline_years, function(y) {
      as.Date(paste(y, format(all_obs_span, "%m"), "01", sep = "-"))
    }))))

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} month{?s})..."
      )
    }

    baseline_path <- .request_era5_monthly(
      indicator,
      catalogue    = catalogue,
      extent       = global_extent,
      years        = format(all_baseline_span, "%Y"),
      months       = format(all_baseline_span, "%m"),
      cache        = cache,
      path         = path,
      prefix       = "baseline",
      product_type = product_type,
      request_time = request_time,
      verbose      = verbose
    )

    baseline_raster <- terra::rast(baseline_path)
    if (!inherits(terra::time(baseline_raster), "POSIXt")) {
      baseline_raster <- raster_timestamp(
        baseline_raster,
        days   = "01",
        months = format(all_baseline_span, "%m"),
        years  = format(all_baseline_span, "%Y"),
        span   = all_baseline_span
      )
    }
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

  # Collapse to single layer for study column if count mode returned stack
  study_layer <- if (terra::nlyr(extracted) > 1) {
    terra::app(extracted, study_fun)
  } else {
    extracted
  }
  names(study_layer) <- .col("study", prefix)
  .data <- c(.data, study_layer, warn = FALSE)

  # Placeholder layers so column order is consistent before .add_baseline
  placeholder              <- study_layer
  terra::values(placeholder) <- NA_real_
  baseline_ph              <- placeholder
  result_ph                <- placeholder
  names(baseline_ph)       <- .col("baseline", prefix)
  names(result_ph)         <- .col("result",   prefix)
  .data <- c(.data, baseline_ph, warn = FALSE)
  .data <- c(.data, result_ph,   warn = FALSE)

  # Keep .linked for internal baseline comparison
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

  # Remove internal .linked layer
  .data[[".linked"]] <- NULL

  # Store metadata via metags
  terra::metags(.data) <- c(
    indicator      = indicator,
    unit           = if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator),
    study_fun      = study_fun_name,
    baseline_fun   = if (isFALSE(baseline)) NA_character_ else baseline_fun_name,
    baseline_years = if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2]),
    time_span      = as.character(time_span),
    months         = if (is.null(months)) "" else paste(months, collapse = ","),
    time_lag       = as.character(time_lag),
    prefix         = prefix %||% ""
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
