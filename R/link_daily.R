#' Link with ERA5 daily indicators
#'
#' @description Augments spatio-temporal data with daily indicators from the
#' Copernicus earth observation database (ERA5).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download daily statistics from Copernicus database}
#'  \item{Link raster statistics back to input}
#'  \item{Optionally, add comparative statistics based on a baseline period}
#' }
#'
#' This function interfaces the daily means of ERA5 indicators. For monthly
#' means see \code{\link{link_monthly}}.
#'
#' @param .data An `sf` object containing the spatial data (polygons or points).
#' @param indicator Character string specifying the indicator to download
#'   (e.g., `"2m_temperature"`). Allowed indicators differ by catalogue. See
#'   the **Details** section for available indicators.
#' @param date_var Character string specifying the name of the date variable in `data`.
#' @param time_span Integer specifying the time span in days for averaging the
#'   climate indicator values prior to linking with the spatial data (default
#'   is `0`).
#' @param time_lag Integer specifying the time lag in days to shift the
#'   `date_var` backward (default is `0`).
#' @param baseline Either `FALSE` (default) or a character vector of length 2
#'   specifying the baseline period in years. For example,
#'   `baseline = c("1980", "2010")` uses the years 1980 to 2010 as the baseline.
#'   If `FALSE`, no baseline calculation is performed.
#' @param method Character string specifying the resampling method to use when
#'   aligning the downloaded data with the grid. Options include `"bilinear"`
#'   (default), `"near"`, `"cubic"`, etc. See \code{\link[terra]{resample}} for
#'   details.
#' @param buffer Numeric value specifying the buffer radius (in kilometers) to
#'   be applied around each geometry. The default is `0`, corresponding to a
#'   direct cell match; values greater than 0 generate a spatial buffer
#'   around each point for aggregated extraction.
#' @param catalogue Character string specifying which catalogue to use.
#'   Options are `"derived-era5-land-daily-statistics"` (default),
#'   `"derived-era5-single-levels-daily-statistics"`, or `"dwd-hyras-daily"`.
#' @param statistic Character string specifying the type of daily statistic to
#'   download (ERA5 only). Options are `"daily_mean"` (default),
#'   `"daily_maximum"`, and `"daily_minimum"`.
#' @param time_zone Character string specifying the time zone to use (ERA5
#'   only, default is `"utc+00:00"`).
#' @param cache Logical value indicating whether to keep the downloaded
#'   files and restore them when downloading the same file again.
#'   Enabling caching can speed up functions calls significantly when working
#'   with the same files repeatedly. See the **Caching** section for details. If
#'   `FALSE`, removes the raw files after processing.
#' @param path Character string specifying the directory path where data will
#'   be downloaded and cached. If `NULL`, the directory depends on whether
#'   caching is enabled. If `cache = FALSE`, files are stored in a temporary
#'   directory (\code{\link{tempdir}}), otherwise they are stored in the user
#'   directory (\code{\link{R_user_dir}}). Defaults to \code{NULL}.
#' @param parallel Logical indicating whether to use parallel processing with
#'   chunking. See section **Parallel processing** for details. Default is
#'   `FALSE` (i.e., sequential execution).
#' @param chunk_size Integer specifying the number of observations per chunk
#'   when parallelizing. Default is `50`.
#' @param verbose Logical value specifiying whether to show informative status
#'   updates. Defaults to \code{TRUE}.
#' @param ... Arguments passed to methods.
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to
#' download ERA5 daily reanalysis data for a specified climate indicator and
#' time period based on daily temporal resolution. The input spatial points
#' (an sf object) are first optionally buffered (if `buffer > 0`), then
#' processed to determine the geographic extent. The time dimension is adjusted
#' using the specified `time_lag` and `time_span` (both in days) to create
#' daily time sequences based on the given `date_var`. The function downloads
#' the corresponding daily statistics (e.g., daily mean, maximum, or minimum)
#' and extracts these values for each point. When no buffer is specified, the
#' value from the directly underlying raster cell is extracted; if a buffer is
#' specified, the mean value over the buffer area is computed. If a baseline
#' period is provided (e.g., `baseline = c("1980", "2010")`), baseline daily
#' statistics are downloaded for the specified period and appended as a new
#' attribute.
#'
#' The following indicators are currently supported:
#'
#' `r rd_indicators("link_daily")`
#'
#' @note Users must have a CDS account and have their API key configured for
#' `ecmwfr` when using ERA5 catalogues.
#'
#' @section Parallel processing:
#' This function can use parallel processing with chunking via
#' \code{\link[future.apply]{future_lapply}} when `parallel = TRUE`. If
#' `parallel = FALSE`, the function runs sequentially. When `parallel = TRUE`,
#' set your parallel plan (for example, using
#' \code{\link[future]{plan}(multisession, workers = 6)})
#' before calling this function. If no plan is set before but `parallel = TRUE`,
#' the function will run sequentially through the chunks, which will most
#' likely increase duration.
#'
#' @return An object of the input class with the original data and appended
#' climate indicator values.
#'
#' @export
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
#' # Example: Direct extraction (buffer = 0)
#' result1 <- link_daily(pts_sf, indicator = "2m_temperature")
#'
#' # Example: Aggregated extraction with a 5 km buffer
#' result2 <- link_daily(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer = 5,
#'   baseline = c("1980", "2010")
#' )
#'
#' # Example: DWD HYRAS daily data
#' result3 <- link_daily(
#'   pts_sf,
#'   indicator = "air_temperature_mean",
#'   catalogue = "dwd-hyras-daily"
#' )
#'
#' # The input can also be raster
#' germany_bbox <- c(xmin = 5, xmax = 16, ymin = 47, ymax = 55)
#' grid <- rast(
#'   xmin = germany_bbox["xmin"], xmax = germany_bbox["xmax"],
#'   ymin = germany_bbox["ymin"], ymax = germany_bbox["ymax"]
#' )
#' terra::time(grid) <- as.Date("2014-08-01")
#' link_daily(grid, indicator = "2m_temperature")}
link_daily <- function(.data,
                       indicator,
                       ...,
                       cache = TRUE,
                       path = NULL,
                       parallel = FALSE,
                       chunk_size = 50,
                       verbose = TRUE) {
  UseMethod("link_daily")
}


#' @rdname link_daily
#' @export
link_daily.sf <-
  function(
    .data,
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
    verbose        = TRUE
  ) {
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

    crs_data <- terra::crs(.data)
    old_geom <- sf::st_geometry(.data)
    prepared <- sf::st_transform(.data, 4326)
    prepared <- sf::st_buffer(prepared, buffer)

    splits   <- split(prepared, prepared[[date_var]])
    n_splits <- length(splits)
    result   <- vector("list", n_splits)

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

    # Collect all unique days across all splits
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

    # Dispatch request by catalogue source
    obs_path <- if (.catalogue_source(catalogue) == "era5") {
      obs_request <- .build_era5_daily_request(
        indicator = indicator,
        catalogue = catalogue,
        extent    = global_extent,
        years     = format(all_obs_span, "%Y"),
        months    = format(all_obs_span, "%m"),
        days      = format(all_obs_span, "%d"),
        prefix    = "observation",
        statistic = statistic,
        time_zone = time_zone
      )
      .submit_era5_batch(obs_request, path = path, cache = cache, verbose = verbose)
    } else {
      .request_dwd_daily(
        indicator = indicator,
        years     = format(all_obs_span, "%Y"),
        months    = format(all_obs_span, "%m"),
        days      = format(all_obs_span, "%d"),
        cache     = cache,
        path      = path,
        prefix    = "observation"
      )
    }

    # Load global observation raster once for all splits
    obs_raster <- terra::rast(obs_path)
    if (!inherits(terra::time(obs_raster), "POSIXt")) {
      obs_raster <- raster_timestamp(
        obs_raster,
        days   = format(all_obs_span, "%d"),
        months = format(all_obs_span, "%m"),
        years  = format(all_obs_span, "%Y"),
        span   = all_obs_span
      )
    }

    # Baseline requests upfront if needed
    baseline_raster <- NULL
    if (!isFALSE(baseline)) {
      baseline_years <- format(
        make_dates(seq(baseline[1], baseline[2]), months = 1, days = 1),
        "%Y"
      )
      all_baseline_span <- sort(unique(do.call(c, lapply(baseline_years, function(y) {
        as.Date(paste(y, format(all_obs_span, "%m-%d"), sep = "-"))
      }))))

      if (verbose) {
        cli::cli_progress_message(
          "Submitting baseline requests ({length(all_baseline_span)} day{?s})..."
        )
      }

      baseline_path <- if (.catalogue_source(catalogue) == "era5") {
        baseline_request <- .build_era5_daily_request(
          indicator = indicator,
          catalogue = catalogue,
          extent    = global_extent,
          years     = format(all_baseline_span, "%Y"),
          months    = format(all_baseline_span, "%m"),
          days      = format(all_baseline_span, "%d"),
          prefix    = "baseline",
          statistic = statistic,
          time_zone = time_zone
        )
        .submit_era5_batch(
          baseline_request, path = path, cache = cache, verbose = verbose
        )
      } else {
        .request_dwd_daily(
          indicator = indicator,
          years     = format(all_baseline_span, "%Y"),
          months    = format(all_baseline_span, "%m"),
          days      = format(all_baseline_span, "%d"),
          cache     = cache,
          path      = path,
          prefix    = "baseline"
        )
      }

      # Load global baseline raster once for all splits
      baseline_raster <- terra::rast(baseline_path)
      baseline_raster <- raster_timestamp(
        baseline_raster,
        days   = format(all_baseline_span, "%d"),
        months = format(all_baseline_span, "%m"),
        years  = format(all_baseline_span, "%Y"),
        span   = all_baseline_span
      )
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
          time_lag  = time_lag
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
          time_span      = time_span,
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

  temporals <- .transform_time(
    prepared,
    time_span = time_span,
    time_lag  = time_lag
  )

  global_extent <- .get_extent(prepared)
  # Collect all unique days from temporals
  all_spans    <- list(sort(unique(do.call(c, lapply(temporals$time_span_seq, as_date)))))
  all_obs_span <- sort(unique(do.call(c, all_spans)))

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

  all_obs_span <- sort(unique(do.call(c, all_spans)))

  if (verbose) {
    cli::cli_progress_message(
      "Submitting observation requests ({length(all_obs_span)} day{?s})..."
    )
  }

  # Dispatch request by catalogue source
  obs_path <- if (.catalogue_source(catalogue) == "era5") {
    obs_request <- .build_era5_daily_request(
      indicator = indicator,
      catalogue = catalogue,
      extent    = global_extent,
      years     = format(all_obs_span, "%Y"),
      months    = format(all_obs_span, "%m"),
      days      = format(all_obs_span, "%d"),
      prefix    = "observation",
      statistic = statistic,
      time_zone = time_zone
    )
    .submit_era5_batch(obs_request, path = path, cache = cache, verbose = verbose)
  } else {
    .request_dwd_daily(
      indicator = indicator,
      years     = format(all_obs_span, "%Y"),
      months    = format(all_obs_span, "%m"),
      days      = format(all_obs_span, "%d"),
      cache     = cache,
      path      = path,
      prefix    = "observation"
    )
  }

  # Load global observation raster and reproject to original CRS
  obs_raster <- terra::rast(obs_path)
  if (!inherits(terra::time(obs_raster), "POSIXt")) {
    obs_raster <- raster_timestamp(
      obs_raster,
      days   = format(all_obs_span, "%d"),
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
      as.Date(paste(y, format(all_obs_span, "%m-%d"), sep = "-"))
    }))))

    if (verbose) {
      cli::cli_progress_message(
        "Submitting baseline requests ({length(all_baseline_span)} day{?s})..."
      )
    }

    baseline_path <- if (.catalogue_source(catalogue) == "era5") {
      baseline_request <- .build_era5_daily_request(
        indicator = indicator,
        catalogue = catalogue,
        extent    = global_extent,
        years     = format(all_baseline_span, "%Y"),
        months    = format(all_baseline_span, "%m"),
        days      = format(all_baseline_span, "%d"),
        prefix    = "baseline",
        statistic = statistic,
        time_zone = time_zone
      )
      .submit_era5_batch(
        baseline_request, path = path, cache = cache, verbose = verbose
      )
    } else {
      .request_dwd_daily(
        indicator = indicator,
        years     = format(all_baseline_span, "%Y"),
        months    = format(all_baseline_span, "%m"),
        days      = format(all_baseline_span, "%d"),
        cache     = cache,
        path      = path,
        prefix    = "baseline"
      )
    }

    baseline_raster <- terra::rast(baseline_path)
    baseline_raster <- raster_timestamp(
      baseline_raster,
      days   = format(all_baseline_span, "%d"),
      months = format(all_baseline_span, "%m"),
      years  = format(all_baseline_span, "%Y"),
      span   = all_baseline_span
    )
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
    time_lag       = as.character(time_lag),
    prefix         = prefix %||% ""
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
