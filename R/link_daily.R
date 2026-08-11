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
#' @param time_lag Integer specifying the time lag to shift the `date_var`
#'   backward before extraction. Default is `0`. For `link_daily()`, this
#'   is in **days**. For `link_monthly()`, this is in **calendar months**
#'   (using calendar-aware month arithmetic, e.g. shifting back a month
#'   from March 31 correctly lands on the last day of February) -- a
#'   day-based shift wouldn't make sense there, since the date is
#'   normalized to its containing month before matching against
#'   monthly-resolution rasters anyway.
#' @param months Optional integer vector specifying explicit months to use
#'   as the study period (e.g. `c(3, 4, 5)` for spring), mirroring
#'   `link_monthly()`'s `months` argument but expanded to the full daily
#'   sequence spanning those months (needed since `link_daily()` matches
#'   individual days, not months). If the input date falls within one of
#'   the specified months, the window is automatically shifted one year
#'   back to avoid using incomplete data -- months given out of calendar
#'   order (e.g. `c(12, 1, 2)` for winter) correctly span a year boundary,
#'   with no special tagging needed. Cannot be combined with `time_span`.
#' @param buffer Numeric value specifying the buffer radius in metres to
#'   be applied around each geometry. The default is `0`, corresponding to a
#'   direct cell match; values greater than 0 generate a spatial buffer
#'   around each point for aggregated extraction.
#' @param downsample_factor Integer (e.g. `5`) or `NULL` (default, off).
#'   If set, an aggregated (coarser-resolution) copy of the downloaded
#'   raster is built once (and cached across calls sharing the same files
#'   and factor) and used for polygon (buffer > 0) extraction where
#'   `buffer >= downsample_min_buffer`. Point extraction (`buffer = 0`)
#'   always stays at full resolution regardless of this setting. Own
#'   measurement (5-100km buffers, factor 5) showed <0.1% relative
#'   difference vs. full resolution, with a 10-32x extraction speedup.
#' @param downsample_min_buffer Numeric, default `0`. Only used when
#'   `downsample_factor` is set. Minimum buffer radius (metres) for the
#'   aggregated raster to be used; buffers smaller than this still use
#'   full resolution. Default `0` means every real buffer (> 0) uses the
#'   aggregated version, and only point extraction is excluded.
#' @param baseline Either `FALSE` (default) or a character vector of length 2
#'   specifying the baseline period as start and end year. For example,
#'   `baseline = c(1980, 2010)` uses the years 1980 to 2010 as the
#'   baseline. If `FALSE`, no baseline calculation is performed.
#' @param baseline_fun Character string or function specifying how baseline
#'   layers are collapsed to a single reference value. Accepts one of
#'   `"mean"` (default), `"median"`, `"min"`, `"max"`, `"sd"`, or percentiles
#'   `"p05"`, `"p10"`, `"p20"`, `"p80"`, `"p90"`, `"p95"`. A custom function
#'   is also accepted but will be labelled `"custom"` in output metadata.
#'   Ignored when `baseline = FALSE`.
#'
#'   Can also be a **list** of such values (e.g.
#'   `list("median", "p90")`) to request multiple baseline_fun/
#'   stat_wrangling combinations in a single call, sharing the (expensive)
#'   baseline extraction across all of them -- see the **Multiple
#'   combinations** section below. `stat_wrangling` must then also be a list
#'   of the same length (or length 1, recycled).
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
#'   Ignored when `baseline = FALSE`. Can also be a **list**, paired
#'   positionally with `baseline_fun` -- see **Multiple combinations** below.
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
#' @param cache_results Logical, default `FALSE`. If `TRUE`, caches the
#'   final per-date result (after extraction and aggregation) to disk, so a
#'   crash mid-run only loses whatever hasn't been cached yet, and re-runs
#'   with identical parameters skip computation entirely for already-cached
#'   dates. Distinct from `cache`, which only caches raw downloaded files --
#'   this caches the finished output. Not enabled by default because cached
#'   entries necessarily store the exact geometry of the input observations
#'   (needed for cache-key uniqueness and an integrity self-check), which
#'   for survey data can indirectly reveal respondent locations.
#' @param results_cache_path Character string, required when
#'   `cache_results = TRUE`. No default is provided deliberately -- the
#'   storage location for potentially sensitive cached geometry should be
#'   a conscious choice, not a fallback default.
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
#' @section Multiple combinations:
#' `baseline_fun` and `stat_wrangling` can each be given as a **list** to
#' request several baseline_fun/stat_wrangling combinations in a single
#' call, e.g.:
#' \preformatted{
#' link_daily(
#'   pts, indicator = "air_temperature_mean", catalogue = "dwd-hyras-daily",
#'   baseline = c("1961", "1990"),
#'   baseline_fun   = list("median", "p90"),
#'   stat_wrangling = list("count_above", "count_above")
#' )
#' }
#' Combinations are paired **positionally** (element 1 with element 1,
#' element 2 with element 2 -- not a cross product). If one of the two is
#' length 1 and the other is longer, the length-1 one is recycled. The
#' expensive part of the computation (downloading and extracting baseline
#' values) is shared across all combinations; only the final aggregation
#' (`baseline_fun`/`stat_wrangling`) is repeated per combination, on
#' already-extracted values.
#'
#' The return value changes shape when more than one combination is
#' requested: instead of a single `sf` object, a **named list** of `sf`
#' objects is returned, one per combination (named
#' `"{baseline_fun_name}_{stat_wrangling}"`). A single combination (the
#' default, and the case when both arguments are given as plain strings/
#' functions) continues to return a single `sf` object, unchanged from
#' previous versions.
#'
#' This is currently only supported for `sf` input, not `SpatRaster` input.
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
#'   values and associated metadata. A **named list** of such objects if
#'   multiple baseline_fun/stat_wrangling combinations were requested (sf
#'   input only) -- see **Multiple combinations** above.
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
#' # Multiple baseline_fun/stat_wrangling combinations, sharing extraction
#' result2b <- link_daily(
#'   pts_sf,
#'   indicator      = "2m_temperature",
#'   baseline       = c("1980", "2010"),
#'   baseline_fun   = list("median", "p90"),
#'   stat_wrangling = list("count_above", "count_above")
#' )
#' result2b$median_count_above
#' result2b$p90_count_above
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
                          months         = NULL,
                          buffer         = 0,
                          downsample_factor      = NULL,
                          downsample_min_buffer  = 0,
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
                          cache_results       = FALSE,
                          results_cache_path  = NULL,
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
  .check_months_time_span(months, time_span)
  .check_valid_months(months)

  if (isTRUE(cache_results) && is.null(results_cache_path)) {
    cli::cli_abort(c(
      "{.arg results_cache_path} must be set when {.arg cache_results = TRUE}.",
      "i" = "There is no default location -- cached results contain the ",
      "exact geometry of your input observations, which for survey data ",
      "can indirectly reveal respondent locations. Choose the storage ",
      "location deliberately."
    ))
  }

  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  # ---------------------------------------------------------------------
  # Resolve baseline_fun/stat_wrangling -- either single values (default,
  # fully backward compatible) or lists of length > 1 requesting multiple
  # combinations that share the (expensive) baseline extraction. See the
  # "Multiple combinations" section in the function docs.
  # ---------------------------------------------------------------------

  multi_combo <- is.list(baseline_fun) || is.list(stat_wrangling)

  if (!multi_combo) {
    stat_wrangling <- match.arg(stat_wrangling)

    if (stat_wrangling %in% c("count_above", "count_below") &&
        time_span == 0 && is.null(months)) {
      cli::cli_abort(c(
        "{.val {stat_wrangling}} requires {.arg time_span} > 0 or {.arg months} to be specified.",
        "i" = "With a single focal day there is only one value to compare against the baseline."
      ))
    }

    baseline_resolved  <- .resolve_baseline_fun(baseline_fun, arg = "baseline_fun")
    baseline_fun_names <- baseline_resolved$name
    baseline_fun_list  <- list(baseline_resolved$fun)
    stat_wrangling_list <- list(stat_wrangling)

  } else {
    baseline_fun_list_raw   <- if (is.list(baseline_fun))   baseline_fun   else list(baseline_fun)
    stat_wrangling_list_raw <- if (is.list(stat_wrangling)) stat_wrangling else list(stat_wrangling)

    if (length(baseline_fun_list_raw) != length(stat_wrangling_list_raw)) {
      if (length(baseline_fun_list_raw) == 1) {
        baseline_fun_list_raw <- rep(baseline_fun_list_raw, length(stat_wrangling_list_raw))
      } else if (length(stat_wrangling_list_raw) == 1) {
        stat_wrangling_list_raw <- rep(stat_wrangling_list_raw, length(baseline_fun_list_raw))
      } else {
        cli::cli_abort(c(
          "{.arg baseline_fun} and {.arg stat_wrangling} must have the same ",
          "length (or one of length 1) when either is given as a list.",
          "i" = "Got {length(baseline_fun_list_raw)} baseline_fun value(s) and ",
          "{length(stat_wrangling_list_raw)} stat_wrangling value(s)."
        ))
      }
    }

    stat_wrangling_list <- lapply(stat_wrangling_list_raw, function(sw) {
      match.arg(sw, choices = c("deviation", "sd_deviation", "count_above", "count_below"))
    })

    for (sw in stat_wrangling_list) {
      if (sw %in% c("count_above", "count_below") && time_span == 0 && is.null(months)) {
        cli::cli_abort(c(
          "{.val {sw}} requires {.arg time_span} > 0 or {.arg months} to be specified.",
          "i" = "With a single focal day there is only one value to compare against the baseline."
        ))
      }
    }

    resolved_list <- lapply(baseline_fun_list_raw, .resolve_baseline_fun, arg = "baseline_fun")
    baseline_fun_names <- vapply(resolved_list, `[[`, character(1), "name")
    baseline_fun_list  <- lapply(resolved_list, `[[`, "fun")
  }

  n_combos     <- length(baseline_fun_list)
  combo_labels <- paste0(baseline_fun_names, "_", unlist(stat_wrangling_list))

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
      "Time span"         = "{.val {if (is.null(months)) time_span else 'via months'}}",
      "Months"            = "{.val {if (is.null(months)) '(none)' else paste(months, collapse = ', ')}}",
      "Time lag"          = "{.val {time_lag}}",
      "Baseline"          =
        "{.val {if (isFALSE(baseline)) 'none' else paste0(baseline[1], '-', baseline[2])}}",
      "Baseline function" = "{.val {paste(baseline_fun_names, collapse = ', ')}}",
      "Study function"    = "{.val {study_fun_name}}",
      "Stat wrangling"    = "{.val {paste(unlist(stat_wrangling_list), collapse = ', ')}}",
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
      date_var        = date_var,
      time_span       = time_span,
      time_lag        = time_lag,
      months          = months,
      daily_expansion = TRUE
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

  obs_raster <- .load_climate_raster(
    obs_path, all_obs_span, daily = TRUE,
    downsample_factor = downsample_factor
  )

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
    baseline_raster <- .load_climate_raster(
      baseline_path, all_baseline_span, daily = TRUE,
      downsample_factor = downsample_factor
    )
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

      cache_key <- NULL
      cached_combo_list <- NULL

      if (isTRUE(cache_results)) {
        cache_key <- .results_cache_key(
          splitted, "daily", indicator, catalogue, time_span, months, time_lag,
          buffer, baseline, baseline_fun_names, stat_wrangling_list,
          study_fun_name, downsample_factor, downsample_min_buffer, prefix,
          splitted[[date_var]][1]
        )
        cached_combo_list <- .read_results_cache(results_cache_path, cache_key, splitted)
      }

      if (!is.null(cached_combo_list)) {
        cached_combo_list

      } else {
        prepared_i <- .transform_time(
          splitted,
          date_var        = date_var,
          time_span       = time_span,
          time_lag        = time_lag,
          months          = months,
          daily_expansion = TRUE
        )

        prepared_i <- .align_crs_vector(prepared_i, obs_raster)

        # baseline_fun/stat_wrangling passed through as LISTS -- the
        # extraction itself is identical regardless of how many combinations
        # are requested; only .add_baseline()'s final aggregation below
        # varies per combination. time_span is forced to a positive dummy
        # value when months is set, mirroring link_monthly.R -- .toi_extract()
        # only checks time_span > 0 to decide whether to aggregate; the
        # actual window used comes entirely from time_span_seq (built above
        # with daily_expansion = TRUE), not from this value.
        raster_values <- .toi_extract(
          prepared_i,
          obs_raster,
          obs_path,
          time_span              = if (!is.null(months)) 1L else time_span,
          parallel               = parallel,
          chunk_size             = chunk_size,
          baseline_fun           = baseline_fun_list,
          stat_wrangling         = stat_wrangling_list,
          buffer                 = buffer,
          downsample_min_buffer  = downsample_min_buffer
        )

        prepared_i[[.col("study",    prefix)]] <-
          .extract_study_values(raster_values, study_fun)
        prepared_i[[.col("baseline", prefix)]] <- NA_real_
        prepared_i[[.col("result",   prefix)]] <- NA_real_
        prepared_i$.linked <- prepared_i[[.col("study", prefix)]]

        if (!isFALSE(baseline)) {
          combo_result <- .add_baseline(
            prepared_i,
            baseline               = baseline,
            baseline_fun_list      = baseline_fun_list,
            baseline_fun_names     = baseline_fun_names,
            stat_wrangling_list    = stat_wrangling_list,
            indicator              = indicator,
            focal_values           = raster_values,
            prefix                 = prefix,
            obs_raster             = obs_raster,
            baseline_raster        = baseline_raster,
            cache                  = cache,
            path                   = path,
            parallel               = parallel,
            chunk_size             = chunk_size,
            verbose                = verbose,
            buffer                 = buffer,
            downsample_min_buffer  = downsample_min_buffer
          )
        } else {
          combo_result <- prepared_i
        }

        # .add_baseline() returns a single sf object when n_combos == 1, or a
        # named list of them when n_combos > 1 (see baseline.R). Normalize to
        # always a named list here, so the rest of this block (and the
        # re-assembly after the loop) is uniform regardless of n_combos.
        combo_list <-
          if (n_combos > 1) combo_result else stats::setNames(list(combo_result), combo_labels[1])

        combo_list <- lapply(seq_along(combo_list), function(k) {
          d <- combo_list[[k]]
          d$.linked <- NULL

          # Column/metadata naming stays governed ONLY by the user-supplied
          # prefix, regardless of n_combos -- disambiguation across
          # combinations happens via the LIST NAME (combo_labels), not via
          # column suffixing. See the matching fix in .add_baseline()
          # (baseline.R) for the full explanation.
          this_prefix <- prefix

          # When months is set, `time_span` itself is just the internal
          # dummy value (1L) used to trigger the aggregation branch -- the
          # actual window length lives in prepared_i$time_span_seq instead.
          # Metadata should reflect what was ACTUALLY used, not this dummy.
          actual_time_span <- if (!is.null(months)) {
            lengths(prepared_i$time_span_seq)[1]
          } else {
            time_span
          }

          .write_metadata_sf(
            d,
            prefix                 = this_prefix,
            indicator              = indicator,
            catalogue              = catalogue,
            baseline               = baseline,
            stat_wrangling         = stat_wrangling_list[[k]],
            study_fun_name         = study_fun_name,
            baseline_fun_name      = baseline_fun_names[k],
            time_span              = actual_time_span,
            time_lag               = time_lag,
            buffer                 = buffer,
            time_unit              = "days",
            months                 = months,
            downsample_factor      = downsample_factor,
            downsample_min_buffer  = downsample_min_buffer
          )
        })
        names(combo_list) <- combo_labels

        if (!cache) unlink(obs_path)

        if (isTRUE(cache_results)) {
          .write_results_cache(results_cache_path, cache_key, splitted, combo_list)
        }

        combo_list
      }
    }
  }

  if (verbose) {
    cli::cli_progress_done(id = pb)
    elapsed <- round(proc.time()[["elapsed"]] - t_start, 1)
    cli::cli_alert_success(
      "Done. {nrow(.data)} observation{?s} across {n_splits} date{?s} linked in {elapsed}s."
    )
  }

  # result: list (per date split) of named list (per combination) of sf
  # objects. Re-assemble per combination: row-bind that combination's
  # fragment across all date splits, then restore input order/CRS/geometry
  # exactly as before.
  finalize_combo <- function(combo_name) {
    prepared_combo <- do.call(rbind, lapply(result, function(x) x[[combo_name]]))

    # Restore original input order (split() above grouped rows by date_var,
    # sorted by unique date values) before old_geom -- which is still in
    # original input order -- gets written back below.
    prepared_combo <- prepared_combo[order(prepared_combo$.row_id), ]

    prepared_combo <- sf::st_transform(prepared_combo, crs = crs_data)
    prepared_combo[c("link_date", "link_date_end", "time_span_seq", ".row_id")] <- NULL
    prepared_combo <- move_to_back(prepared_combo, attr(prepared_combo, "sf_column"))
    sf::st_geometry(prepared_combo) <- old_geom
    as_sf_tibble(prepared_combo)
  }

  if (n_combos == 1) {
    finalize_combo(combo_labels[1])
  } else {
    stats::setNames(lapply(combo_labels, finalize_combo), combo_labels)
  }
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
  # NOTE: multi-combination baseline_fun/stat_wrangling (see link_daily.sf())
  # is NOT supported here -- SpatRaster input still only accepts a single
  # baseline_fun/stat_wrangling value each, exactly as before.
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
      baseline            = baseline,
      baseline_fun_list   = list(baseline_fun),
      baseline_fun_names  = baseline_fun_name,
      stat_wrangling_list = list(stat_wrangling),
      indicator           = indicator,
      prefix              = prefix,
      obs_raster          = obs_raster,
      baseline_raster     = baseline_raster,
      cache               = cache,
      path                = path,
      parallel            = parallel,
      chunk_size          = chunk_size,
      verbose             = verbose
    )
  }

  .data[[".linked"]] <- NULL

  terra::metags(.data) <- c(
    indicator         = indicator,
    unit              = .indicator_units[[indicator]] %||% NA_character_,
    resolution        = .catalogue_resolution[[catalogue]] %||% NA_character_,
    time_unit         = "days",
    result_unit       = if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator),
    study_fun         = study_fun_name,
    baseline_fun      = if (isFALSE(baseline)) NA_character_ else baseline_fun_name,
    baseline_years    = if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2]),
    time_span         = as.character(time_span),
    time_lag          = as.character(time_lag),
    prefix            = prefix %||% "",
    downsample_factor = NA_character_,  # not supported for SpatRaster input
    source            = .catalogue_citation(catalogue, indicator)
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
