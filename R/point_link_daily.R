#' Link Spatial Points with Copernicus Earth Observation Daily Indicators
#'
#' @description Downloads and processes Copernicus Earth observation data (ERA5)
#' based on daily temporal parameters, and extracts the relevant daily climate
#' indicator values for the provided spatial points. The function constructs
#' daily time sequences based on a specified date variable and time adjustments
#' (time_lag and time_span), downloads the corresponding daily statistics
#' (e.g., daily mean, maximum, or minimum), and links these values to the
#' spatial points. An optional spatial buffer (in kilometers) can be specified
#' so that instead of a direct cell match, the mean value over the buffer is
#' extracted. If a baseline period is provided (e.g., `baseline = c("1980",
#' "2010")`), baseline daily statistics are downloaded for the specified period
#' and appended as a separate attribute.
#'
#' @param .data An `sf` object containing the spatial data (polygons or points).
#' @param indicator Character string specifying the indicator to download (e.g., "2m_temperature").
#'   Allowed indicators differ by catalogue. See the **Details** section for available indicators.
#' @param date_var Character string specifying the name of the date variable in `data`.
#' @param time_span Integer specifying the time span in days for averaging the climate indicator values prior to linking
#'   with the spatial data (default is `0`).
#' @param time_lag Integer specifying the time lag in days to shift the `date_var` backward (default is `0`).
#' @param baseline Either `FALSE` (default) or a character vector of length 2 specifying the baseline
#'   period in years. For example, `baseline = c("1980", "2010")` uses the years 1980 to 2010 as the baseline.
#'   If `FALSE`, no baseline calculation is performed.
#' @param order Character string specifying the date format for parsing `date_var` (default is `"ymd"`).
#' @param buffer Numeric value specifying the buffer radius (in kilometers) to be applied around each point.
#'   The default is `0`, corresponding to a direct cell match; values greater than 0 generate a spatial buffer
#'   around each point for aggregated extraction.
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"derived-era5-land-daily-statistics"` (default) or `"derived-era5-single-levels-daily-statistics"`.
#' @param statistic Character string specifying the type of daily statistic to download.
#'   Options are `"daily_mean"` (default), `"daily_maximum"`, and `"daily_minimum"`.
#' @param time_zone Character string specifying the time zone to use (default is `"utc+00:00"`).
#' @param cache Logical value indicating whether to keep the downloaded
#'   `.grib` files and restore them when downloading the same file again.
#'   Enabling caching can speed up functions calls significantly when working
#'   with the same files repeatedly. See the **Caching** section for details. If
#'   `FALSE`, removes the raw files after processing.
#' @param path Character string specifying the directory path where data will
#'   be downloaded and cached. If `NULL`, the directory depends on whether
#'   caching is enabled. If `cache = FALSE`, files are stored in a temporary
#'   directory (`tempdir()`), otherwise they are stored in the user directory
#'   (`tools::R_user_dir()`). Defaults to \code{NULL}.
#' @param parallel Logical indicating whether to use parallel processing with
#'   chunking. See section **Parallel processing** for details. Default is
#'   `FALSE` (i.e. sequential execution).
#' @param chunk_size Integer specifying the number of observations per chunk when parallelizing.
#'   Default is `50`.
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to download ERA5 daily reanalysis data for a specified
#' climate indicator and time period based on daily temporal resolution. The input spatial points (an sf object) are first optionally
#' buffered (if `buffer > 0`), then processed to determine the geographic extent. The time dimension is adjusted using the specified
#' `time_lag` and `time_span` (both in days) to create daily time sequences based on the given `date_var`. The function downloads the
#' corresponding daily statistics (e.g., daily mean, maximum, or minimum) and extracts these values for each point. When no buffer is
#' specified, the value from the directly underlying raster cell is extracted; if a buffer is specified, the mean value over the buffer area
#' is computed. If a baseline period is provided (e.g., `baseline = c("1980", "2010")`), baseline daily statistics are downloaded for the
#' specified period and appended as a new attribute. Parallel processing via `future.apply::future_lapply` is supported.
#'
#' @note Users must have a CDS account and have their API key configured for
#' `ecmwfr`.
#'
#' @section Parallel processing:
#' This function can use parallel processing with chunking via
#' `future.apply::future_lapply` when `parallel = TRUE`. If `parallel = FALSE`,
#' the function runs sequentially. When `parallel = TRUE`, set your parallel
#' plan (for example, using `future::plan(multisession, workers = 6)`)
#' before calling this function. If no plan is set before but `parallel = TRUE`,
#' the function will run sequentially through the chunks, which will most
#' likely increase duration.
#'
#' @return An `sf` object with the original spatial data and appended climate indicator values. If a baseline
#' period is specified, additional columns for baseline values and deviations are included.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
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
#' result1 <- point_link_daily(pts_sf, indicator = "2m_temperature")
#'
#' # Example: Aggregated extraction with a 5 km buffer
#' result2 <- point_link_daily(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer = 5,
#'   baseline = c("1980", "2010")
#' )
#'
#' # View the results
#' head(result1)
#' head(result2)}
#' @export
point_link_daily <- function(.data,
                             indicator,
                             date_var = "date",
                             time_span = 0,
                             time_lag = 0,
                             buffer = 0,
                             baseline = FALSE,
                             time_fmt = "%Y-%m-%d",
                             catalogue = "derived-era5-land-daily-statistics",
                             statistic = "daily_mean",
                             time_zone = "utc+00:00",
                             cache = TRUE,
                             path = NULL,
                             parallel = FALSE,
                             chunk_size = 50,
                             verbose = TRUE) {
  # Validate catalogue, indicator choice, statistic, and time-zone
  .check_valid_catalogue(catalogue, temp_res = "daily")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_statistic(statistic)
  .check_valid_time_zone(time_zone)
  .check_column(.data, date_var)
  .check_api_key("ecmwfr")
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  if (!isFALSE(baseline) && length(baseline) != 2) {
    cli::cli_abort(paste(
      "Argument `baseline` must be either FALSE or a vector of length 2",
      "specifying minimum and maximum baseline years."
    ))
  }

  # Prep data and create extent
  prepared <- .prep_points(.data, buffer)
  prepared <- .add_timelag(
    prepared,
    date_var = date_var,
    time_span = time_span,
    time_lag = time_lag
  )
  extent <- .get_extent(.data)

  years <- date_component(unlist(prepared$time_span_seq), "year")
  months <- date_component(unlist(prepared$time_span_seq), "month")
  days <- date_component(unlist(prepared$time_span_seq), "day")

  # Download data from API
  obs_path <- .make_request_daily(
    indicator,
    catalogue = catalogue,
    extent = extent,
    years = years,
    months = months,
    days = days,
    cache = cache,
    path = path,
    prefix = "observation",
    statistic = statistic,
    time_zone = time_zone,
    verbose = verbose
  )

  raster <- terra::rast(obs_path)

  # Add timestamp to raster file
  raster <- .raster_timestamp(raster, days, months, years, path)

  # Check CRS of both datasets and adjust if necessary
  if (!identical(terra::crs(prepared), terra::crs(raster))) {
    prepared <- sf::st_transform(prepared, crs = terra::crs(raster))
  }

  info(
    "Extracting values from raster",
    msg_done = "Extracted values from raster.",
    msg_failed = "Failed to extract values from raster.",
    level = "step"
  )

  # Extract focal values
  raster_values <- .focal_extract(
    raster,
    obs_path,
    prepared,
    time_span = time_span,
    parallel = parallel,
    chunk_size = chunk_size
  )

  # Create new variable in dataframe
  prepared$link_value <- unlist(raster_values)

  # Check baseline argument
  # If no baseline requested, transform back to longitude and latitude and
  # export final output
  if (!isFALSE(baseline) && length(baseline) == 2) {

    # Extract minimum and maximum baseline years
    min_year <- baseline[1]
    max_year <- baseline[2]
    min_bl <- as.POSIXct(sprintf("%s-01-01", min_year))
    max_bl <- as.POSIXct(sprintf("%s-01-01", max_year))
    baseline_years <- seq(min_bl, max_bl, by = "1 year")
    baseline_years <- format(baseline_years, "%Y")

    baseline_path <- .make_request_daily(
      indicator,
      catalogue,
      extent,
      baseline_years,
      months,
      days,
      cache = cache,
      path = path,
      prefix = "baseline",
      statistic = statistic,
      time_zone = time_zone,
      verbose = verbose
    )

    baseline_raster <- terra::rast(baseline_path)

    # Add timestamp to raster file
    baseline_raster <- .raster_timestamp(
      baseline_raster,
      days = days,
      months = months,
      years = baseline_years,
      path = path
    )

    # Extract values from raster for each observation and add to dataframe
    if (length(unique(prepared$link_date)) == 1) {
      # All observations have the same link date
      baseline_raster <- terra::app(baseline_raster, mean)
      baseline_values <- terra::extract(
        baseline_raster,
        prepared,
        fun = mean,
        na.rm = TRUE,
        ID = FALSE
      )
    } else {
      # All observations have different link dates and mean calculation of months
      if (!parallel) {
        # Sequential approach
        baseline_dates <- as.Date(terra::time(baseline_raster))
        baseline_df <- data.frame(
          layer_index = seq_along(baseline_dates),
          month = lubridate::month(baseline_dates),
          day = lubridate::day(baseline_dates)
        )

        baseline_values <- lapply(seq_len(nrow(prepared)), function(i) {
          if (!is.na(prepared[i,]$link_date)) {

            target_dates <- as.Date(ymd(unlist(prepared[i,]$time_span_seq)))
            # "month-day" identifier for the target and baseline dates
            target_md <- paste(month(target_dates), day(target_dates), sep = "-")
            baseline_md <- paste(baseline_df$month, baseline_df$day, sep = "-")
            # Find matches
            match_idx <- baseline_df$layer_index[baseline_md %in% target_md]

            if (length(match_idx) == 0) return(NA)
            raster_subset <- terra::app(baseline_raster[[match_idx]],
                                        fun = mean,
                                        na.rm = TRUE)
            terra::extract(
              raster_subset,
              prepared[i,],
              fun = mean,
              na.rm = TRUE,
              ID = FALSE
            )
          } else {
            NA
          }
        })
        baseline_values <- unlist(baseline_values, recursive = FALSE)
      } else {
        # Parallelization approach
        chunks <- split(seq_len(nrow(prepared)),
                        ceiling(seq_len(nrow(prepared)) / chunk_size))

        baseline_values_list <- future.apply::future_lapply(chunks, function(idx) {
          local_baseline_raster <- terra::rast(baseline_path)
          baseline_dates <- as.Date(terra::time(local_baseline_raster))
          baseline_df <- data.frame(
            layer_index = seq_along(baseline_dates),
            month = lubridate::month(baseline_dates),
            day = lubridate::day(baseline_dates)
          )

          sapply(idx, function(i) {
            if (!is.na(prepared[i,]$link_date)) {
              target_dates <- as.Date(ymd(unlist(prepared[i,]$time_span_seq)))
              target_md <- paste(month(target_dates), day(target_dates), sep = "-")
              baseline_md <- paste(baseline_df$month, baseline_df$day, sep = "-")
              match_idx <- baseline_df$layer_index[baseline_md %in% target_md]

              if (length(match_idx) == 0) return(NA)
              raster_subset <- terra::app(local_baseline_raster[[match_idx]],
                                          fun = mean,
                                          na.rm = TRUE)
              terra::extract(
                raster_subset,
                prepared[i,],
                fun = mean,
                na.rm = TRUE,
                ID = FALSE
              )
            } else {
              NA
            }
          })
        }, future.seed = TRUE)

        baseline_values <- unlist(baseline_values_list, recursive = FALSE)
      }
    }

    # Add variable to dataframe
    prepared$baseline_value <- unlist(baseline_values)

    # Calculate absolute deviation between observation and baseline values
    prepared$deviation <- prepared$link_value - prepared$baseline_value

  }

  prepared <- sf::st_transform(prepared, crs = 4326)

  if (!cache) {
    file.remove(obs_path)
  }

  prepared
}
