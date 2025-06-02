#' Link Spatial Points with Copernicus Earth Observation Daily Indicators
#'
#' @description Augments spatio-temporal data with indicators from the
#' Copernicus earth observation database (ERA5).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download daily statistics from Copernicus database}
#'  \item{Link raster statistics back to input}
#'  \item{Optionially, add comparative statistics based on a baseline period}
#' }
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
#'   when parallelizing. This is the number of rows that is processed
#'   simultaneously during parallel processing. Setting this to something lower
#'   than `nrow(.data)` increases the total number of parallel processes.
#'   Default is `50`.
#' @param ... Arguments passed to methods. If `.data` is a stars object,
#'   arguments are passed to `link_daily.SpatRaster`, otherwise to
#'   `link_daily.sf`.
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
link_daily.sf <- function(.data,
                          indicator,
                          date_var = "date",
                          time_span = 0,
                          time_lag = 0,
                          buffer = 0,
                          baseline = FALSE,
                          time_fmt = "%Y-%m-%d",
                          catalogue = "derived-era5-land-daily-statistics",
                          tatistic = "daily_mean",
                          time_zone = "utc+00:00",
                          cache = TRUE,
                          path = NULL,
                          parallel = FALSE,
                          chunk_size = 50,
                          verbose = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "daily")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_statistic(statistic)
  .check_valid_time_zone(time_zone)
  .check_baseline(baseline)
  .check_column(.data, date_var)
  .check_api_key("ecmwfr")
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  if (chunk_size > nrow(.data)) {
    info("Chunk size is higher than number of input rows. Disabling parallelization.")
    parallel <- FALSE
  }

  # Prep data and create extent
  prepared <- sf::st_transform(.data, 4326)
  prepared <- sf::st_buffer(.data, buffer)
  prepared <- .transform_time(
    prepared,
    date_var = date_var,
    time_span = time_span,
    time_lag = time_lag
  )
  extent <- .get_extent(.data)

  span <- unlist(prepared$time_span_seq)
  years <- num_keys(year(span))
  months <- num_keys(month(span))
  days <- num_keys(day(span))

  # Download data from API
  obs_path <- .request_daily_temp(
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
    info(
      "Transforming input data to match the CRS of the indicator dataset",
      level = "warning"
    )
    prepared <- sf::st_transform(prepared, crs = terra::crs(raster))
  }

  info(
    "Extracting values from raster",
    msg_done = "Extracted values from raster.",
    msg_failed = "Failed to extract values from raster.",
    level = "step"
  )

  # Extract focal values
  raster_values <- .toi_extract(
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
  if (!isFALSE(baseline)) {
    prepared <- .add_baseline(
      prepared,
      baseline = baseline,
      indicator = indicator,
      days = days,
      months = months,
      extent = extent,
      catalogue = catalogue,
      statistic = statistic,
      time_zone = time_zone,
      cache = cache,
      path = path,
      parallel = parallel,
      chunk_size = chunk_size,
      verbose = verbose
    )
  }

  prepared <- sf::st_transform(prepared, crs = 4326)

  if (!cache) {
    unlink(obs_path)
    unlink(baseline_path)
  }

  prepared[c("link_date", "link_date_end", "time_span_seq")] <- NULL
  prepared <- move_to_back(prepared, attr(prepared, "sf_column"))
  as_sf_tibble(prepared)
}


#' @rdname link_daily
#' @export
link_daily.SpatRaster <- function(.data,
                                  indicator,
                                  time_span = 0,
                                  time_lag = 0,
                                  baseline = FALSE,
                                  method = "bilinear",
                                  catalogue = "derived-era5-land-daily-statistics",
                                  tatistic = "daily_mean",
                                  time_zone = "utc+00:00",
                                  cache = TRUE,
                                  path = NULL,
                                  parallel = FALSE,
                                  chunk_size = 50,
                                  verbose = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "daily")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_statistic(statistic)
  .check_valid_time_zone(time_zone)
  .check_baseline(baseline)
  .check_terra_time(.data)
  .check_api_key("ecmwfr")
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  temporals <- .transform_time(
    .data,
    date_var = date_var,
    time_span = time_span,
    time_lag = time_lag
  )
  extent <- .get_extent(.data)

  span <- unlist(temporals$time_span_seq)
  years <- num_keys(year(span))
  months <- num_keys(month(span))
  days <- num_keys(day(span))

  # Download data from API
  obs_path <- .request_daily_temp(
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
  if (!identical(terra::crs(.data), terra::crs(raster))) {
    info("Reprojecting indicator raster to match input data", level = "warning")
    prepared <- sf::st_transform(raster, crs = terra::crs(.data))
  }

  info(
    "Extracting values from raster",
    msg_done = "Extracted values from raster.",
    msg_failed = "Failed to extract values from raster.",
    level = "step"
  )

  # Extract focal values
  raster_values <- .toi_extract_grid(
    raster,
    .data,
    temporals,
    time_span = time_span,
    parallel = parallel,
    chunk_size = chunk_size,
    method = method
  )

  .data <- c(.data, linked = raster_values)
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
