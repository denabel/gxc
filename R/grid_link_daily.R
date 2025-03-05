#' Link Gridded Data with Copernicus Earth Observation Daily Indicators
#'
#' Downloads and processes Copernicus Earth observation data (ERA5) based on
#' gridded daily temporal parameters, and extracts the relevant daily climate
#' indicator values for the provided gridded dataset. The function uses the
#' time dimension of the input grid (a SpatRaster) along with specified time
#' adjustments (time_lag and time_span) to construct daily time sequences.
#' It then downloads the corresponding daily statistics (e.g., daily mean, maximum,
#' or minimum) and extracts these values for each grid cell. If a baseline period
#' is specified (e.g., `baseline = c("1980", "2010")`), baseline daily statistics
#' are downloaded for the specified period and appended as a separate layer.
#' The resampling method (e.g., `"bilinear"`) is used to align the downloaded data
#' with the input grid. Parallel processing via `future.apply::future_lapply`
#' is supported.
#'
#' @param indicator Character string specifying the indicator to download (e.g., "2m_temperature").
#'   Allowed indicators differ by catalogue. See the **Details** section for available indicators.
#' @param data A SpatRaster containing the gridded input data.
#' @param time_span Integer specifying the time span in days for averaging the climate indicator values prior to linking
#'   with the spatial data (default is `0`).
#' @param time_lag Integer specifying the time lag in days to shift the `date_var` backward (default is `0`).
#' @param baseline Either `FALSE` (default) or a character vector of length 2 specifying the baseline
#'   period in years. For example, `baseline = c("1980", "2010")` uses the years 1980 to 2010 as the baseline.
#'   If `FALSE`, no baseline calculation is performed.
#' @param path Character string specifying the directory path where data will be downloaded and/or stored
#'   (default is `"./data/raw"`).
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"derived-era5-land-daily-statistics"` (default) or `"derived-era5-single-levels-daily-statistics"`.
#' @param statistic Character string specifying the type of daily statistic to download.
#'   Options are `"daily_mean"` (default), `"daily_maximum"`, and `"daily_minimum"`.
#' @param time_zone Character string specifying the time zone to use (default is `"utc+00:00"`).
#' @param method Character string specifying the resampling method to use when aligning the downloaded data with the grid.
#'   Options include `"bilinear"` (default), `"near"`, `"cubic"`, etc.
#' @param keep_raw Logical value indicating whether to keep the downloaded raw `.grib` files.
#'   If `FALSE`, the files are deleted after processing (default is `FALSE`).
#' @param parallel Logical indicating whether to use parallel processing with chunking.
#'   Default is `FALSE` (i.e. sequential execution).
#' @param chunk_size Integer specifying the number of observations per chunk when parallelizing.
#'   Default is `50`.
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to download ERA5 daily reanalysis data for a specified
#' climate indicator and time period based on daily temporal resolution. The input gridded data (a SpatRaster) is processed to determine
#' its spatial extent and its time dimension is used—after adjusting by the specified time_lag and time_span—to build daily time sequences.
#' The function downloads the corresponding daily statistics (e.g., daily mean, maximum, or minimum) and extracts these values for each
#' grid cell. If a baseline period is provided (e.g., `baseline = c("1980", "2010")`), baseline daily statistics are downloaded for the
#' specified period and appended as a new layer. The resampling method (e.g., `"bilinear"`) is applied to ensure that the downloaded data
#' aligns with the input grid. Parallel processing via `future.apply::future_lapply` is supported to improve performance.
#'
#' **Note:** Users must have a CDS account and have their API key configured for `ecmwfr`.
#'
#' **Parallel Processing:**
#' This function can use parallel processing with chunking via
#' `future.apply::future_lapply` when `parallel = TRUE`. If `parallel = FALSE`,
#' the function runs sequentially. When `parallel = TRUE`, set your parallel
#' plan (for example, using `future::plan(multisession, workers = 6)`)
#' before calling this function. If no plan is set before but `parallel = TRUE`,
#' the function will run sequentially through the chunks, which will most
#' likely increase duration.
#'
#' @return A SpatRaster containing the original grid along with appended layers for the focal climate indicator values and, if requested,
#'   baseline values (and optionally deviations).
#'
#' @importFrom dplyr mutate %>%
#' @importFrom lubridate parse_date_time ymd year month day
#' @importFrom terra rast extract app time crs writeCDF project resample
#' @importFrom tibble tibble
#' @importFrom keyring key_get
#' @importFrom ecmwfr wf_set_key wf_request
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Create a sample grid (SpatRaster) covering a given extent.
#' # For instance, create a raster with 10 km resolution over Germany's bounding box.
#' germany_bbox <- c(xmin = 5, xmax = 16, ymin = 47, ymax = 55) # approximate extent
#' sample_grid <- rast(xmin = germany_bbox["xmin"], xmax = germany_bbox["xmax"],
#'                     ymin = germany_bbox["ymin"], ymax = germany_bbox["ymax"],
#'                     resolution = 10000, crs = "EPSG:4326")
#' terra::time(sample_grid) <- as.Date("2014-08-01")
#'
#' # Example: Download daily focal values for "2m_temperature" and optionally baseline
#' result <- grid_link_daily(
#'   indicator = "2m_temperature",
#'   data = sample_grid,
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = c("1980", "2010"),
#'   path = "./data/raw",
#'   catalogue = "derived-era5-land-daily-statistics",
#'   statistic = "daily_mean",
#'   time_zone = "utc+00:00",
#'   method = "bilinear",
#'   keep_raw = FALSE,
#'   parallel = FALSE,
#'   chunk_size = 50
#' )
#' plot(result)
#' }
#'
#' @export

grid_link_daily <- function(
    indicator,
    data,
    time_span = 0,
    time_lag = 0,
    baseline = FALSE,
    path = "./data/raw",
    catalogue = "derived-era5-land-daily-statistics",
    statistic = "daily_mean",
    time_zone = "utc+00:00",
    method = "bilinear",
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
) {
  with_progress({
    p <- progressor(steps = 4)

    # Validate catalogue, indicator choice, statistic, and time-zone
    .check_valid_catalogue(catalogue, temp_res = "daily")
    .check_valid_indicator(indicator, catalogue)
    .check_valid_statistic(statistic)
    .check_valid_time_zone(time_zone)

    # Access to API
    api_key <- key_get("wf_api_key")
    wf_set_key(key = api_key)

    # Prep data and create extent
    prepared <- .prep_grid(data)
    data_sf <- prepared$grid
    extent <- prepared$extent

    # Transform date-variable and extract relevant time points
    grid_dates <- as.Date(terra::time(data_sf))

    grid_df <- tibble(link_date = grid_dates) |>
      mutate(
        link_date = link_date - days(time_lag),
        link_date_end = link_date - days(time_span)
      )

    grid_df$time_span_seq <- future_lapply(1:nrow(grid_df), function(i) {
        if (!is.na(grid_df[i,]$link_date) & !is.na(grid_df[i,]$link_date_end)) {
          seq_dates <- seq(grid_df[i,]$link_date_end, grid_df[i,]$link_date, by = "1 day")
          format(seq_dates, "%Y-%m-%d")
        } else {
          NA_character_
        }
      }, future.seed = TRUE)
    years <- as.character(sort(unique(year(unlist(grid_df$time_span_seq)))))
    months <- as.character(sort(unique(month(unlist(grid_df$time_span_seq)))))
    days <- as.character(sort(unique(day(unlist(grid_df$time_span_seq)))))
    p(amount = 1, message = "Preprocessing complete")

    # Download data from API
    focal_path <- .make_request_daily(indicator, catalogue, extent, years,
                                      months, days, path, prefix = "focal",
                                      statistic = statistic, time_zone = time_zone)
    p(amount = 1, message = "Download complete")

    # Load raster file
    raster <- terra::rast(focal_path)

    # Add timestamp to raster file
    raster <- .raster_timestamp(raster, days, months, years, path, focal_path)

    # Check CRS of both datasets and adjust if necessary
    if (!identical(terra::crs(data_sf), terra::crs(raster))) {
      message("Projecting grid data to match the downloaded raster's CRS.")
      data_sf <- terra::project(data_sf, terra::crs(raster))
    }

    # Extract focal values
    raster_values <- .focal_extract_grid(raster,
                                         data_sf,
                                         grid_df,
                                         time_span = time_span,
                                         parallel = parallel,
                                         chunk_size = chunk_size,
                                         method = method
    )

    # Add as attribute to input grid
    data_sf <- c(data_sf, focal_value = raster_values)
    p(amount = 1, message = "Focal extraction complete")

    # Check baseline argument
    # If no baseline requested, transform back to longitude and latitude and final output
    if(isFALSE(baseline)){
      crs_info <- terra::crs(data_sf, describe = TRUE)
      if (is.null(crs_info$code) || crs_info$code != "EPSG:4326") {
        message("Transforming raster data to WGS84 (EPSG:4326).")
        data_sf <- terra::project(data_sf, "EPSG:4326")
      }

      # Remove files if keep_raw = FALSE
      if (!keep_raw) {
        file.remove(focal_path)
        message("Raw file has been removed.")
      } else {
        message("Raw file has been stored at: ", focal_path)
      }
      p(amount = 1, message = "Baseline skipped")
      p(amount = 0)
      return(data_sf)

    } else if (is.vector(baseline) && length(baseline) == 2) {

      # Extract minimum and maximum baseline years
      min_year <- baseline[1]
      max_year <- baseline[2]

      # Translate user specified baseline years into sequence
      min_baseline <- parse_date_time(paste0(min_year, "-01-01"), order="ymd")
      max_baseline <- parse_date_time(paste0(max_year, "-01-01"), order="ymd")
      baseline_years <- seq(min_baseline, max_baseline, by = "1 year")
      baseline_years <- format(baseline_years, "%Y")


      # Download data from API
      baseline_path <- .make_request_daily(indicator, catalogue, extent,
                                           baseline_years, months, days, path,
                                           prefix = "baseline",
                                           statistic = statistic,
                                           time_zone = time_zone)

      # Load data
      baseline_raster <- terra::rast(baseline_path)

      # Add timestamp to raster file
      baseline_raster <- .raster_timestamp(baseline_raster, days, months,
                                           baseline_years, path, baseline_path)

      # Baseline extraction for gridded data:
      if (time_span == 0) {
        # All grid cells share the same link_date.
        unique_date <- unique(grid_df$link_date)
        if (length(unique_date) != 1) {
          stop("For gridded data with time_span == 0, link_date should be unique.")
        }
        # Build a "month-day" identifier from the unique link_date.
        md_target <- paste(month(unique_date), day(unique_date), sep = "-")
        # Get baseline raster dates and their month-day identifiers.
        baseline_dates <- as.Date(terra::time(baseline_raster))
        baseline_md <- paste(month(baseline_dates), day(baseline_dates), sep = "-")
        # Find layers with matching month-day.
        match_idx <- which(baseline_md == md_target)
        if (length(match_idx) == 0) {
          warning("No matching baseline layers found for the target date.")
          return(NA)
        }
        # Average the matched layers.
        baseline_avg <- terra::app(baseline_raster[[match_idx]], mean, na.rm = TRUE)
        # Resample to match the grid.
        baseline_extracted <- terra::resample(baseline_avg, data_sf, method = method)

      } else {
        # For time_span > 0, all grid cells share the same time_span_seq.
        unique_seq <- unique(grid_df$time_span_seq)
        if (length(unique_seq) != 1) {
          stop("For gridded data with time_span > 0, all cells should share the same time_span_seq.")
        }
        seq_dates <- lubridate::ymd(unique_seq[[1]])
        # Build month-day identifiers for the target sequence.
        seq_md <- paste(month(seq_dates), day(seq_dates), sep = "-")
        baseline_dates <- as.Date(terra::time(baseline_raster))
        baseline_md <- paste(month(baseline_dates), day(baseline_dates), sep = "-")
        # Find all layers whose month-day appears in the target sequence.
        match_idx <- which(baseline_md %in% seq_md)
        if (length(match_idx) == 0) {
          warning("No matching baseline layers found for the target time span.")
          return(NA)
        }
        baseline_avg <- terra::app(baseline_raster[[match_idx]], mean, na.rm = TRUE)
        baseline_extracted <- terra::resample(baseline_avg, data_sf, method = method)
      }

      names(baseline_extracted) <- "baseline_value"

      # Append the baseline as a new layer to the grid.
      data_sf <- c(data_sf, baseline_value = baseline_extracted)
      # Calculate the deviation (focal_value minus baseline_value) using raster arithmetic.
      deviation <- data_sf[["focal_value"]] - data_sf[["baseline_value"]]
      names(deviation) <- "deviation"
      data_sf <- c(data_sf, deviation = deviation)

      # Transform the final output back to WGS84
      data_sf <- terra::project(data_sf, "EPSG:4326")
      p(amount = 1, message = "Baseline complete")
      p(amount = 0)

      # Remove files if keep_raw = FALSE
      if (!keep_raw) {
        file.remove(focal_path)
        file.remove(baseline_path)
        message("Raw files have been removed.")
      } else {
        message("Raw files have been stored in: ", path)
      }
      return(data_sf)

    } else {
      stop("Baseline argument must be either FALSE or a vector of length 2 specifying min and max baseline years.")
    }
  })
}
