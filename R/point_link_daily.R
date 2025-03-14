#' Link Spatial Points with Copernicus Earth Observation Daily Indicators
#'
#' Downloads and processes Copernicus Earth observation data (ERA5) based on daily temporal parameters,
#' and extracts the relevant daily climate indicator values for the provided spatial points. The function
#' constructs daily time sequences based on a specified date variable and time adjustments (time_lag and time_span),
#' downloads the corresponding daily statistics (e.g., daily mean, maximum, or minimum), and links these values to
#' the spatial points. An optional spatial buffer (in kilometers) can be specified so that instead of a direct cell match,
#' the mean value over the buffer is extracted. If a baseline period is provided (e.g., `baseline = c("1980", "2010")`),
#' baseline daily statistics are downloaded for the specified period and appended as a separate attribute.
#'
#' @param indicator Character string specifying the indicator to download (e.g., "2m_temperature").
#'   Allowed indicators differ by catalogue. See the **Details** section for available indicators.
#' @param data An `sf` object containing the spatial data (polygons or points).
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
#' @param path Character string specifying the directory path where data will be downloaded and/or stored
#'   (default is `"./data/raw"`).
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"derived-era5-land-daily-statistics"` (default) or `"derived-era5-single-levels-daily-statistics"`.
#' @param statistic Character string specifying the type of daily statistic to download.
#'   Options are `"daily_mean"` (default), `"daily_maximum"`, and `"daily_minimum"`.
#' @param time_zone Character string specifying the time zone to use (default is `"utc+00:00"`).
#' @param keep_raw Logical value indicating whether to keep the downloaded raw `.grib` files.
#'   If `FALSE`, the files are deleted after processing (default is `FALSE`).
#' @param parallel Logical indicating whether to use parallel processing with chunking.
#'   Default is `FALSE` (i.e. sequential execution).
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
#' @return An `sf` object with the original spatial data and appended climate indicator values. If a baseline
#' period is specified, additional columns for baseline values and deviations are included.
#'
#' @importFrom sf st_transform st_bbox st_crs st_buffer
#' @importFrom dplyr mutate %>% pull filter arrange
#' @importFrom tidyr expand_grid
#' @importFrom lubridate parse_date_time ymd year month day days make_date
#' @importFrom terra rast extract app time crs writeCDF
#' @importFrom rlang sym
#' @importFrom keyring key_get
#' @importFrom ecmwfr wf_set_key wf_request
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # Create sample point data (sf object)
#' pts <- data.frame(lon = c(13.4, 11.6, 9.9),
#'                   lat = c(52.5, 51.3, 50.1),
#'                   date_column = c("2014-08-01", "2014-08-01", "2014-08-01"))
#' pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
#'
#' # Example: Direct extraction (buffer = 0)
#' result1 <- point_link_daily(
#'   indicator = "2m_temperature",
#'   data = pts_sf,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   buffer = 0,
#'   baseline = FALSE,
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "derived-era5-land-daily-statistics",
#'   statistic = "daily_mean",
#'   time_zone = "utc+00:00",
#'   keep_raw = FALSE
#' )
#'
#' # Example: Aggregated extraction with a 5 km buffer
#' result2 <- point_link_daily(
#'   indicator = "2m_temperature",
#'   data = pts_sf,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   buffer = 5,
#'   baseline = c("1980", "2010"),
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "derived-era5-land-daily-statistics",
#'   statistic = "daily_mean",
#'   time_zone = "utc+00:00",
#'   keep_raw = TRUE
#' )
#'
#' # View the results:
#' head(result1)
#' head(result2)
#' }
#'
#' @export

point_link_daily <- function(
    indicator,
    data,
    date_var,
    time_span = 0,
    time_lag = 0,
    buffer = 0,
    baseline = FALSE,
    order = "ymd",
    path = "./data/raw",
    catalogue = "derived-era5-land-daily-statistics",
    statistic = "daily_mean",
    time_zone = "utc+00:00",
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
    prepared <- .prep_points(data, buffer)
    data_sf <- prepared$data_sf
    extent <- prepared$extent

    # Transform date-variable and extract relevant time points
    data_sf <- data_sf |>
      mutate(
        link_date = parse_date_time(x=!!sym(date_var), orders=order)
      ) |>
      mutate(
        link_date = link_date - days(time_lag)
      ) |>
      mutate(
        link_date_end = link_date - days(time_span)
      )
    data_sf$time_span_seq <- future_lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date) & !is.na(data_sf[i,]$link_date_end)) {
        seq_dates <- seq(data_sf[i,]$link_date_end, data_sf[i,]$link_date, by = "1 day")
        format(seq_dates, "%Y-%m-%d")
      } else{
        NA_character_
      }
    }, future.seed = TRUE)
    years <- as.character(sort(unique(year(unlist(data_sf$time_span_seq)))))
    months <- as.character(sort(unique(month(unlist(data_sf$time_span_seq)))))
    days <- as.character(sort(unique(day(unlist(data_sf$time_span_seq)))))
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
    if(!identical(crs(data_sf), terra::crs(raster))) {
      data_sf <- data_sf |>
        st_transform(crs=st_crs(raster))
    }

    # Extract focal values
    raster_values <- .focal_extract(raster,
                                    focal_path,
                                    data_sf,
                                    time_span = time_span,
                                    parallel = parallel,
                                    chunk_size = chunk_size
    )

    # Create new variable in dataframe
    data_sf$focal_value <- unlist(raster_values)
    p(amount = 1, message = "Focal extraction complete")

    # Check baseline argument
    # If no baseline requested, transform back to longitude and latitude and
    # export final output
    if(isFALSE(baseline)){
      data_sf <- sf::st_transform(data_sf, crs = 4326)

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

      # Translate baseline years into sequence
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

      # Extract values from raster for each observation and add to dataframe
      if(length(unique(data_sf$link_date)) == 1){
        # All observations have the same link date
        baseline_raster <- terra::app(baseline_raster, mean)
        baseline_values <- terra::extract(
          baseline_raster,
          data_sf,
          fun = mean,
          na.rm = TRUE,
          ID = FALSE
        )
      } else {
        # All observations have different link dates and mean calculation of focal months
        if (!parallel) {
          # Sequential approach
          baseline_dates <- as.Date(terra::time(baseline_raster))
          baseline_df <- data.frame(
            layer_index = seq_along(baseline_dates),
            month = month(baseline_dates),
            day = day(baseline_dates)
          )

          baseline_values <- lapply(seq_len(nrow(data_sf)), function(i) {
            if (!is.na(data_sf[i,]$link_date)) {

              target_dates <- as.Date(ymd(unlist(data_sf[i,]$time_span_seq)))
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
                data_sf[i,],
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
          chunks <- split(seq_len(nrow(data_sf)),
                          ceiling(seq_len(nrow(data_sf)) / chunk_size))

          baseline_values_list <- future_lapply(chunks, function(idx) {
            local_baseline_raster <- terra::rast(baseline_path)
            baseline_dates <- as.Date(terra::time(local_baseline_raster))
            baseline_df <- data.frame(
              layer_index = seq_along(baseline_dates),
              month = month(baseline_dates),
              day = day(baseline_dates)
            )

            sapply(idx, function(i) {
              if (!is.na(data_sf[i,]$link_date)) {
                target_dates <- as.Date(ymd(unlist(data_sf[i,]$time_span_seq)))
                target_md <- paste(month(target_dates), day(target_dates), sep = "-")
                baseline_md <- paste(baseline_df$month, baseline_df$day, sep = "-")
                match_idx <- baseline_df$layer_index[baseline_md %in% target_md]

                if (length(match_idx) == 0) return(NA)
                raster_subset <- terra::app(local_baseline_raster[[match_idx]],
                                            fun = mean,
                                            na.rm = TRUE)
                terra::extract(
                  raster_subset,
                  data_sf[i,],
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
      data_sf$baseline_value <- unlist(baseline_values)

      # Calculate absolute deviation between focal and baseline values
      data_sf <- data_sf |>
        mutate(
          deviation = focal_value - baseline_value
        )

      # Transform back to longitude and latitude WGS84
      data_sf <- st_transform(data_sf, crs = 4326)
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
