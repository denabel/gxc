#' Link Spatial Data with Copernicus Earth Observation Indicators
#'
#' Downloads and processes Copernicus Earth observation data (ERA5) based on spatial and temporal parameters,
#' and extracts the relevant indicators for the provided spatial dataset. Compared to previous versions,
#' the set of accessible indicators has been expanded.
#'
#' @param indicator Character string specifying the indicator to download (e.g., "2m_temperature").
#'   Allowed indicators differ by catalogue. See the **Details** section for available indicators.
#' @param data An `sf` object containing the spatial data (polygons or points).
#' @param date_var Character string specifying the name of the date variable in `data`.
#' @param time_span Integer specifying the time span in months for averaging the climate indicator
#'   values prior to linking with the spatial data (default is `0`).
#' @param time_lag Integer specifying the time lag in months to shift the `date_var` backward
#'   (default is `0`).
#' @param baseline Either `FALSE` (default) or a character vector of length 2 specifying the baseline
#'   period in years. For example, `baseline = c("1980", "2010")` uses the years 1980 to 2010 as the baseline.
#'   If `FALSE`, no baseline calculation is performed.
#' @param order Character string specifying the date format for parsing `date_var`
#'   (default is `"my"` for month-year format).
#' @param path Character string specifying the directory path where data will be downloaded and/or stored
#'   (default is `"./data/raw"`).
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"reanalysis-era5-land-monthly-means"` (default, higher resolution but lower temporal
#'   coverage)
#'   or `"reanalysis-era5-single-levels-monthly-means"` (lower resolution but larger temporal coverage).
#' @param by_hour Logical or character. If `FALSE` (default), the monthly averaged values are derived
#'   from the entire day (`"monthly_averaged_reanalysis"`). If a character string specifying an hour (e.g., `"03:00"`),
#'   then the dataset `"monthly_averaged_reanalysis_by_hour_of_day"` is used, and only values from that hour of the day
#'   are included.
#' @param keep_raw Logical value indicating whether to keep the downloaded raw `.grib` files.
#'   If `FALSE`, the files are deleted after processing (default is `FALSE`).
#' @param parallel Logical indicating whether to use parallel processing with chunking.
#'   Default is `FALSE` (i.e. sequential execution).
#' @param chunk_size Integer specifying the number of observations per chunk when parallelizing.
#'   Default is `50`.
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to download ERA5 monthly-averaged
#' reanalysis data for a specified climate indicator and time period. It processes the spatial data to determine
#' the geographic extent and constructs time sequences based on the provided `date_var`, `time_span`, and `time_lag`.
#'
#' **Note:** Users must have a CDS account and have their API key configured for `ecmwfr`.
#'
#' The function now supports an extended set of indicators. The allowed indicators depend on the chosen `catalogue`:
#'
#' **For `"reanalysis-era5-land-monthly-means"`** (higher resolution, from 1950 onwards):
#' - `"2m_temperature"`
#' - `"total_precipitation"`
#' - `"10m_u_component_of_wind"`
#' - `"10m_v_component_of_wind"`
#' - `"leaf_area_index_high_vegetation"`
#' - `"leaf_area_index_low_vegetation"`
#' - `"snowfall"`
#'
#' **For `"reanalysis-era5-single-levels-monthly-means"`** (lower resolution, from 1940 onwards):
#' - `"2m_temperature"`
#' - `"10m_u_component_of_wind"`
#' - `"10m_v_component_of_wind"`
#' - `"10m_wind_speed"`
#' - `"total_cloud_cover"`
#' - `"leaf_area_index_high_vegetation"`
#' - `"leaf_area_index_low_vegetation"`
#'
#' If a baseline period is provided (e.g. `baseline = c("1980", "2010")`), it calculates and appends baseline
#' values for comparison, as well as the deviation from the baseline. Without a baseline, only the focal climate
#' indicator values are appended.
#'
#' If `keep_raw = TRUE`, the original `.grib` files downloaded from the CDS are retained in the specified `path`.
#' If `keep_raw = FALSE`, these files are removed after processing, saving storage space.
#'
#' The default catalogue `"reanalysis-era5-land-monthly-means"` provides higher spatial resolution at 0.1x0.1 degrees
#' but is only available from 1950 onwards. If you need data before 1950 or if you are working with large
#' spatial extents where finer resolution is not required, you can switch to
#' `"reanalysis-era5-single-levels-monthly-means"`. This dataset provides a spatial resolution of 0.25x0.25 degrees and
#' a temporal coverage until 1940.
#'
#' If `by_hour` is specified as an hour (e.g. `"03:00"`), monthly averages for that specific hour of the day are accessed.
#' This can be useful if you are interested in intra-day climate patterns over long periods such as heat or cold during
#' day or nighttime. The time is specified as UTC.
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
#' @importFrom sf st_transform st_bbox st_crs
#' @importFrom dplyr mutate %>%
#' @importFrom lubridate parse_date_time ymd year month
#' @importFrom terra rast extract app time crs
#' @importFrom rlang sym
#' @importFrom keyring key_get
#' @importFrom ecmwfr wf_set_key wf_request
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # Example 1: Using the default (sequential) mode with no baseline and time_span = 0
#' result1 <- poly_link_monthly(
#'   indicator = "2m_temperature",
#'   data = my_data,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = FALSE,
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "reanalysis-era5-land-monthly-means",
#'   by_hour = FALSE,
#'   keep_raw = FALSE
#' )
#'
#' # Example 2: Sequential mode with a baseline period specified
#' result2 <- poly_link_monthly(
#'   indicator = "2m_temperature",
#'   data = my_data,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = c("1980", "2010"),
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "reanalysis-era5-land-monthly-means",
#'   by_hour = FALSE,
#'   keep_raw = TRUE
#' )
#'
#' # Example 3: Sequential mode with by_hour specified (e.g., "03:00")
#' result3 <- poly_link_monthly(
#'   indicator = "2m_temperature",
#'   data = my_data,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = FALSE,
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "reanalysis-era5-land-monthly-means",
#'   by_hour = "03:00",
#'   keep_raw = FALSE
#' )
#'
#' # Example 4: Using parallel processing with chunking
#' # (This example uses a parallel plan set externally.)
#' library(future)
#' plan(multisession, workers = 6)
#'
#' result4 <- poly_link_monthly(
#'   indicator = "2m_temperature",
#'   data = my_data,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = c("1980", "2010"),
#'   order = "ymd",
#'   path = "./data/raw",
#'   catalogue = "reanalysis-era5-land-monthly-means",
#'   by_hour = FALSE,
#'   keep_raw = TRUE,
#'   parallel = TRUE,
#'   chunk_size = 50
#' )
#'
#' # View the results:
#' head(result1)
#' head(result2)
#' head(result3)
#' head(result4)
#' }
#'
#' @export

poly_link_monthly <- function(
  indicator,
  data,
  date_var,
  time_span = 0,
  time_lag = 0,
  baseline = FALSE,
  order = "my",
  path = "./data/raw",
  catalogue = "reanalysis-era5-land-monthly-means",
  by_hour = FALSE,
  keep_raw = FALSE,
  parallel = FALSE,
  chunk_size = 50
  ) {
  with_progress({
    p <- progressor(steps = 4)

    # Validate catalogue and indicator choice
    .check_valid_catalogue(catalogue, temp_res = "monthly")
    .check_valid_indicator(indicator, catalogue)
    .check_valid_by_hour(by_hour)

    # Access to API
    api_key <- key_get("wf_api_key")
    wf_set_key(key = api_key)

    # Prep data and create extent
    prepared <- .prep_poly(data)
    data_sf <- prepared$data_sf
    extent <- prepared$extent

    # Transform date-variable and extract relevant time points
    data_sf <- data_sf |>
      mutate(
        link_date = parse_date_time(x=!!sym(date_var), orders=order)
      ) |>
      mutate(
        link_date = link_date - months(time_lag)
      ) |>
      mutate(
        link_date_end = link_date - months(time_span)
      )
    data_sf$time_span_seq <- future_lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date) & !is.na(data_sf[i,]$link_date_end)) {
        seq_dates <- seq(data_sf[i,]$link_date_end, data_sf[i,]$link_date, by = "1 month")
        format(seq_dates, "%Y-%m-%d")
      } else{
        NA_character_
      }
    }, future.seed = TRUE)
    years <- as.character(sort(unique(year(unlist(data_sf$time_span_seq)))))
    months <- as.character(sort(unique(month(unlist(data_sf$time_span_seq)))))
    p(amount = 1, message = "Preprocessing complete")

    # Average across entire day or by hour
    if (isFALSE(by_hour)) {
      product_type <- "monthly_averaged_reanalysis"
      request_time <- "00:00"
    } else {
      product_type <- "monthly_averaged_reanalysis_by_hour_of_day"
      request_time <- by_hour
    }

    # Download data from API
    focal_path <- .make_request_monthly(indicator, catalogue, extent, years,
                                        months, path, prefix = "focal",
                                        product_type, request_time)
    p(amount = 1, message = "Download complete")

    # Load raster file
    raster <- terra::rast(focal_path)

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
    # If no baseline requested, transform back to longitude and latitude and final output
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

      # Translate user specified baseline years into sequence
      min_baseline <- parse_date_time(paste0(min_year, "-01-01"), order="ymd")
      max_baseline <- parse_date_time(paste0(max_year, "-01-01"), order="ymd")
      baseline_years <- seq(min_baseline, max_baseline, by = "1 year")
      baseline_years <- format(baseline_years, "%Y")


      # Download data from API
      baseline_path <- .make_request(indicator, catalogue, extent, baseline_years,
                                     months, path, prefix = "baseline", product_type,
                                     request_time)

      # Load data
      baseline_raster <- terra::rast(baseline_path)

      # Extract values from raster for each observation and add to dataframe
      if(length(unique(data_sf$link_date)) == 1){
        # All observations have the same link date
        baseline_raster <- app(baseline_raster, mean)
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
          baseline_months <- month(time(baseline_raster))
          baseline_values <- lapply(seq_len(nrow(data_sf)), function(i) {
            if (!is.na(data_sf[i,]$link_date)) {
              target_months <- month(ymd(unlist(data_sf[i,]$time_span_seq)))
              layer_index <- which(baseline_months %in% target_months)
              if (length(layer_index) == 0) return(NA)
              raster_subset <- app(baseline_raster[[layer_index]], mean)
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
            baseline_months <- month(time(local_baseline_raster))

            sapply(idx, function(i) {
              if (!is.na(data_sf[i,]$link_date)) {
                target_months <- month(ymd(unlist(data_sf[i,]$time_span_seq)))
                layer_index <- which(baseline_months %in% target_months)
                if (length(layer_index) == 0) return(NA)
                raster_subset <- app(local_baseline_raster[[layer_index]], mean)
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
