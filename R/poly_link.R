#' Link Spatial Data with Copernicus Earth observation data
#'
#' Downloads and processes Copernicus Earth observation data based on spatial and temporal parameters,
#' and extracts the relevant indicators for the provided spatial dataset.
#'
#' @param indicator Character string specifying the indicator to download (e.g., "2m_temperature").
#' @param data An `sf` object containing the spatial data (polygons or points).
#' @param date_var Character string specifying the name of the date variable in `data`.
#' @param time_span Integer specifying the time span in months for averaging (default is `0`).
#' @param time_lag Integer specifying the time lag in months to adjust the date variable (default is `0`).
#' @param baseline Logical indicating whether to calculate a baseline (default is `FALSE`).
#' @param min_year Character string specifying the minimum year for the baseline period (required if `baseline = TRUE`).
#' @param max_year Character string specifying the maximum year for the baseline period (required if `baseline = TRUE`).
#' @param order Character string specifying the date format for parsing (default is `"my"`).
#' @param path Character string specifying the path where data will be stored (default is `"./data/raw"`).
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to download ERA5 climate data
#' for a specified indicator and time period. It processes the spatial data to create a bounding box,
#' constructs the necessary time sequences, and extracts the climate data corresponding to each spatial
#' observation. If a baseline is requested, it calculates the deviation from the baseline period.
#'
#' **Note:** Users must have an account with the CDS and have their API key configured for `ecmwfr`.
#'
#' @return An `sf` object with the original spatial data and appended climate indicator values.
#' If `baseline = TRUE`, additional columns for the baseline values and deviations are included.
#'
#' @importFrom sf st_transform st_bbox st_crs
#' @importFrom dplyr mutate %>%
#' @importFrom lubridate parse_date_time months ymd year month
#' @importFrom terra rast extract app time crs
#' @importFrom rlang sym
#' @importFrom keyring key_get
#' @importFrom ecmwfr wf_set_key wf_request
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' library(sf)
#' library(dplyr)
#'
#' # Load spatial data
#' my_data <- st_read("path/to/your/spatial_data.shp")
#'
#' # Use the function to link climate data
#' result_dataset <- poly_link(
#'   indicator = "2m_temperature",
#'   data = my_data,
#'   date_var = "date_column",
#'   time_span = 0,
#'   time_lag = 0,
#'   baseline = FALSE,
#'   min_year = NULL,
#'   max_year = NULL,
#'   order = "ymd",
#'   path = "./data/raw"
#' )
#'
#' # View the results
#' head(result_dataset)
#' }
#' @export

poly_link <- function(
    indicator,
    data,
    date_var,
    time_span = 0,
    time_lag = 0,
    baseline = FALSE,
    min_year,
    max_year,
    order = "my",
    path = "./data/raw"
  ) {

  # Transform data to spatial object with WGS84
  data_sf <- sf::st_transform(data, crs = 4326)

  # Create bounding box
  extent <- .make_bbox(data_sf)

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
  data_sf$time_span_seq <- lapply(1:nrow(data_sf), function(i) {
    if (!is.na(data_sf[i,]$link_date) & !is.na(data_sf[i,]$link_date_end)) {
      seq_dates <- seq(data_sf[i,]$link_date_end, data_sf[i,]$link_date, by = "1 month")
      format(seq_dates, "%Y-%m-%d")
    } else{
      NA_character_
    }
  })
  years <- as.character(sort(unique(year(unlist(data_sf$time_span_seq)))))
  months <- as.character(sort(unique(month(unlist(data_sf$time_span_seq)))))

  # Access to API
  api_key <- key_get("wf_api_key")
  wf_set_key(key = api_key)

  # Specify API request
  focal_request <- list(
    data_format = "grib",
    variable = indicator,
    product_type = "monthly_averaged_reanalysis",
    time = "00:00",
    year = years,
    month = months,
    area = extent,
    dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
    target = paste0(indicator, "_", date_var, ".grib")
  )

  # Download data from C3S
  focal_path <- wf_request(
    request = focal_request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  # Load raster file
  raster <- terra::rast(paste0(path, "/", indicator, "_", date_var, ".grib"))

  # Check CRS of both datasets and adjust if necessary
  if(!identical(crs(data_sf), terra::crs(raster))) {
    data_sf <- data_sf |>
      st_transform(crs=st_crs(raster))
  }

  # Extract values from raster for each observation and add to dataframe
  # Different approaches for different data structures to maximize performance(?)
  if(length(unique(data_sf$link_date)) == 1 & time_span == 0){
    # All observations have the same link date and direct link to focal month
    raster_values <- terra::extract(
      raster,
      data_sf,
      fun = "mean", # for buffer
      ID = FALSE
    )
  } else if (length(unique(data_sf$link_date)) > 1 & time_span == 0){
    # All observations have different link dates and direct link to focal month
    raster_values <- lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date)) {
        raster_value <- terra::extract(
          raster[[as.Date(time(raster))==data_sf[i,]$link_date]],
          data_sf[i,],
          fun = "mean", # for buffer
          ID = FALSE
        )
      } else {
        raster_value <- NA
      }
    })
  } else if (length(unique(data_sf$link_date)) >= 1 & time_span > 0){
    # All observations have different link dates and mean calculation of focal months
    raster_values <- lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date)) {
        raster_subset <- app(
          raster[[as.Date(time(raster)) %in% ymd(unlist(data_sf[i,]$time_span_seq))]], mean)
        raster_value <- terra::extract(
          raster_subset,
          data_sf[i,],
          fun = "mean", # for buffer
          ID = FALSE
        )
      } else {
        raster_value <- NA
      }
    })
  }

  # Create new variable in dataframe
  data_sf$focal_value <- unlist(raster_values)

  if(baseline==FALSE){
    # If no baseline requested, transform back to longitude and latitude and final output
    data_sf <- st_transform(data_sf, crs = 4326)
    return(data_sf)

  } else{

    # Translate user specified baseline years into sequence
    min_baseline <- parse_date_time(paste0(min_year, "-01-01"), order="ymd")
    max_baseline <- parse_date_time(paste0(max_year, "-01-01"), order="ymd")
    baseline_years <- seq(min_baseline, max_baseline, by = "1 year")
    baseline_years <- format(baseline_years, "%Y")

    # Specify API request
    baseline_request <- list(
      data_format = "grib",
      variable = indicator,
      product_type = "monthly_averaged_reanalysis",
      time = "00:00",
      year = baseline_years,
      month = months,
      area = extent,
      dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
      target = paste0(indicator, "_", date_var, "_baseline_", ".grib")
    )

    # Download data from C3S
    baseline_path <- wf_request(
      request = baseline_request,
      transfer = TRUE,
      path = path,
      verbose = FALSE
    )

    # Load data
    baseline_raster <- terra::rast(paste0(path, "/", indicator, "_", date_var,
                                          "_baseline_", ".grib"))

    # Extract values from raster for each observation and add to dataframe
    # Different approaches for different data structures to maximize performance(?)
    if(length(unique(data_sf$link_date)) == 1){
      # All observations have the same link date
      baseline_raster <- app(baseline_raster, mean)
      baseline_values <- terra::extract(
        baseline_raster,
        data_sf,
        fun = "mean", # for buffer
        ID = FALSE
      )
    } else {
      # All observations have different link dates and mean calculation of focal months
      baseline_values <- lapply(1:nrow(data_sf), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          raster_subset <- app(
            baseline_raster[[month(time(baseline_raster)) %in%
                               month(ymd(unlist(data_sf[i,]$time_span_seq)))]], mean)
          baseline_value <- terra::extract(
            raster_subset,
            data_sf[i,],
            fun = "mean", # for buffer
            ID = FALSE
          )
        } else {
          baseline_value <- NA
        }
      })
    }

    # Add variable to dataframe
    data_sf$baseline_value <- unlist(baseline_values)

    # Calculate absolute deviation between focal and baseline values
    data_sf <- data_sf |>
      mutate(
        deviation = focal_value - baseline_value
      )

    # Transform back to longitude and latitude and final output
    data_sf <- st_transform(data_sf, crs = 4326)
    return(data_sf)

  }

}
