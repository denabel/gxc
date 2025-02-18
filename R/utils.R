# R/utils.R


# Valid input checks ------------------------------------------------------

# Allowed catalogues
allowed_catalogues_monthly <- c(
  "reanalysis-era5-land-monthly-means",
  "reanalysis-era5-single-levels-monthly-means"
)

allowed_catalogues_daily <- c(
  "derived-era5-land-daily-statistics",
  "derived-era5-single-levels-daily-statistics"
)

# Allowed indicators
allowed_indicators_by_catalogue <- list(
  "reanalysis-era5-land-monthly-means" = c("2m_temperature",
                                           "total_precipitation",
                                           "10m_u_component_of_wind",
                                           "10m_v_component_of_wind",
                                           #"10m_wind_speed", # does not exist in catalogue
                                           "leaf_area_index_high_vegetation",
                                           "leaf_area_index_low_vegetation",
                                           #"snow_cover", # time stamp first day of NEXT month, if by_hour, then LAST day of focal month
                                           "snowfall"
  ),
  "reanalysis-era5-single-levels-monthly-means" = c("2m_temperature",
                                                    #"total_precipitation", # time stamp last day of previous month, if by_hour, time stamp one hour earlier
                                                    "10m_u_component_of_wind",
                                                    "10m_v_component_of_wind",
                                                    "10m_wind_speed",
                                                    #"instantaneous_10m_wind_gust", # time stamp last day of previous month, but if by_hour, time stamp correct
                                                    #"downward_uv_radiation_at_the_surface", # time stamp last day of previous month, if by_hour, time stamp one hour earlier
                                                    "total_cloud_cover",
                                                    #"k_index", # Time stamp LAST day of focal month (e.g. 2014-08-31) when requested August 2014
                                                    "leaf_area_index_high_vegetation",
                                                    "leaf_area_index_low_vegetation"
                                                    #"snowfall" # time stamp last day of previous month
  ),
  "derived-era5-land-daily-statistics" = c("2m_temperature"
                                           #"snow_cover",
                                           #"10m_u_component_of_wind",
                                           #"10m_v_component_of_wind",
                                           #"leaf_area_index_high_vegetation",
                                           #"leaf_area_index_low_vegetation"
  ),
  "derived-era5-single-levels-daily-statistics" = c("2m_temperature"
                                                    #"total_precipitation",
                                                    #"10m_u_component_of_wind",
                                                    #"10m_v_component_of_wind",
                                                    #"10m_wind_speed",
                                                    #"instantaneous_10m_wind_gust",
                                                    #"downward_uv_radiation_at_the_surface",
                                                    #"total_cloud_cover",
                                                    #"k_index",
                                                    #"leaf_area_index_high_vegetation",
                                                    #"leaf_area_index_low_vegetation"
                                                    #"snowfall"
  )
)


# Allowed input hours
allowed_hours <- sprintf("%02d:00", 0:23)  # "00:00", "01:00", ..., "23:00"

# Check allowed values functions
#' @noRd
.check_valid_catalogue <- function(catalogue, temp_res = "monthly") {
  allowed_catalogues <- if(temp_res == "monthly") {
    allowed_catalogues_monthly
  } else if(temp_res == "daily") {
    allowed_catalogues_daily
  } else {
    stop("Invalid temporal resolution. Choose either 'monthly' or 'daily'.")
  }

  if (!catalogue %in% allowed_catalogues) {
    stop(
      paste0(
        "Invalid 'catalogue' argument. Please choose one of: ",
        paste(allowed_catalogues, collapse = ", ")
      ), call. = FALSE
    )
  }
}

#' @noRd
.check_valid_indicator <- function(indicator, catalogue) {
  # Get the allowed indicators for the given catalogue
  indicators_for_catalogue <- allowed_indicators_by_catalogue[[catalogue]]

  if (!indicator %in% indicators_for_catalogue) {
    stop(
      paste0(
        "Invalid 'indicator' for the chosen catalogue '", catalogue, "'.\n",
        "Please choose one of: ", paste(indicators_for_catalogue, collapse = ", ")
      ), call. = FALSE
    )
  }
}

#' @noRd
.check_valid_by_hour <- function(by_hour) {
  # Allowed by_hour values: FALSE or one of the allowed_hours vector
  if (!isFALSE(by_hour) && !by_hour %in% allowed_hours) {
    stop(
      paste0(
        "Invalid 'by_hour' argument. Please choose either FALSE or one of: ",
        paste(allowed_hours, collapse = ", ")
      ), call. = FALSE
    )
  }
}


# Spatial processing of input ---------------------------------------------

#' @title Helper function for making a bbox and create the spatial extent of the dataset
#' @noRd
.prep_poly <- function(data) {

  # Check sf
  if (!inherits(data, "sf")) {
    stop("Data must be a sf object.")
  }

  # Check CRS and transform to WGS84 if necessary
  if (sf::st_crs(data)$epsg != 4326) {
    message("Transforming data to WGS84 (EPSG:4326).")
    data <- sf::st_transform(data, crs = 4326)
  }

  # Extract bounding box
  box <- sf::st_bbox(data)

  # Create extent in order: north, west, south, east
  extent <- c(
    ceiling(box$ymax),
    floor(box$xmin),
    floor(box$ymin),
    ceiling(box$xmax)
  )

  # Return data and extent
  list(data_sf = data, extent = extent)
}


# Data retrieval helpers --------------------------------------------------

#' @title Internal helper function to request ERA5 monthly data from C3S
#'
#' @description This function requests ERA5 monthly-averaged reanalysis data for
#' a specified indicator, catalogue, time period, and spatial extent.
#'
#' @param indicator Character string specifying the indicator to download.
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#' @param extent Numeric vector specifying the bounding box area (N,W,S,E).
#' @param years Character vector of years for which data should be retrieved.
#' @param months Character vector of months for which data should be retrieved.
#' @param path Character string specifying the directory path where data will be stored.
#' @param prefix Character string specifying a prefix for the target filename (e.g., "focal" or "baseline").
#'
#' @return A character string with the path to the downloaded file.
#'
#' @importFrom ecmwfr wf_request
#' @noRd
.make_request_monthly <- function(indicator, catalogue, extent, years, months,
                                  path, prefix,
                                  product_type = "monthly_averaged_reanalysis",
                                  request_time = "00:00") {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp, ".grib")

  request <- list(
    data_format = "grib",
    download_format = "unarchived",
    variable = indicator,
    product_type = product_type,
    time = request_time,
    year = years,
    month = months,
    area = extent,
    dataset_short_name = catalogue,
    target = file_name
  )

  file_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  return(file_path)
}


#' @title Internal helper function to request ERA5 daily data from C3S
#'
#' @description This function requests ERA5 daily-averaged reanalysis data for
#' a specified indicator, catalogue, time period, and spatial extent.
#'
#' @param indicator Character string specifying the indicator to download.
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#' @param extent Numeric vector specifying the bounding box area (N,W,S,E).
#' @param years Character vector of years for which data should be retrieved.
#' @param months Character vector of months for which data should be retrieved.
#' @param days Character vector of days for which data should be retrieved.
#' @param path Character string specifying the directory path where data will be stored.
#' @param prefix Character string specifying a prefix for the target filename (e.g., "focal" or "baseline").
#'
#' @return A character string with the path to the downloaded file.
#'
#' @importFrom ecmwfr wf_request
#' @noRd
.make_request_daily <- function(indicator, catalogue, extent, years, months, days,
                                path, prefix) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp, ".nc.zip")

  request <- list(
    variable = indicator,
    product_type = "reanalysis",
    year = years,
    month = months,
    day = days,
    daily_statistic = "mean",
    time_zone = "utc+00:00",
    frequency = "1_hourly",
    area = extent,
    dataset_short_name = catalogue,
    target = file_name
  )

  file_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  return(file_path)
}
