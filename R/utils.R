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
allowed_hours <- sprintf("%02d:00", 0:23)

# Allowed statistic
allowed_statistic <- c("daily_mean",
                       "daily_maximum",
                       "daily_minimum")

# Allowed time-zone
allowed_time_zone <- sprintf("utc%+03d:00", -12:14)

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

#' @noRd
.check_valid_statistic <- function(statistic) {
  if (!statistic %in% allowed_statistic)  {
    stop(
      paste0(
        "Invalid 'statistic' argument. Please choose one of: ",
        paste(allowed_statistic, collapse = ", ")
      ), call. = FALSE
    )
  }
}

#' @noRd
.check_valid_time_zone <- function(time_zone) {
  if (!time_zone %in% allowed_time_zone)  {
    stop(
      paste0(
        "Invalid 'time_zone' argument. Please choose one between utc-12:00 and utc+14:00."
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
                                path, prefix, statistic = "daily_mean",
                                time_zone = "utc+00:00") {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)

  request <- list(
    variable = indicator,
    product_type = "reanalysis",
    year = years,
    month = months,
    day = days,
    daily_statistic = statistic,
    time_zone = time_zone,
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


# Raster processing helpers -----------------------------------------------

#' Process raster file to add timestamp and update the stored file
#'
#' This function takes a SpatRaster object and a set of days, months, and years,
#' constructs a valid date vector, assigns it as the raster's time dimension,
#' writes the raster to a temporary NetCDF file using terra::writeCDF, removes the
#' original file, and renames the temporary file to the original file path.
#'
#' @param raster A SpatRaster object to be processed.
#' @param days Vector of days (numeric or character) used to build the date vector.
#' @param months Vector of months (numeric or character).
#' @param years Vector of years (numeric or character).
#' @param path Directory where the raster file is stored.
#' @param file_path Full file path to the raster file that will be replaced.
#'
#' @return The input SpatRaster with its time dimension updated. On disk, the original
#'         file is replaced by the new file with time information.
.raster_timestamp <- function(raster, days, months, years, path, file_path) {

  # Build a vector of valid date strings
  valid_date_strings <- expand_grid(
    day = as.numeric(days),
    month = as.numeric(months),
    year = as.numeric(years)
  ) |>
    mutate(date = make_date(year, month, day)) |>
    filter(!is.na(date)) |>
    arrange(date) |>
    pull(date) |>
    format("%Y-%m-%d")

  # Assign the date vector as the raster's time dimension
  terra::time(raster) <- as.Date(valid_date_strings)

  # Extract the original variable names and units from the raster
  orig_units <- terra::units(raster)
  orig_varnames <- terra::varnames(raster)

  # Create a temporary file path for the updated raster file
  temp_file <- file.path(path, paste0("temp_", basename(file_path)))

  # Write the raster to the temporary file (NetCDF format)
  terra::writeCDF(raster,
                  filename = temp_file,
                  varname = orig_varnames,
                  unit = orig_units,
                  overwrite = TRUE)

  # Remove the original file and rename the temporary file to the original name
  file.remove(file_path)
  file.rename(temp_file, file_path)

  return(raster)
}

# Data extraction helpers -------------------------------------------------

#' @noRd

# Focal extraction
.focal_extract <- function(raster,
                           focal_path,
                           data_sf,
                           time_span = 0,
                           parallel = FALSE,
                           chunk_size = 50
                           ) {
  # Extract values from raster for each observation and add to dataframe
  if(length(unique(data_sf$link_date)) == 1 & time_span == 0){
    # All observations have the same link date and direct link to focal month
    raster_values <- terra::extract(
      raster,
      data_sf,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else if (length(unique(data_sf$link_date)) > 1 & time_span == 0){
    # All observations have different link dates and direct link to focal month
    if (!parallel) {
      # Sequential approach
      raster_dates <- as.Date(terra::time(raster))
      raster_values <- lapply(seq_len(nrow(data_sf)), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          target_date <- data_sf[i,]$link_date
          layer_index <- which(raster_dates == target_date)
          if (length(layer_index) == 0) return(NA)
          terra::extract(
            raster[[layer_index]],
            data_sf[i,],
            fun = mean,
            na.rm = TRUE,
            ID = FALSE
          )
        } else {
          NA
        }
      })
      raster_values <- unlist(raster_values, recursive = FALSE)
    } else {
      # Parallelization approach
      chunks <- split(seq_len(nrow(data_sf)),
                      ceiling(seq_len(nrow(data_sf)) / chunk_size))

      raster_values_list <- future.apply::future_lapply(chunks, function(idx) {
        local_raster <- terra::rast(focal_path)
        local_dates <- as.Date(terra::time(local_raster))

        sapply(idx, function(i) {
          if (!is.na(data_sf[i,]$link_date)) {
            target_date <- data_sf[i,]$link_date
            layer_index <- which(local_dates == target_date)
            if(length(layer_index) == 0) return(NA)
            terra::extract(
              local_raster[[layer_index]],
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

      raster_values <- unlist(raster_values_list, recursive = FALSE)
    }

  } else if (length(unique(data_sf$link_date)) >= 1 & time_span > 0){
    # All observations have different link dates and mean calculation of focal months
    if (!parallel) {
      # Sequential approach
      raster_dates <- as.Date(terra::time(raster))
      raster_values <- lapply(seq_len(nrow(data_sf)), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          target_dates <- lubridate::ymd(unlist(data_sf[i,]$time_span_seq))
          layer_index <- which(raster_dates %in% target_dates)
          if (length(layer_index) == 0) return(NA)
          raster_subset <- terra::app(raster[[layer_index]], mean)
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
      raster_values <- unlist(raster_values, recursive = FALSE)
    } else {
      # Parallel approach with chunking
      chunks <- split(seq_len(nrow(data_sf)),
                      ceiling(seq_len(nrow(data_sf)) / chunk_size))

      raster_values_list <- future.apply::future_lapply(chunks, function(idx) {
        local_raster <- terra::rast(focal_path)
        local_dates <- as.Date(terra::time(local_raster))

        sapply(idx, function(i) {
          if (!is.na(data_sf[i,]$link_date)) {
            target_dates <- lubridate::ymd(unlist(data_sf[i,]$time_span_seq))
            layer_index <- which(local_dates %in% target_dates)
            if(length(layer_index) == 0) return(NA)
            raster_subset <- terra::app(local_raster[[layer_index]], mean)
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

      raster_values <- unlist(raster_values_list, recursive = FALSE)
    }
  }
}

