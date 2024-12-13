# R/utils.R

# Allowed catalogues and indicators (by catalogue)
allowed_catalogues <- c(
  "reanalysis-era5-land-monthly-means",
  "reanalysis-era5-single-levels-monthly-means"
)

allowed_indicators_by_catalogue <- list(
  "reanalysis-era5-land-monthly-means" = c("2m_temperature"
                                           ),
  "reanalysis-era5-single-levels-monthly-means" = c("2m_temperature"
                                                    )
)

#' @noRd
.check_valid_catalogue <- function(catalogue) {
  # Helper functions for checking allowed catalogues and indicators
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

#' @title Internal helper function to request ERA5 data from C3S
#'
#' @description This function requests ERA5 monthly-averaged reanalysis data for a specified indicator,
#' catalogue, time period, and spatial extent.
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
.make_request <- function(indicator, catalogue, extent, years, months, path, prefix) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp, ".grib")

  request <- list(
    data_format = "grib",
    variable = indicator,
    product_type = "monthly_averaged_reanalysis",
    time = "00:00",
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

