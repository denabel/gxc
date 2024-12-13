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

# Helper functions for checking allowed catalogues and indicators
.check_valid_catalogue <- function(catalogue) {
  if (!catalogue %in% allowed_catalogues) {
    stop(
      paste0(
        "Invalid 'catalogue' argument. Please choose one of: ",
        paste(allowed_catalogues, collapse = ", ")
      ), call. = FALSE
    )
  }
}

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

# Helper function for making a bbox and create the spatial extent of the dataset
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
