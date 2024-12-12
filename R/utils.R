# R/utils.R

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
