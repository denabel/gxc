# R/utils.R

# Helper function for making a bbox and create the spatial extent of the dataset
.make_bbox <- function(data_sf) {
  # Extract bounding box
  box <- sf::st_bbox(data_sf)

  # Create extent in order: north, west, south, east
  extent <- c(
    ceiling(box$ymax),
    floor(box$xmin),
    floor(box$ymin),
    ceiling(box$xmax)
  )

  return(extent)
}
