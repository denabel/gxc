#' Creates the bbox of an sf object in order north, west, south, east
#' @param x sf dataframe
#' @returns A vector of length 4
#' @noRd
.get_extent <- function(.data, grid_resolution = 0.1) {
  .snap_to_grid <- function(x, resolution, direction = c("floor", "ceiling")) {
    direction <- match.arg(direction)
    if (direction == "floor") floor(x / resolution) * resolution
    else ceiling(x / resolution) * resolution
  }

  if (is_sf(.data)) {
    box <- sf::st_bbox(.data)
    ymax <- .snap_to_grid(box$ymax, grid_resolution, "ceiling")
    xmin <- .snap_to_grid(box$xmin, grid_resolution, "floor")
    ymin <- .snap_to_grid(box$ymin, grid_resolution, "floor")
    xmax <- .snap_to_grid(box$xmax, grid_resolution, "ceiling")

  } else if (is_terra(.data)) {
    box <- terra::ext(.data)
    ymax <- .snap_to_grid(box[4], grid_resolution, "ceiling")
    xmin <- .snap_to_grid(box[1], grid_resolution, "floor")
    ymin <- .snap_to_grid(box[3], grid_resolution, "floor")
    xmax <- .snap_to_grid(box[2], grid_resolution, "ceiling")
  }

  # point case: bbox has no extent → one grid step in each direction
  if (xmin >= xmax) { xmin <- xmin - grid_resolution; xmax <- xmax + grid_resolution }
  if (ymin >= ymax) { ymin <- ymin - grid_resolution; ymax <- ymax + grid_resolution }

  c(ymax, xmin, ymin, xmax)
}

# .get_extent <- function(.data) {
#   if (is_sf(.data)) {
#     box <- sf::st_bbox(.data)
#     c(ceiling(box$ymax), floor(box$xmin), floor(box$ymin), ceiling(box$xmax))
#   } else if (is_terra(.data)) {
#     box <- terra::ext(.data)
#     c(ceiling(box[4]), floor(box[1]), floor(box[3]), ceiling(box[2]))
#   }
# }


.transform_time <- function(.data,
                            date_var = "date",
                            time_span = 0,
                            time_lag = 0,
                            by = "1 day") {
  if (is_sf(.data)) {
    .data$link_date <- .data[[date_var]]
  } else if (is_terra(.data)) {
    .data <- data.frame(link_date = terra::time(.data))
  }

  .data$link_date <- as_date(.data$link_date)
  .data$link_date <- .data$link_date - days(time_lag)
  .data$link_date_end <- .data$link_date - days(time_span)
  .data$time_span_seq <- Map(
    .data$link_date_end,
    .data$link_date,
    f = function(end, start) {
      format(seq(end, start, by = by), "%Y-%m-%d")
    }
  )

  .data
}


.align_crs_raster <- function(x, y) {
  if (!identical(terra::crs(x), terra::crs(y))) {
    info("Reprojecting indicator raster to match input data", level = "warning")
    y <- terra::project(y, y = x)
  }
  y
}


.align_crs_vector <- function(x, y) {
  if (!identical(terra::crs(x), terra::crs(y))) {
    x <- sf::st_transform(x, crs = terra::crs(y))
  }
  x
}
