#' Creates the bbox of an sf object in order north, west, south, east
#' @param x sf dataframe
#' @returns A vector of length 4
#' @noRd
.get_extent <- function(.data) {
  if (is_sf(.data)) {
    box <- sf::st_bbox(.data)
    c(ceiling(box$ymax), floor(box$xmin), floor(box$ymin), ceiling(box$xmax))
  } else if (is_terra(.data)) {
    box <- terra::ext(.data)
    c(ceiling(box[4]), floor(box[1]), floor(box[3]), ceiling(box[2]))
  }
}


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

  if (!inherits(.data$link_date, "POSIXct")) {
    .data$link_date <- as.POSIXct(.data$link_date)
  }

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
