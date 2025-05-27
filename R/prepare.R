#' Checks if data is an sf dataframe, builds buffers and transforms to 4326.
#' @param .data sf dataframe
#' @param buffer Buffer radius
#' @returns sf dataframe
#' @noRd
.prep_points <- function(.data, buffer = 0) {
  .check_class(.data, "sf")

  if (buffer > 0) {
    .data <- sf::st_buffer(.data, dist = buffer * 1000)
  }

  sf::st_transform(.data, crs = 4326)
}


#' Creates the bbox of an sf object in order north, west, south, east
#' @param x sf dataframe
#' @returns A vector of length 4
#' @noRd
.get_extent <- function(.data) {
  box <- sf::st_bbox(.data)
  c(ceiling(box$ymax), floor(box$xmin), floor(box$ymin), ceiling(box$xmax))
}


.add_timelag <- function(.data, date_var = "date", time_span = 0, time_lag = 0) {
  .data$link_date <- .data[[date_var]]
  if (!inherits(.data$link_date, "POSIXct")) {
    .data$link_date <- as.POSIXct(.data$link_date)
  }

  .data$link_date <- .data$link_date - days(time_lag)
  .data$link_date_end <- .data$link_date - days(time_span)

  .data$time_span_seq <- Map(
    .data$link_date_end,
    .data$link_date,
    f = function(end, start) {
      format(seq(end, start, by = "1 day"), "%Y-%m-%d")
    }
  )

  .data
}
