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
    c(ceiling(e[4]), floor(e[1]), floor(e[3]), ceiling(e[2]))
  }
}


.transform_time <- function(.data,
                            date_var = "date",
                            time_span = 0,
                            time_lag = 0) {
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
      format(seq(end, start, by = "1 day"), "%Y-%m-%d")
    }
  )

  .data
}


.add_baseline <- function(.data,
                          baseline,
                          indicator,
                          days,
                          months,
                          extent,
                          catalogue = "derived-era5-land-daily-statistics",
                          statistic = "daily_mean",
                          time_zone = "utc+00:00",
                          cache = TRUE,
                          path = NULL,
                          parallel = FALSE,
                          chunk_size = 50,
                          verbose = TRUE) {
  min_year <- baseline[1]
  max_year <- baseline[2]
  years <- make_dates(seq(min_year, max_year), months = 1, days = 1)
  years <- format(years, "%Y")

  baseline_path <- .request_daily_temp(
    indicator,
    catalogue,
    extent,
    years,
    months,
    days,
    cache = cache,
    path = path,
    prefix = "baseline",
    statistic = statistic,
    time_zone = time_zone,
    verbose = verbose
  )

  baseline_raster <- terra::rast(baseline_path)

  # Add timestamp to raster file
  baseline_raster <- .raster_timestamp(
    baseline_raster,
    days = days,
    months = months,
    years = years,
    path = path
  )

  # Extract values from raster for each observation and add to dataframe
  baseline_values <- if (length(unique(.data$link_date)) == 1) {
    # All observations have the same link date
    baseline_raster <- terra::app(baseline_raster, mean)
    terra::extract(
      baseline_raster,
      .data,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else {
    # All observations have different link dates and mean calculation of months
    if (!parallel) {
      .toi_extract_impl(baseline_raster, .data, baseline = TRUE)
    } else {
      chunks <- split(
        seq_len(nrow(.data)),
        ceiling(seq_len(nrow(.data)) / chunk_size)
      )

      future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          baseline_path,
          .data,
          idx = chunk,
          baseline = TRUE
        ),
        future.seed = TRUEe
      )
    }
  }

  .data$.baseline <- unlist(baseline_values)
  .data$.deviation <- .data$link_value - .data$baseline_value
  .data
}
