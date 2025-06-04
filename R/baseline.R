add_baseline <- function(.data, baseline) {
  lnk <- make_lnk(.data, baseline = baseline)
  check_lnk(lnk, "baseline")
  .add_baseline(
    .data,
    baseline = baseline,
    indicator = lnk %>>% "indicator",
    days = lnk %>>% "days",
    months = lnk %>>% "months",
    extent = lnk %>>% "extent",
    catalogue = lnk %>>% "catalogue",
    statistic = lnk %>>% "statistic",
    time_zone = lnk %>>% "time_zone",
    cache = lnk %>>% "cache",
    path = lnk %>>% "path",
    parallel = lnk %>>% "parallel",
    chunk_size = lnk %>>% "chunk_size",
    verbose = lnk %>>% "verbose"
  )
}


.add_baseline <- function(.data,
                          baseline,
                          indicator,
                          days,
                          months,
                          extent,
                          ...,
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
  baseline_raster <- raster_timestamp(
    baseline_raster,
    days = days,
    months = months,
    years = years
  )

  # Extract values from raster for each observation and add to dataframe
  if (is_sf(.data)) {
    baseline <- .toi_extract_baseline(
      .data,
      baseline_raster,
      baseline_path,
      ...,
      parallel = parallel,
      chunk_size = chunk_size
    )

    .data$.baseline <- unlist(baseline)
    .data$.deviation <- .data$.linked - .data$.baseline
  } else {
    baseline <- .toi_extract_grid_baseline(
      .data,
      baseline_raster,
      baseline_path,
      ...,
      parallel = parallel,
      chunk_size = chunk_size
    )

    names(baseline) <- ".baseline"
    .data <- c(.data, baseline)
    deviation <- .data[[".linked"]] - .data[[".baseline"]]
    names(deviation) <- ".deviation"
    .data <- c(.data, deviation)
  }

  .data
}
