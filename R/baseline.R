add_baseline <- function(.data, baseline) {
  lnk <- make_lnk(.data, baseline = baseline)
  check_lnk(lnk, "baseline")

  request_args <- list(
    indicator = lnk %>>% "indicator",
    days = lnk %>>% "days",
    months = lnk %>>% "months",
    extent = lnk %>>% "extent",
    catalogue = lnk %>>% "catalogue",
    statistic = lnk %>>% "statistic",
    time_zone = lnk %>>% "time_zone"
  )

  .add_baseline(
    .data,
    baseline = baseline,
    request_args = request_args,
    requester = lnk %>>% "requester",
    cache = lnk %>>% "cache",
    path = lnk %>>% "path",
    parallel = lnk %>>% "parallel",
    chunk_size = lnk %>>% "chunk_size",
    verbose = lnk %>>% "verbose"
  )
}


.add_baseline <- function(.data,
                          baseline,
                          requester,
                          request_args,
                          ...,
                          cache = TRUE,
                          path = NULL,
                          parallel = FALSE,
                          chunk_size = 50,
                          verbose = TRUE) {
  min_year <- baseline[1]
  max_year <- baseline[2]
  dates <- make_dates(seq(min_year, max_year), months = 1, days = 1)
  years <- format(dates, "%Y")
  request_args$years <- years

  request_args <- c(
    request_args,
    cache = cache,
    path = path,
    prefix = "baseline",
    verbose = verbose
  )
  path <- do.call(requester, request_args)
  raster <- terra::rast(path)

  # Add timestamp to raster file
  if (!inherits(terra::time(raster), "POSIXt")) {
    months <- request_args$months
    days <- request_args$days

    raster <- raster_timestamp(
      raster,
      days = days,
      months = months,
      years = years
    )
  }

  # Extract values from raster for each observation and add to dataframe
  if (is_sf(.data)) {
    baseline <- .toi_extract_baseline(
      .data,
      raster,
      path,
      ...,
      parallel = parallel,
      chunk_size = chunk_size
    )

    .data$.baseline <- unlist(baseline)
    .data$.deviation <- .data$.linked - .data$.baseline
  } else {
    baseline <- .toi_extract_grid_baseline(
      .data,
      raster,
      path,
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
