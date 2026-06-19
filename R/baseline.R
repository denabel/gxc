.baseline_funs <- list(
  mean   = function(x) mean(x,   na.rm = TRUE),
  median = function(x) median(x, na.rm = TRUE),
  min    = function(x) min(x,    na.rm = TRUE),
  max    = function(x) max(x,    na.rm = TRUE),
  sd     = function(x) sd(x,     na.rm = TRUE),
  p05    = function(x) quantile(x, 0.05, na.rm = TRUE),
  p10    = function(x) quantile(x, 0.10, na.rm = TRUE),
  p20    = function(x) quantile(x, 0.20, na.rm = TRUE),
  p80    = function(x) quantile(x, 0.80, na.rm = TRUE),
  p90    = function(x) quantile(x, 0.90, na.rm = TRUE),
  p95    = function(x) quantile(x, 0.95, na.rm = TRUE)
)

.resolve_baseline_fun <- function(x, arg = "baseline_fun") {
  if (is.function(x)) return(list(fun = x, name = "custom"))
  x <- match.arg(x, choices = names(.baseline_funs))
  list(fun = .baseline_funs[[x]], name = x)
}

.result_unit <- function(stat_wrangling, indicator) {
  switch(stat_wrangling,
         deviation    = indicator,
         sd_deviation = "sd",
         count_above  = "days_above",
         count_below  = "days_below"
  )
}

.col <- function(name, prefix = NULL) {
  if (is.null(prefix)) paste0(".", name) else paste0(".", name, "_", prefix)
}

compute_stat_wrangling <- function(
    baseline_values,
    focal_value,
    stat_wrangling = c("deviation", "sd_deviation", "count_above", "count_below"),
    baseline_fun   = function(x) mean(x, na.rm = TRUE)
) {
  stat_wrangling <- match.arg(stat_wrangling)
  reference_stat <- baseline_fun(baseline_values)

  result <- switch(stat_wrangling,
                   deviation    = focal_value - reference_stat,
                   sd_deviation = (focal_value - reference_stat) / sd(baseline_values, na.rm = TRUE),
                   count_above  = sum(focal_value > reference_stat, na.rm = TRUE),
                   count_below  = sum(focal_value < reference_stat, na.rm = TRUE)
  )

  list(reference_stat = reference_stat, result = result)
}

add_baseline <- function(.data, baseline, baseline_fun) {
  lnk <- .make_lnk(.data, baseline = baseline)
  .check_lnk(lnk, "baseline")

  request_args <- list(
    indicator = lnk %>>% "indicator",
    days      = lnk %>>% "days",
    months    = lnk %>>% "months",
    extent    = lnk %>>% "extent",
    catalogue = lnk %>>% "catalogue",
    statistic = lnk %>>% "statistic",
    time_zone = lnk %>>% "time_zone"
  )

  .add_baseline(
    .data,
    baseline     = baseline,
    baseline_fun = baseline_fun,
    request_args = request_args,
    requester    = lnk %>>% "requester",
    cache        = lnk %>>% "cache",
    path         = lnk %>>% "path",
    parallel     = lnk %>>% "parallel",
    chunk_size   = lnk %>>% "chunk_size",
    verbose      = lnk %>>% "verbose"
  )
}

.add_baseline <- function(.data,
                          baseline,
                          baseline_fun,
                          baseline_fun_name,
                          indicator,
                          ...,
                          focal_values    = NULL,
                          stat_wrangling  = "deviation",
                          prefix          = NULL,
                          obs_raster      = NULL,
                          baseline_raster = NULL,
                          cache           = TRUE,
                          path            = NULL,
                          parallel        = FALSE,
                          chunk_size      = 50,
                          verbose         = TRUE) {

  # If baseline_raster is not provided, fall back to request-based approach
  # (used by add_baseline public function)
  if (is.null(baseline_raster)) {
    min_year <- baseline[1]
    max_year <- baseline[2]
    dates    <- make_dates(seq(min_year, max_year), months = 1, days = 1)
    years    <- format(dates, "%Y")

    focal_span    <- sort(unique(as_date(unlist(.data$time_span_seq))))
    baseline_span <- sort(unique(as_date(unlist(lapply(years, function(y) {
      as.Date(paste(y, format(focal_span, "%m-%d"), sep = "-"))
    })))))

    request_args <- list(
      indicator = indicator,
      catalogue = list(...)$catalogue,
      extent    = list(...)$extent %||% .get_extent(.data),
      years     = format(baseline_span, "%Y"),
      months    = format(baseline_span, "%m"),
      days      = format(baseline_span, "%d"),
      statistic = list(...)$statistic,
      time_zone = list(...)$time_zone,
      cache     = cache,
      path      = path,
      prefix    = "baseline",
      verbose   = verbose
    )

    baseline_path   <- do.call(.request_era5_daily, request_args)
    baseline_raster <- terra::rast(baseline_path)
    baseline_raster <- .align_crs_raster(.data, baseline_raster)

    if (!inherits(terra::time(baseline_raster), "POSIXt")) {
      baseline_raster <- raster_timestamp(
        baseline_raster,
        days   = format(baseline_span, "%d"),
        months = format(baseline_span, "%m"),
        years  = format(baseline_span, "%Y"),
        span   = baseline_span
      )
    }
  }

  if (is_sf(.data)) {
    baseline_result <- .toi_extract_baseline(
      .data,
      baseline_raster,
      ...,
      parallel       = parallel,
      chunk_size     = chunk_size,
      baseline_fun   = baseline_fun,
      stat_wrangling = stat_wrangling,
      focal_values   = focal_values
    )

    .data[[.col("baseline", prefix)]] <-
      sapply(baseline_result, `[[`, "reference_stat")
    .data[[.col("result", prefix)]] <-
      sapply(baseline_result, `[[`, "result")

  } else {
    baseline_result <- .toi_extract_grid_baseline(
      .data, baseline_raster, ...,
      parallel   = parallel,
      chunk_size = chunk_size
    )
    names(baseline_result) <- .col("baseline", prefix)
    .data <- c(.data, baseline_result)
    deviation        <- .data[[".linked"]] - .data[[.col("baseline", prefix)]]
    names(deviation) <- .col("result", prefix)
    .data            <- c(.data, deviation)
  }

  if (!cache && !is.null(baseline_path)) unlink(baseline_path)
  .data
}
