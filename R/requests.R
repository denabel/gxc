.default_download_dir <- function(cache, service = NULL) {
  dir <- if (cache) {
    tools::R_user_dir("gxc", which = "data")
  } else {
    tempdir()
  }

  if (!is.null(service)) {
    dir <- file.path(dir, service)
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

.split_request_by_day <- function(request) {
  years  <- as.integer(request$year)
  months <- as.integer(request$month)
  days   <- as.integer(request$day)

  if (length(years) == length(months) && length(months) == length(days)) {
    dates <- as.Date(paste(years, months, days, sep = "-"))
  } else {
    counts <- integer(length(months))
    m_idx  <- 1L
    for (i in seq_along(days)) {
      if (i > 1 && days[i] < days[i - 1]) m_idx <- m_idx + 1L
      counts[m_idx] <- counts[m_idx] + 1L
    }
    month_per_day <- rep(months, times = counts)
    year_per_day  <- rep(years, each = length(days))
    dates <- as.Date(paste(year_per_day, month_per_day, days, sep = "-"))
  }

  dates <- sort(dates)

  lapply(dates, function(d) {
    r        <- request
    r$year   <- format(d, "%Y")
    r$month  <- format(d, "%m")
    r$day    <- format(d, "%d")
    r$target <- paste0(request$target, "_", format(d, "%Y%m%d"))
    r
  })
}

# Builds a daily ERA5 request list without submitting it
.build_era5_daily_request <- function(indicator,
                                      catalogue,
                                      extent,
                                      years,
                                      months,
                                      days,
                                      prefix    = "observation",
                                      statistic = "daily_mean",
                                      time_zone = "utc+00:00") {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)

  list(
    variable           = indicator,
    product_type       = "reanalysis",
    year               = years,
    month              = months,
    day                = days,
    daily_statistic    = statistic,
    time_zone          = time_zone,
    frequency          = "1_hourly",
    area               = extent,
    dataset_short_name = catalogue,
    target             = file_name
  )
}

# Submits a pre-built request, checking cache first
.submit_era5_batch <- function(request,
                               path,
                               cache   = TRUE,
                               verbose = TRUE) {
  request_length <- length(.split_request_by_day(request))

  stash    <- new_stash(path, service = "ecmwfr")
  restored <- stash$restore(request, request_length)

  if (!is.null(restored)) {
    file <- basename(restored)
    info(
      "Restoring file {.val {file}} from cache...",
      msg_done   = "Restored file {.val {file}} from cache.",
      msg_failed = "Failed to restore file {.val {file}} from cache.",
      level      = "step"
    )
    return(restored)
  }

  prefix <- strsplit(request$target, "_")[[1]][2]

  info(
    "Preparing {prefix} data from ECMWF...",
    msg_done   = "Successfully prepared {prefix} data from ECMWF.",
    msg_failed = "Failed to prepare {prefix} data from ECMWF.",
    level      = "step"
  )

  fail_if_test()
  capture.output(
    capture.output(
      data_path <-
        ecmwfr::wf_request_batch(
          .split_request_by_day(request),
          path    = path,
          workers = 6,
          retry   = 5
        ),
      type = "message"
    ),
    type = "output"
  )

  if (cache) {
    info("Storing file {.val {basename(data_path)}} in cache.")
    stash$store(data_path, request)
  }

  data_path
}

# Wrapper: build + submit in one call (used by link_monthly and add_baseline)
.ecmwf_request <- function(indicator,
                           ...,
                           cache   = FALSE,
                           path    = tempdir(),
                           prefix  = "toi",
                           verbose = TRUE) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)
  request   <- list(variable = indicator, ..., target = file_name)
  .submit_era5_batch(request, path = path, cache = cache, verbose = verbose)
}

.request_era5_monthly <- function(indicator,
                                  catalogue,
                                  extent,
                                  years,
                                  months,
                                  days,
                                  cache        = FALSE,
                                  path         = NULL,
                                  prefix       = "observation",
                                  product_type = "monthly_averaged_reanalysis",
                                  request_time = "00:00",
                                  verbose      = NULL) {
  .ecmwf_request(
    indicator          = indicator,
    download_format    = "unarchived",
    product_type       = product_type,
    time               = request_time,
    year               = years,
    month              = months,
    area               = extent,
    dataset_short_name = catalogue,
    cache              = cache,
    path               = path,
    prefix             = prefix,
    verbose            = verbose
  )
}

.request_era5_daily <- function(indicator,
                                catalogue,
                                extent,
                                years,
                                months,
                                days,
                                cache     = FALSE,
                                path      = NULL,
                                prefix    = "observation",
                                statistic = "daily_mean",
                                time_zone = "utc+00:00",
                                verbose   = NULL) {
  request <- .build_era5_daily_request(
    indicator = indicator,
    catalogue = catalogue,
    extent    = extent,
    years     = years,
    months    = months,
    days      = days,
    prefix    = prefix,
    statistic = statistic,
    time_zone = time_zone
  )
  .submit_era5_batch(request, path = path, cache = cache, verbose = verbose)
}
