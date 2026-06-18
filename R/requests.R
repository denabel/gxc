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

  # case: all three vector have the same lenght
  if (length(years) == length(months) && length(months) == length(days)) {
    dates <- as.Date(paste(years, months, days, sep = "-"))
  } else {
    # fallback
    counts <- integer(length(months))
    m_idx <- 1L
    for (i in seq_along(days)) {
      if (i > 1 && days[i] < days[i - 1]) m_idx <- m_idx + 1L
      counts[m_idx] <- counts[m_idx] + 1L
    }
    month_per_day <- rep(months, times = counts)
    year_per_day <- rep(years, each = length(days))
    dates <- as.Date(paste(year_per_day, month_per_day, days, sep = "-"))
  }

  dates <- sort(dates)

  lapply(dates, function(d) {
    r <- request
    r$year  <- format(d, "%Y")
    r$month <- format(d, "%m")
    r$day   <- format(d, "%d")
    r$target <- paste0(request$target, "_", format(d, "%Y%m%d"))
    r
  })
}


# .split_request_list <- function(request) {
#   y <- request$year
#   m <- request$month
#   d <- request$day
#
#   split_idx <- rep(seq_along(m), length.out = length(d))
#
#   df <- data.frame(
#     year  = rep(y, length(d)),
#     month = m[split_idx],
#     day   = d
#   )
#
#   df_list <- split(df, list(df$year, df$month))
#
#   batch_request <- lapply(df_list, function(df_m) {
#
#     new_request <- request
#     new_request$year  <- unique(df_m$year)
#     new_request$month <- unique(df_m$month)
#     new_request$day   <- df_m$day
#
#     new_request$target <- paste(
#       new_request$target,
#       new_request$year,
#       new_request$month,
#       sep = "_"
#     )
#
#     new_request
#   })
#
#   batch_request
# }

#' @title Internal helper function to request monthly data from C3S
#'
#' @description This function requests monthly-averaged reanalysis data for
#' a specified indicator, catalogue, time period, and spatial extent.
#'
#' @param indicator Character string specifying the indicator to download.
#' @param catalogue Character string specifying which catalogue to use.
#' @param extent Numeric vector specifying the bounding box area (N,W,S,E).
#' @param years Character vector of years for which data should be retrieved.
#' @param months Character vector of months for which data should be retrieved.
#' @param path Character string specifying the directory path where data will be stored.
#' @param prefix Character string specifying a prefix for the target filename (e.g., "focal" or "baseline").
#'
#' @return A character string with the path to the downloaded file.
#'
#' @importFrom ecmwfr wf_request
#' @noRd
.request_era5_monthly <- function(indicator,
                                  catalogue,
                                  extent,
                                  years,
                                  months,
                                  days,
                                  cache = FALSE,
                                  path = NULL,
                                  prefix = "observation",
                                  product_type = "monthly_averaged_reanalysis",
                                  request_time = "00:00",
                                  verbose = NULL) {
  .ecmwf_request(
    indicator = indicator,
    download_format = "unarchived",
    product_type = product_type,
    time = request_time,
    year = years,
    month = months,
    area = extent,
    dataset_short_name = catalogue,
    cache = cache,
    path = path,
    prefix = prefix,
    verbose = verbose
  )
}


#' @title Internal helper function to request daily data from C3S
#'
#' @description This function requests daily-averaged reanalysis data for
#' a specified indicator, catalogue, time period, and spatial extent.
#'
#' @param indicator Character string specifying the indicator to download.
#' @param catalogue Character string specifying which catalogue to use.
#' @param extent Numeric vector specifying the bounding box area (N,W,S,E).
#' @param years Character vector of years for which data should be retrieved.
#' @param months Character vector of months for which data should be retrieved.
#' @param days Character vector of days for which data should be retrieved.
#' @param path Character string specifying the directory path where data will be stored.
#' @param prefix Character string specifying a prefix for the target filename (e.g., "focal" or "baseline").
#'
#' @return A character string with the path to the downloaded file.
#'
#' @importFrom ecmwfr wf_request
#' @noRd
.request_era5_daily <- function(indicator,
                                catalogue,
                                extent,
                                years,
                                months,
                                days,
                                cache = FALSE,
                                path = NULL,
                                prefix = "observation",
                                statistic = "daily_mean",
                                time_zone = "utc+00:00",
                                verbose = NULL) {
  .ecmwf_request(
    indicator = indicator,
    product_type = "reanalysis",
    year = years,
    month = months,
    day = days,
    daily_statistic = statistic,
    time_zone = time_zone,
    frequency = "1_hourly",
    area = extent,
    dataset_short_name = catalogue,
    cache = cache,
    path = path,
    prefix = prefix,
    verbose = verbose
  )
}


.ecmwf_request <- function(indicator,
                           ...,
                           cache = FALSE,
                           path = tempdir(),
                           prefix = "toi",
                           verbose = TRUE) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)

  request <- list(variable = indicator, ..., target = file_name)

  request_length <- length(.split_request_by_day(request))

  stash <- new_stash(path, service = "ecmwfr")
  # restored <- stash$restore(request)
  restored <- stash$restore(request, request_length)
  if (!is.null(restored)) {
    file <- basename(restored)
    info(
      "Restoring file {.val {file}} from cache...",
      msg_done = "Restored file {.val {file}} from cache.",
      msg_failed = "Failed to restore file {.val {file}} from cache.",
      level = "step"
    )
    return(restored)
  }

  info(
    "Preparing {prefix} data from ECMWF...",
    msg_done = "Successfully prepared {prefix} data from ECMWF.",
    msg_failed = "Failed to prepare {prefix} data from ECMWF.",
    level = "step"
  )

  fail_if_test()
  capture.output(
    capture.output(
      data_path <-
        ecmwfr::wf_request_batch(
          .split_request_by_day(request),
          path = path,
          workers = 6,
          retry = 5
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
