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

.split_request_list <- function(request) {
  y <- request$year
  m <- request$month
  d <- request$day

  split_idx <- rep(seq_along(m), length.out = length(d))

  df <- data.frame(
    year  = rep(y, length(d)),
    month = m[split_idx],
    day   = d
  )

  df_list <- split(df, list(df$year, df$month))

  batch_request <- lapply(df_list, function(df_m) {

    new_request <- request
    new_request$year  <- unique(df_m$year)
    new_request$month <- unique(df_m$month)
    new_request$day   <- df_m$day

    new_request$target <- paste(
      new_request$target,
      new_request$year,
      new_request$month,
      sep = "_"
    )

    new_request
  })

  batch_request
}

# .split_request_list <-
#   function(request, split_elements = c("year", "month", "day")) {
#     split_values <- lapply(split_elements, function(var) request[[var]])
#     names(split_values) <- split_elements
#
#     y <- split_values$year
#     m <- split_values$month
#     d <- split_values$day
#
#     k <- max(length(m), length(d))
#
#     m2 <- rep(m, length.out = k)
#     d2 <- rep(d, length.out = k)
#
#     ny <- length(y)
#
#     split_values <-
#       data.frame(
#         year  = rep(y, each = k),
#         month = rep(m2, times = ny),
#         day   = rep(d2, times = ny),
#         stringsAsFactors = FALSE
#       )
#
#     batch_request <- apply(split_values, 1, function(row) {
#       new_request <- request
#
#       for (i in seq_along(split_elements)) {
#         new_request[[split_elements[i]]] <- as.vector(row[i])
#       }
#
#       new_request$target <-
#         paste(
#           new_request$target, new_request$year, new_request$month,
#           new_request$day, sep = "_")
#
#       new_request
#     })
#
#     batch_request
#   }


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
    data_format = "grib",
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

  # request_length <- length(.split_request_list(request))

  stash <- new_stash(path, service = "ecmwfr")
  restored <- stash$restore(request)
  # restored <- stash$restore(request, request_length)
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
  data_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )


  # if (sum(lengths(list(request$year, request$month, request$day))) > 3) {
  #   batch_request <- .split_request_list(request)
  #
  #   data_path <- suppressMessages(sapply(batch_request, function(i) {
  #     ecmwfr::wf_request(
  #       request = i,
  #       transfer = FALSE,
  #       path = path,
  #       verbose = FALSE
  #     )
  #   }))
  #
  #   # year_variable <- "year"
  #   # batch_request <- lapply(request[[year_variable]], function(year) {
  #   #   batch_request_i <- request
  #   #   batch_request_i[[year_variable]] <- year
  #   #   batch_request_i$target <- paste0(batch_request_i$target, "_", year)
  #   #   batch_request_i
  #   # })
  #
  #   # data_path <- suppressMessages(ecmwfr::wf_request_batch(
  #   #   request_list = batch_request,
  #   #   path = path,
  #   #   workers = 6,
  #   #   retry = 5
  #   # ))
  # } else {
  #   data_path <- ecmwfr::wf_request(
  #     request = request,
  #     transfer = TRUE,
  #     path = path,
  #     verbose = FALSE
  #   )
  # }
  #
  # data_path <- sapply(data_path, function(request) {
  #   status <- request$get_status()
  #
  #   while (status != "successful") {
  #     Sys.sleep(5)
  #     request$update_status()
  #     status <- request$get_status()
  #   }
  #
  #   request$download()
  #   request$get_file()
  # })

  #   ecmwfr::wf_transfer(data_path[[request]]$get_url())
  # })

  # sapply(length(data_path), function (request) {
  #   ecmwfr::wf_transfer(data_path[[request]]$get_url())
  # })

  if (cache) {
    info("Storing file {.val {basename(data_path)}} in cache.")
    stash$store(data_path, request)
  }

  data_path
}
