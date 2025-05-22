gxc_cache <- new.env(parent = emptyenv())


cache_restore <- function(request) {
  request$target <- NULL
  prev_req <- get0("requests", envir = gxc_cache)
  is_dup <- vapply(
    prev_req,
    function(prev) identical(request, prev$request),
    FUN.VALUE = logical(1)
  )

  if (any(is_dup)) {
    path <- prev_req[is_dup][[1]]$path

    if (file.exists(path)) {
      path
    }
  }
}


cache_store <- function(path, request) {
  request$target <- NULL
  request <- list(path = path, request = request)
  prev_reqs <- get0("requests", envir = gxc_cache)
  assign("requests", c(list(request), prev_reqs), envir = gxc_cache)
}


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
.make_request_monthly <- function(indicator,
                                  catalogue,
                                  extent,
                                  years,
                                  months,
                                  path,
                                  prefix,
                                  product_type = "monthly_averaged_reanalysis",
                                  request_time = "00:00",
                                  verbose = NULL) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp, ".grib")

  request <- list(
    data_format = "grib",
    download_format = "unarchived",
    variable = indicator,
    product_type = product_type,
    time = request_time,
    year = years,
    month = months,
    area = extent,
    dataset_short_name = catalogue,
    target = file_name
  )

  restored <- cache_restore(request)
  if (!is.null(restored)) {
    return(restored)
  }

  data_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )
  cache_store(data_path, request)
  data_path
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
.make_request_daily <- function(indicator,
                                catalogue,
                                extent,
                                years,
                                months,
                                days,
                                path,
                                prefix,
                                statistic = "daily_mean",
                                time_zone = "utc+00:00",
                                verbose = NULL) {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)

  restored <- cache_restore(request)
  if (!is.null(restored)) {
    return(restored)
  }

  request <- list(
    variable = indicator,
    product_type = "reanalysis",
    year = years,
    month = months,
    day = days,
    daily_statistic = statistic,
    time_zone = time_zone,
    frequency = "1_hourly",
    area = extent,
    dataset_short_name = catalogue,
    target = file_name
  )

  info(
    "Downloading data from ECMWF...",
    msg_done = "Successfully downloaded data from ECMWF.",
    msg_failed = "Failed to download data from ECMWF.",
    level = "step"
  )
  data_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )
  cache_store(data_path, request)
  data_path
}
