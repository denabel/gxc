.cache_get <- function(cache = NULL, service = "ecmwfr") {
  cache <- cache %||% .default_download_dir(cache = TRUE, service)
  index_path <- file.path(cache, "index.rds")

  if (file.exists(index_path)) {
    readRDS(index_path)
  } else {
    list()
  }
}


.cache_prune <- function(cache = NULL, service = NULL) {
  cache <- cache %||% .default_download_dir(cache = TRUE, service = service)
  unlink(cache, recursive = TRUE)
}


.cache_restore <- function(request, cache = NULL, service = "ecmwfr") {
  request$target <- NULL
  request$service <- service
  cache <- cache %||% .default_download_dir(cache = TRUE, service)
  hash <- rlang::hash(request)
  index <- .cache_get(cache, service = service)
  cached_path <- index[[hash]]

  if (!is.null(cached_path) && !file.exists(cached_path)) {
    cli::cli_warn(c(
      "!" = "A matching file has been found in the cache but it is corrupt.",
      "i" = "Will clean the cache and redownload instead."
    ))

    index[[hash]] <- NULL
    return(NULL)
  }

  cached_path
}


.cache_store <- function(path, request, cache = NULL, service = "ecmwfr") {
  request$target <- NULL
  request$service <- service
  cache <- cache %||% .default_download_dir(cache = TRUE, service)
  hash <- rlang::hash(request)
  entry <- list(normalizePath(path))
  names(entry) <- hash
  index <- .cache_get(cache, service = service)
  index <- c(index, entry)
  index_path <- file.path(cache, "index.rds")
  saveRDS(index, index_path)
}


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

  restored <- .cache_restore(request)
  if (!is.null(restored)) {
    return(restored)
  }

  data_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  .cache_store(data_path, request)
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

  restored <- .cache_restore(request, cache = path, service = "ecmwfr")
  if (!is.null(restored)) {
    info("Restoring file {.val {basename(restored)}} from cache.")
    return(restored)
  }

  info(
    "Downloading {prefix} data from ECMWF...",
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

  if (cache) {
    info("Storing file {.val {basename(data_path)}} in cache.")
    .cache_store(data_path, request, cache = path, service = "ecmwfr")
  }

  data_path
}
