new_stash <- function(cache = NULL, service = "ecmwfr") {
  cache <- cache %||% .default_download_dir(cache = TRUE, service)

  .get <- function() {
    index_path <- file.path(cache, "index.rds")

    if (file.exists(index_path)) {
      readRDS(index_path)
    } else {
      list()
    }
  }

  .prune <- function() {
    unlink(cache, recursive = TRUE, force = TRUE)
  }

  .pop <- function(n = 1) {
    index <- .get()
    len <- length(index)
    to_pop <- index[seq(len - (n - 1), len)]
    for (file in to_pop) unlink(file)
    index <- index[!names(index) %in% names(to_pop)]
    .write(index)
  }

  .restore <- function(request) {
    request$target <- NULL
    request$service <- service
    hash <- rlang::hash(request)
    index <- .get()
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

  .store <- function(path, request) {
    request$target <- NULL
    request$service <- service
    hash <- rlang::hash(request)
    entry <- list(normalizePath(path))
    names(entry) <- hash
    index <- .get()
    index <- c(index, entry)
    index_path <- file.path(cache, "index.rds")
    saveRDS(index, index_path)
  }

  .write <- function(index) {
    index_path <- file.path(cache, "index.rds")
    saveRDS(index, index_path)
  }

  structure(
    class = "gxc_stash",
    list(
      path = cache,
      get = .get,
      write = .write,
      prune = .prune,
      pop = .pop,
      store = .store,
      restore = .restore
    )
  )
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

  stash <- new_stash(path, service = "ecmwfr")
  restored <- stash$restore(request)
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

  data_path <- ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  if (cache) {
    info("Storing file {.val {basename(data_path)}} in cache.")
    stash$store(data_path, request)
  }

  data_path
}
