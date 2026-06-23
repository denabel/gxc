# Returns the default download directory for cached files. If cache = TRUE,
# uses the user data directory; otherwise uses a temporary directory.
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


# Decompresses a .gz file to a target path using base R connections,
# reading in 64 KB chunks to support arbitrarily large files.
.decompress_gz <- function(path_gz, path_out) {
  con_in  <- gzcon(file(path_gz, "rb"))
  con_out <- file(path_out, "wb")
  on.exit({
    close(con_in)
    close(con_out)
  }, add = TRUE)
  repeat {
    chunk <- readBin(con_in, "raw", n = 65536L)
    if (length(chunk) == 0L) break
    writeBin(chunk, con_out)
  }
  invisible(path_out)
}


# Splits a multi-day ERA5 request into individual per-day requests.
# wf_request_batch requires one request object per day.
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
    dates         <- as.Date(paste(year_per_day, month_per_day, days, sep = "-"))
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


# Splits a multi-month ERA5 request into individual per-month requests.
# wf_request_batch requires one request object per month.
.split_request_by_month <- function(request) {
  dates <- sort(as.Date(paste(request$year, request$month, "01", sep = "-")))

  lapply(dates, function(d) {
    r         <- request
    r$year    <- format(d, "%Y")
    r$month   <- format(d, "%m")
    r$target  <- paste0(r$variable, "_", r$.prefix, "_", format(d, "%Y%m"))
    r$.prefix <- NULL
    r
  })
}


# Builds a daily ERA5 request list without submitting it. The target field
# contains a timestamp to ensure unique filenames across concurrent calls;
# it is excluded from cache hashing in new_stash().
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


# Builds a monthly ERA5 request list without submitting it. The .prefix
# field is used by .split_request_by_month() to construct unique target
# filenames and is excluded from cache hashing in new_stash().
.build_era5_monthly_request <- function(indicator,
                                        catalogue,
                                        extent,
                                        years,
                                        months,
                                        prefix       = "observation",
                                        product_type = "monthly_averaged_reanalysis",
                                        request_time = "00:00") {
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  file_name <- paste0(indicator, "_", prefix, "_", timestamp)

  list(
    variable           = indicator,
    download_format    = "unarchived",
    product_type       = product_type,
    time               = request_time,
    year               = years,
    month              = months,
    area               = extent,
    dataset_short_name = catalogue,
    target             = file_name,
    .prefix            = prefix
  )
}


# Shared submission logic for both daily and monthly ERA5 batch requests.
# Checks the stash cache first; submits via wf_request_batch if not cached.
# split_fn is either .split_request_by_day or .split_request_by_month.
.submit_batch <- function(request,
                          split_fn,
                          path,
                          cache   = TRUE,
                          verbose = TRUE) {
  request_length <- length(split_fn(request))

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
      data_path <- ecmwfr::wf_request_batch(
        split_fn(request),
        path    = path,
        workers = 6,
        retry   = 5
      ),
      type = "message"
    ),
    type = "output"
  )

  data_path <- as.character(data_path)

  if (cache) {
    info("Storing file {.val {basename(data_path)}} in cache.")
    stash$store(data_path, request)
  }

  data_path
}


# Submits a pre-built daily ERA5 request as a batch, checking cache first.
.submit_era5_batch <- function(request, path, cache = TRUE, verbose = TRUE) {
  .submit_batch(request, .split_request_by_day, path, cache, verbose)
}


# Submits a pre-built monthly ERA5 request as a batch, checking cache first.
.submit_era5_monthly_batch <- function(request, path, cache = TRUE, verbose = TRUE) {
  .submit_batch(request, .split_request_by_month, path, cache, verbose)
}


# Convenience wrapper: build + submit a daily ERA5 request in one call.
# Currently unused but retained for potential use by future pipe-based
# add_baseline() — see baseline.R.
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


# High-level wrapper for monthly ERA5 requests: builds the request list
# and submits it via .submit_era5_monthly_batch.
.request_era5_monthly <- function(indicator,
                                  catalogue,
                                  extent,
                                  years,
                                  months,
                                  days         = NULL,
                                  cache        = FALSE,
                                  path         = NULL,
                                  prefix       = "observation",
                                  product_type = "monthly_averaged_reanalysis",
                                  request_time = "00:00",
                                  verbose      = NULL) {
  request <- .build_era5_monthly_request(
    indicator    = indicator,
    catalogue    = catalogue,
    extent       = extent,
    years        = years,
    months       = months,
    prefix       = prefix,
    product_type = product_type,
    request_time = request_time
  )
  .submit_era5_monthly_batch(request, path = path, cache = cache, verbose = verbose)
}


# High-level wrapper for daily ERA5 requests: builds the request list
# and submits it via .submit_era5_batch.
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


# Downloads a DWD HYRAS year file to a temporary subdirectory. If the file
# is already present from a previous call within the same session it is
# reused; it will be deleted after slicing (see .request_dwd_daily).
.download_dwd_year_file <- function(indicator, year, path) {
  url_template <- .dwd_url_templates$daily[[indicator]]
  url          <- glue::glue(url_template, year = year)
  tmp_dir      <- file.path(path, "dwd", "tmp")
  year_file    <- file.path(tmp_dir, basename(url))

  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  if (file.exists(year_file)) return(year_file)

  info("Downloading DWD year file for {year}...")
  download.file(url, destfile = year_file, mode = "wb", quiet = TRUE)

  year_file
}


# Downloads DWD HYRAS daily data for the requested dates. Year files are
# downloaded once per year, sliced into individual per-day .tif files, and
# then deleted to save disk space. Already-cached day files are skipped.
# CRS is corrected on write — HYRAS daily nc files declare EPSG:4258 but
# the data is in EPSG:3035.
.request_dwd_daily <- function(indicator,
                               years,
                               months,
                               days,
                               cache  = TRUE,
                               path   = NULL,
                               prefix = "observation") {
  path  <- path %||% .default_download_dir(cache, service = "dwd")
  dates <- as.Date(paste(years, months, days, sep = "-"))

  # Group dates by year to load each year file only once
  dates_by_year <- split(dates, format(dates, "%Y"))

  all_paths <- unlist(lapply(names(dates_by_year), function(year) {
    year_dates <- dates_by_year[[year]]

    # Build expected cache paths for all days in this year
    cached_files <- file.path(
      path, "dwd", "daily",
      paste0(indicator, "_", prefix, "_", format(year_dates, "%Y%m%d"), ".tif")
    )

    # Only download year file if at least one day is not yet cached
    missing <- !file.exists(cached_files)

    if (any(missing)) {
      dir.create(
        file.path(path, "dwd", "daily"),
        showWarnings = FALSE,
        recursive    = TRUE
      )

      year_file       <- .download_dwd_year_file(indicator, year, path)
      year_rast       <- terra::rast(year_file)
      year_dates_rast <- as_date(terra::time(year_rast))

      for (idx in which(missing)) {
        d       <- year_dates[[idx]]
        lyr_idx <- which(year_dates_rast == d)

        if (length(lyr_idx) == 0) {
          cli::cli_abort(
            "No layer found for date {.val {d}} in {.path {year_file}}."
          )
        }

        day_layer <- year_rast[[lyr_idx]]
        terra::crs(day_layer) <- "EPSG:3035"
        terra::writeRaster(day_layer, cached_files[[idx]], overwrite = FALSE)
      }

      # Remove raw year file after all missing days have been sliced
      unlink(year_file)
    }

    cached_files
  }), recursive = FALSE)

  as.character(all_paths)
}


# Downloads DWD monthly data for the requested year-month combinations.
# Files are served as compressed ASCII grids (.asc.gz), decompressed,
# and cached as .tif files. CRS is set on write — monthly ASC files
# have no CRS declaration; data is in EPSG:31467.
.request_dwd_monthly <- function(indicator,
                                 years,
                                 months,
                                 cache  = TRUE,
                                 path   = NULL,
                                 prefix = "observation") {
  path  <- path %||% .default_download_dir(cache, service = "dwd")
  dates <- as.Date(paste(years, months, "01", sep = "-"))

  all_paths <- sapply(dates, function(d) {
    yearmonth    <- format(d, "%Y%m")
    month_folder <- .dwd_month_folder(format(d, "%m"))
    url_template <- .dwd_url_templates$monthly[[indicator]]
    url          <- glue::glue(
      url_template,
      month_folder = month_folder,
      yearmonth    = yearmonth
    )

    cached_file <- file.path(
      path, "dwd", "monthly",
      paste0(indicator, "_", prefix, "_", yearmonth, ".tif")
    )
    dir.create(dirname(cached_file), showWarnings = FALSE, recursive = TRUE)

    if (file.exists(cached_file)) return(cached_file)

    tmp_gz  <- tempfile(fileext = ".asc.gz")
    tmp_asc <- sub("\\.gz$", "", tmp_gz)

    download.file(url, destfile = tmp_gz, mode = "wb", quiet = TRUE)
    .decompress_gz(tmp_gz, tmp_asc)

    r <- terra::rast(tmp_asc)
    terra::crs(r) <- "EPSG:31467"
    terra::writeRaster(r, cached_file, overwrite = FALSE)
    unlink(c(tmp_gz, tmp_asc))

    cached_file
  })

  as.character(all_paths)
}
