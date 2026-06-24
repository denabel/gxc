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


# Internal replacement for ecmwfr::wf_request_batch with per-file progress
# reporting and an optional callback after each completed request. This
# allows immediate cache storage and clean interrupt handling without
# relying on capture.output or external chunking.
.wf_request_batch <- function(request_list,
                              workers     = 3L,
                              path        = tempdir(),
                              time_out    = 3600,
                              retry       = 30,
                              already_done = 0L,
                              total        = length(request_list),
                              on_progress  = NULL) {
  N     <- length(request_list)
  slots <- as.list(rep(FALSE, min(workers, N)))
  queue <- request_list
  done  <- list()

  total_timeout <- Sys.time() + N * time_out / workers

  # Progress bar shows position relative to total request_length,
  # offset by already_done so it starts where the previous run left off
  pb <- cli::cli_progress_bar(
    name   = "Downloading from CDS",
    total  = total,
    format = paste0(
      "{cli::pb_spin} {cli::pb_name} ",
      "{cli::pb_current}/{cli::pb_total} ",
      "[{cli::pb_elapsed}] {cli::pb_bar}"
    )
  )
  if (already_done > 0L) cli::cli_progress_update(id = pb, set = already_done)
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  tryCatch(
    while (length(done) < N && Sys.time() < total_timeout) {
      for (w in seq_along(slots)) {
        Sys.sleep(retry)

        # Assign next pending request to free slot
        if (isFALSE(slots[[w]]) && length(queue) > 0) {
          invisible(capture.output(
            suppressMessages(
              slots[[w]] <- ecmwfr::wf_request(
                queue[[1]],
                user     = "ecmwfr",
                time_out = time_out,
                retry    = retry,
                path     = path,
                transfer = FALSE
              )
            ),
            type = "output"
          ))
          queue <- queue[-1]
        }

        # Try to download
        if (!isFALSE(slots[[w]])) {
          invisible(capture.output(
            suppressMessages(slots[[w]]$download()),
            type = "output"
          ))
        }

        # Request complete — update progress and fire callback
        if (!isFALSE(slots[[w]]) && !slots[[w]]$is_pending()) {
          invisible(capture.output(
            suppressMessages(slots[[w]]$delete()),
            type = "output"
          ))
          done      <- append(done, slots[[w]])
          file_path <- done[[length(done)]]$get_file()
          slots[[w]] <- FALSE

          cli::cli_progress_update(id = pb)

          if (!is.null(on_progress)) {
            on_progress(
              completed = already_done + length(done),
              total     = total,
              file      = file_path
            )
          }
        }
      }
    },
    interrupt = function(e) {
      cli::cli_progress_done(id = pb)
      cli::cli_alert_warning(
        "Download interrupted after {already_done + length(done)}/{total} \\
        file{?s}."
      )
    }
  )

  unlist(lapply(done, function(x) x$get_file()))
}


# Shared submission logic for both daily and monthly ERA5 batch requests.
# Checks the stash cache first; submits via .wf_request_batch if not cached.
# The on_progress callback stores each file in the cache immediately after
# download, so partial results survive interrupts and errors.
.submit_batch <- function(request,
                          split_fn,
                          path,
                          cache   = TRUE,
                          verbose = TRUE,
                          workers = 3L) {
  all_requests   <- split_fn(request)
  request_length <- length(all_requests)

  stash    <- new_stash(file.path(path, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, request_length)

  if (!is.null(restored)) {
    n_restored <- length(restored)
    if (verbose) {
      cli::cli_alert_success(
        "Restored {n_restored}/{request_length} file{?s} from cache."
      )
    }
    return(restored)
  }

  # Check for partial cache — resume from where we left off
  partial      <- stash$get()[[stash$make_hash(request)]]
  n_partial    <- length(partial)
  already_done <- if (cache && n_partial > 0L) n_partial else 0L
  todo_requests <- all_requests[seq(already_done + 1L, request_length)]

  if (cache && already_done > 0L) {
    if (verbose) {
      cli::cli_alert_success(
        "Restored {already_done}/{request_length} file{?s} from cache."
      )
      cli::cli_alert_info(
        "Resuming — {length(todo_requests)} file{?s} remaining."
      )
    }
  }

  prefix <- strsplit(request$target, "_")[[1]][2]

  info(
    "Preparing {prefix} data from ECMWF \\
    ({request_length} request{?s} total, \\
    downloading {length(todo_requests)})...",
    msg_done   = "Successfully prepared {prefix} data from ECMWF.",
    msg_failed = "Failed to prepare {prefix} data from ECMWF.",
    level      = "step"
  )

  fail_if_test()

  new_paths <- .wf_request_batch(
    todo_requests,
    workers      = workers,
    path         = file.path(path, "era5"),
    retry        = 30,
    already_done = already_done,
    total        = request_length,
    on_progress  = function(completed, total, file) {
      if (cache) stash$store_partial(file, request)
    }
  )

  unlist(c(partial, new_paths))
}


# Submits a pre-built daily ERA5 request as a batch, checking cache first.
.submit_era5_batch <- function(request, path, cache = TRUE,
                               verbose = TRUE, workers = 3L) {
  .submit_batch(request, .split_request_by_day, path, cache, verbose, workers)
}


# Submits a pre-built monthly ERA5 request as a batch, checking cache first.
.submit_era5_monthly_batch <- function(request, path, cache = TRUE,
                                       verbose = TRUE, workers = 3L) {
  .submit_batch(request, .split_request_by_month, path, cache, verbose, workers)
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


# Downloads a DWD HYRAS year file to a temporary subdirectory. Tmp files
# are always redownloaded if present — a leftover tmp file means a previous
# run was interrupted mid-download and the file may be incomplete.
.download_dwd_year_file <- function(indicator, year, path) {
  url_template <- .dwd_url_templates$daily[[indicator]]
  url          <- glue::glue(url_template, year = year)
  tmp_dir      <- file.path(path, "tmp")
  year_file    <- file.path(tmp_dir, basename(url))

  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # Always redownload — a leftover tmp file is likely incomplete
  if (file.exists(year_file)) unlink(year_file)

  info("Downloading DWD year file for {year}...")
  download.file(url, destfile = year_file, mode = "wb", quiet = TRUE)

  year_file
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
      path, "daily",
      paste0(indicator, "_", prefix, "_", format(year_dates, "%Y%m%d"), ".tif")
    )

    # Only download year file if at least one day is not yet cached
    missing <- !file.exists(cached_files)

    if (any(missing)) {
      dir.create(
        file.path(path, "daily"),
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
      path, "monthly",
      paste0(indicator, "_", prefix, "_", yearmonth, ".tif")
    )
    dir.create(file.path(path, "monthly"), showWarnings = FALSE, recursive = TRUE)

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


# Dispatches a daily climate data request to either ERA5 or DWD depending
# on the catalogue source.
.request_climate_daily <- function(indicator,
                                   catalogue,
                                   extent,
                                   years,
                                   months,
                                   days,
                                   prefix,
                                   cache,
                                   path,
                                   statistic = "daily_mean",
                                   time_zone = "utc+00:00",
                                   verbose   = TRUE) {
  if (.catalogue_source(catalogue) == "era5") {
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
  } else {
    .request_dwd_daily(
      indicator = indicator,
      years     = years,
      months    = months,
      days      = days,
      cache     = cache,
      path      = file.path(path, "dwd"),
      prefix    = prefix
    )
  }
}


# Dispatches a monthly climate data request to either ERA5 or DWD depending
# on the catalogue source.
.request_climate_monthly <- function(indicator,
                                     catalogue,
                                     extent,
                                     years,
                                     months,
                                     prefix,
                                     cache,
                                     path,
                                     product_type = "monthly_averaged_reanalysis",
                                     request_time = "00:00",
                                     verbose      = TRUE) {
  if (.catalogue_source(catalogue) == "era5") {
    .request_era5_monthly(
      indicator,
      catalogue    = catalogue,
      extent       = extent,
      years        = years,
      months       = months,
      cache        = cache,
      path         = path,
      prefix       = prefix,
      product_type = product_type,
      request_time = request_time,
      verbose      = verbose
    )
  } else {
    .request_dwd_monthly(
      indicator = indicator,
      years     = years,
      months    = months,
      cache     = cache,
      path      = file.path(path, "dwd"),
      prefix    = prefix
    )
  }
}
