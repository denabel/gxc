"%>>%" <- function(x, which) {
  attr(x, which, exact = TRUE)
}


#' Dispatch function that displays a message using cli functions only if
#' verbosity is enabled. If not explicitly provided, the function reads
#' a `verbose` object from the parent frame.
#' @param ... Arguments passed to cli functions
#' @param level info, warning, danger, success or step. Corresponds to
#' respective cli functions
#' @param verbose If FALSE, does nothing.
#' @param .envir Environment from which to detect `verbose`
#' @returns Nothing.
#' @noRd
info <- function(...,
                 level = "info",
                 verbose = NULL,
                 .envir = parent.frame()) {
  verbose <- verbose %||% get0("verbose", envir = .envir, ifnotfound = TRUE)
  if (!verbose) {
    return(invisible(NULL))
  }

  fun <- switch(
    level,
    info = cli::cli_alert_info,
    warning = cli::cli_alert_warning,
    danger = cli::cli_alert_danger,
    success = cli::cli_alert_success,
    step = cli::cli_progress_step,
    update = cli::cli_progress_update
  )

  fun(..., .envir = .envir)
}


#' Catches messages, warnings, and error from an expression and dispatches
#' them through their cli equivalents, effectively showing classic conditions
#' as cli conditions.
#' @param expr An expression
#' @param .envir Environment in which to evaluate error messages
#' @returns Value of expr
#' @noRd
with_cli <- function(expr, .envir = parent.frame()) {
  withCallingHandlers(
    expr,
    message = function(m) {
      cli::cli_inform(conditionMessage(m), .envir = .envir)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      cli::cli_warn(conditionMessage(w), .envir = .envir)
      invokeRestart("muffleWarning")
    },
    error = function(e) cli::cli_abort(conditionMessage(e), .envir = .envir)
  )
}


#' Wrapper around dQuote that doesnt use fancy quotes
#' @return A string
#' @noRd
dquote <- function(x) {
  dQuote(x, q = FALSE)
}


#' Converts an object to an sf tibble
#' @returns An sf tibble
#' @noRd
as_sf_tibble <- function(x, ...) {
  sf::st_as_sf(as_data_frame(x), ...)
}


#' Converts an object to a tibble or dataframe
#' @returns Dataframe or tibble
#' @noRd
as_data_frame <- function(x) {
  if (rlang::is_installed("tibble")) {
    tibble::as_tibble(x)
  } else {
    as.data.frame(x)
  }
}


move_to_back <- function(x, column) {
  col_value <- x[[column]]
  x[[column]] <- NULL
  x[[column]] <- col_value
  x
}


#' Wrapper around deparse(substitute())
#' @return A language object
#' @noRd
obj_name <- function(x, env = parent.frame()) {
  deparse(substitute(x, env))
}


#' Wrapper around as.Date that tries more formats: ISO 8601, US format,
#' European format, European format with dots, full month name (Month Day, Year),
#' abbreviated month name (Mon Day, Year), day-abbreviated month-year, and
#' Year/Month/day
#' @param x Object coercible to date
#' @param try_formats Formats to try
#' @returns A date
#' @noRd
as_date <- function(x, try_formats = c("%Y-%m-%d",
                                       "%m/%d/%Y",
                                       "%d/%m/%Y",
                                       "%B %d, %Y",
                                       "%b %d, %Y",
                                       "%d-%b-%Y",
                                       "%Y/%m/%d",
                                       "%d.%m.%Y")) {
  as.Date(x, tryFormats = try_formats)
}


#' Base equivalent of lubridate::days()
#' @param x Number of days
#' @returns Difftime object
#' @noRd
days <- function(x = 1) {
  as.difftime(x, units = "days")
}


#' Given a numeric value, lists its values, sorts them and returns them as
#' strings.
#' @param x A value that can be coerced to numeric
#' @returns A string
#' @noRd
num_keys <- function(x) {
  as.character(unique(x))
}


#' Extracts the year from a date.
#' @param x A date-time object.
#' @returns A numeric
#' @noRd
year <- function(x) {
  tz <- attr(x, "tzone") %||% ""
  as.POSIXlt(x, tz = tz)[, "year"] + 1900
}


#' Extracts the month from a date.
#' @param x A date-time object.
#' @returns A numeric
#' @noRd
month <- function(x) {
  tz <- attr(x, "tzone") %||% ""
  as.POSIXlt(x, tz = tz)[, "mon"] + 1
}


#' Extracts the day from a date.
#' @param x A date-time object.
#' @returns A numeric
#' @noRd
day <- function(x) {
  tz <- attr(x, "tzone") %||% ""
  as.POSIXlt(x, tz = tz)[, "mday"]
}


#' Constructs dates from their components
#' @param years A vector of years
#' @param months A vector of months
#' @param days A vector of days
#' @param unlist Whether to unlist the date vector or group it by year-month
#' @returns A POSIXct vector
#' @noRd
make_dates <- function(years, months = NULL, days = NULL, unlist = TRUE) {
  months <- months %||% 1:12
  ymd <- expand.grid(year = years, month = months, stringsAsFactors = FALSE)
  ymd <- ymd[order(ymd$year), ]

  if (is.null(days)) {
    ymd$day <- .mapply(dots = ymd, FUN = get_days_in_month, MoreArgs = NULL)
  } else {
    ymd$day <- replicate(nrow(ymd), days, simplify = FALSE)
  }

  dates <- .mapply(make_date, ymd, MoreArgs = NULL)

  if (unlist) {
    dates <- as.POSIXct(unlist(dates))
  }

  dates
}


#' Creates a POSIXct from its year, month, and day
#' @param year A year
#' @param month A month
#' @param day A day
#' @returns POSIXct
#' @noRd
make_date <- function(year, month, day) {
  as.POSIXct(sprintf(
    "%04d-%02d-%02d",
    as.numeric(year),
    as.numeric(month),
    as.numeric(day)
  ))
}


#' Given a year-month, returns all its days
#' @param year A year
#' @param month A month
#' @returns An integer vector
#' @noRd
get_days_in_month <- function(year, month) {
  month_str <- sprintf("%02d", month)
  first_day <- as.Date(paste(year, month_str, "01", sep = "-"))

  if (month == 12) {
    next_month_first_day <- as.Date(paste(year + 1, "01", "01", sep = "-"))
  } else {
    next_month_str <- sprintf("%02d", month + 1)
    next_month_first_day <- as.Date(paste(year, next_month_str, "01", sep = "-"))
  }

  all_days <- seq(first_day, next_month_first_day - 1, by = "day")
  as.integer(format(all_days, "%d"))
}


#' Checks if an object is a SpatRaster or SpatVector
#' @param x R object
#' @returns TRUE or FALSE
#' @noRd
is_terra <- function(x) {
  inherits(x, c("SpatRaster", "SpatVector"))
}


#' Checks if an object is an sf dataframe
#' @param x R object
#' @returns TRUE or FALSE
#' @noRd
is_sf <- function(x) {
  inherits(x, "sf")
}


#' Given a file, makes sure that it and its directory exists. If not,
#' creates it.
#' @param file A file path.
#' @returns Nothing relevant.
#' @noRd
ensure_file <- function(file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file.create(file, showWarnings = FALSE)
}


local_cdf <- function(raster,
                      path = tempfile(fileext = ".nc"),
                      .envir = parent.frame()) {
  orig_units <- terra::units(raster)
  orig_varnames <- terra::varnames(raster)
  raster <- metags_sanitize(raster)

  terra::writeCDF(
    raster,
    filename = path,
    varname = orig_varnames,
    unit = orig_units,
    overwrite = TRUE
  )

  . <- NULL
  do.call(
    on.exit,
    list(bquote(unlink(.(path))), add = TRUE),
    envir = .envir
  )

  path
}


#' Given a SpatRaster this function manually assigns a timestamp based on
#' its components (year, month, day).
#'
#' @param raster A SpatRaster object to be processed.
#' @param days Vector of days (numeric or character) used to build the date vector.
#' @param months Vector of months (numeric or character).
#' @param years Vector of years (numeric or character).
#'
#' @return The input SpatRaster with its time dimension updated. On disk, the
#'   original file is replaced by the new file with time information.
#' @noRd
raster_timestamp <- function(raster, days, months, years, span = NULL) {
  terra::depth(raster) <- NULL

  if (!is.null(span)) {
    valid_dates <- sort(unique(as_date(span)))
  } else {
    valid_dates <- make_dates(years, months, days)
    valid_dates <- as.Date(sort(valid_dates))
  }

  if (length(valid_dates) != terra::nlyr(raster)) {
    cli::cli_abort(c(
      "Number of dates ({length(valid_dates)}) does not match number of raster layers ({terra::nlyr(raster)}).",
      "i" = "Expected {terra::nlyr(raster)} dates, got {length(valid_dates)}."
    ))
  }

  terra::time(raster) <- valid_dates
  raster
}


#' Function to sanitize the metadata of a SpatRaster, i.e., remove all metags
#' that contain disallowed characters.
#' @param raster A SpatRaster
#' @returns A SpatRaster
#' @noRd
metags_sanitize <- function(raster) {
  meta <- terra::metags(raster)

  if (is.null(meta)) {
    return(raster)
  }

  if (utils::packageVersion("terra") > "1.8-42") {
    empty <- meta
    empty[, 2] <- ""
    terra::metags(raster) <- empty
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta$value), ]
  } else {
    terra::metags(raster) <- NULL
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta)]
  }

  raster
}


fail_if_test <- function() {
  if (isTRUE(getOption(".__gxc_fail_on_request__.", FALSE))) {
    stop("Code has been run in a test where this code should not be running!")
  }
}


left_merge <- function(x, y, by.x, by.y, ...) {
  idx <- match(y[[by.y]], x[[by.x]])
  matches <- !is.na(idx)
  idx <- idx[matches]
  n <- nrow(x)

  for (col in setdiff(names(y), by.y)) {
    join_col <- y[[col]][matches]
    if (inherits(join_col, "sfc")) {
      crs <- sf::st_crs(join_col)
      geom_col <- col
      type <- as.character(unique(sf::st_geometry_type(join_col)))[1]
      new_col <- replicate(n, make_empty_geometry(type), simplify = FALSE)
      new_col <- sf::st_as_sfc(new_col)
    } else {
      new_col <- rep(NA, n)
    }

    new_col[idx] <- join_col
    x[[col]] <- new_col
  }

  is_spatial <- any(vapply(x, inherits, "sfc", FUN.VALUE = logical(1)))
  if (is_spatial) {
    as_sf_tibble(x, crs = crs, sf_column_name = geom_col)
  } else {
    as_data_frame(x)
  }
}


psum <- function(..., na.rm=FALSE) {
  dat <- do.call(cbind, list(...))
  res <- rowSums(dat, na.rm = na.rm)
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res
}

# Session-scoped raster caches, at two levels:
# 1. .raster_memo_cache: whole file-SET level (exact repeat requests, e.g.
#    the same indicator + baseline years + months across many grid specs,
#    return the assembled stack instantly).
# 2. .raster_file_cache: individual FILE level, so a DIFFERENT but
#    overlapping file-set (e.g. two baseline periods that share some years,
#    like 1961-1990 and 1981-2010) can reuse the individual files it has in
#    common instead of re-reading them, even though the two file-sets as a
#    whole are different cache entries.
.raster_memo_cache <- new.env(parent = emptyenv())
.raster_file_cache  <- new.env(parent = emptyenv())

.file_cache_key <- function(path) {
  rlang::hash(list(path, file.mtime(path)))
}

#' Safely loads a vector of raster file paths into a single SpatRaster,
#' resampling to a common extent if files have mismatched extents.
#' Memoized at two levels (file-set and individual-file, see
#' .raster_memo_cache/.raster_file_cache above): repeated calls with the
#' EXACT same set of file paths return instantly, and calls with a
#' DIFFERENT but overlapping set of files only re-read the files that
#' aren't already cached individually.
#' @noRd
.safe_rast <- function(paths) {
  if (length(paths) == 1) return(terra::rast(paths))

  # Cache key: hash of the sorted path vector PLUS each file's modification
  # time (computed on the SAME sorted order, so the key is stable
  # regardless of the order paths happen to arrive in). Including mtime
  # means that if a file gets re-downloaded/overwritten during the same
  # session (e.g. replacing a corrupted slice) the key changes and the
  # cache correctly rebuilds, instead of silently serving a stale cached
  # raster for the same path. file.mtime() is a cheap stat() call per file
  # (milliseconds for thousands of files) -- nowhere near the cost of
  # actually opening them.
  sorted_paths <- sort(paths)
  set_key      <- rlang::hash(list(sorted_paths, file.mtime(sorted_paths)))

  if (exists(set_key, envir = .raster_memo_cache, inherits = FALSE)) {
    return(get(set_key, envir = .raster_memo_cache, inherits = FALSE))
  }

  # Not an exact file-set match -- check the file-level cache for each
  # individual path, in the ORIGINAL (not sorted) order, since the final
  # stack must match `paths`' order.
  cached_layers <- vector("list", length(paths))
  missing_idx   <- integer(0)

  for (idx in seq_along(paths)) {
    fk <- .file_cache_key(paths[idx])
    if (exists(fk, envir = .raster_file_cache, inherits = FALSE)) {
      cached_layers[[idx]] <- get(fk, envir = .raster_file_cache, inherits = FALSE)
    } else {
      missing_idx <- c(missing_idx, idx)
    }
  }

  if (length(missing_idx) > 0) {
    # Fast path: try loading all still-missing files in a single vectorized
    # call (see earlier fix -- this is what made a single .safe_rast() call
    # itself ~23s for 2760 files, down from opening each file individually).
    loaded <- tryCatch(terra::rast(paths[missing_idx]), error = function(e) NULL)

    if (!is.null(loaded) && terra::nlyr(loaded) == length(missing_idx)) {
      for (j in seq_along(missing_idx)) {
        idx   <- missing_idx[j]
        layer <- loaded[[j]]
        cached_layers[[idx]] <- layer
        assign(.file_cache_key(paths[idx]), layer, envir = .raster_file_cache)
      }
    } else {
      # Fallback: something's actually mismatched -- load and resample
      # individually against the FIRST resolved layer as reference
      # (whether that came from the file cache or was just loaded), same
      # as the previous single-level fallback.
      for (idx in missing_idx) {
        r <- terra::rast(paths[idx])
        cached_layers[[idx]] <- r
        assign(.file_cache_key(paths[idx]), r, envir = .raster_file_cache)
      }

      reference <- cached_layers[[1]]
      cached_layers <- lapply(cached_layers, function(r) {
        if (!terra::compareGeom(r, reference, stopOnError = FALSE)) {
          terra::resample(r, reference, method = "bilinear")
        } else {
          r
        }
      })
    }
  }

  result <- do.call(c, cached_layers)
  assign(set_key, result, envir = .raster_memo_cache)
  result
}

#' Extracts study values from raster_values — handles both the case where
#' raster_values is a single dataframe (all observations share the same
#' link_date and time_span == 0) and a list (all other cases)
#' @noRd
.extract_study_values <- function(raster_values, study_fun) {
  if (is.data.frame(raster_values)) {
    raster_values[[1]]
  } else {
    sapply(raster_values, function(x) {
      if (is.numeric(x) && length(x) > 1) study_fun(x)
      else if (is.data.frame(x)) x[1, 1]
      else as.numeric(x)
    })
  }
}

# Loads a vector of raster file paths into a single SpatRaster and sets the
# time dimension if not already present. For daily rasters the full date is
# used; for monthly rasters only the first of the month.
.load_climate_raster <- function(paths, span, daily = TRUE) {
  r <- .safe_rast(paths)
  if (!inherits(terra::time(r), "POSIXt")) {
    r <- raster_timestamp(
      r,
      days   = if (daily) format(span, "%d") else "01",
      months = format(span, "%m"),
      years  = format(span, "%Y"),
      span   = span
    )
  }
  r
}


# Computes the baseline date span from a baseline year range and an
# observation span. For daily data the month-day is preserved; for monthly
# data dates are normalised to the first of the month.
.compute_baseline_span <- function(baseline, obs_span, daily = TRUE) {
  baseline_years <- format(
    make_dates(seq(baseline[1], baseline[2]), months = 1, days = 1),
    "%Y"
  )
  sort(unique(do.call(c, lapply(baseline_years, function(y) {
    if (daily) {
      as.Date(paste(y, format(obs_span, "%m-%d"), sep = "-"))
    } else {
      as.Date(paste(y, format(obs_span, "%m"), "01", sep = "-"))
    }
  }))))
}


# Writes metadata columns to an sf result dataframe. Returns the modified
# dataframe.
.write_metadata_sf <- function(.data,
                               prefix,
                               indicator,
                               catalogue,
                               baseline,
                               stat_wrangling,
                               study_fun_name,
                               baseline_fun_name,
                               time_span,
                               time_lag,
                               buffer,
                               time_unit,
                               months = NULL) {
  .data[[.col("indicator",      prefix)]] <- indicator
  .data[[.col("unit",           prefix)]] <-
    .indicator_units[[indicator]] %||% NA_character_
  .data[[.col("resolution",     prefix)]] <-
    .catalogue_resolution[[catalogue]] %||% NA_character_
  .data[[.col("time_unit",      prefix)]] <- time_unit
  .data[[.col("result_unit",    prefix)]] <-
    if (isFALSE(baseline)) NA_character_ else .result_unit(stat_wrangling, indicator)
  .data[[.col("study_fun",      prefix)]] <- study_fun_name
  .data[[.col("baseline_fun",   prefix)]] <-
    if (isFALSE(baseline)) NA_character_ else baseline_fun_name
  .data[[.col("baseline_years", prefix)]] <-
    if (isFALSE(baseline)) NA_character_ else paste0(baseline[1], "-", baseline[2])
  .data[[.col("time_span",      prefix)]] <- time_span
  if (!is.null(months)) {
    .data[[.col("months",       prefix)]] <-
      if (is.null(months)) NA_character_ else paste(months, collapse = ",")
  }
  .data[[.col("time_lag",       prefix)]] <- time_lag
  .data[[.col("buffer",         prefix)]] <- buffer
  .data[[.col("source",         prefix)]] <-
    .catalogue_citation(catalogue, indicator)
  .data
}
