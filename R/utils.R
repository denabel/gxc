#' Dispatch function that displays a message using cli functions only if
#' verbosity is enabled. If not explicitly provided, the function reads
#' a `verbose` object from the parent frame.
#' @param text A text to display
#' @param ... Arguments passed to cli functions
#' @param level info, warning, danger, success or step. Corresponds to
#' respective cli functions
#' @param verbose If FALSE, does nothing.
#' @param .envir Environment from which to detect `verbose`
#' @returns Nothing.
#' @noRd
info <- function(text,
                 ...,
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
    pg_gen
  )

  fun(text, ..., .envir = .envir)
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
as_sf_tibble <- function(x) {
  sf::st_as_sf(tibble::as_tibble(x))
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
  as.character(sort(unique(x)))
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


make_dates <- function(years, months = NULL, days = NULL, unlist = TRUE) {
  months <- months %||% 1:12
  ymd <- expand.grid(year = years, month = months)
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


make_date <- function(year, month, day) {
  as.POSIXct(sprintf("%04d-%02d-%02d", year, month, day))
}


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


is_terra <- function(x) {
  inherits(x, c("SpatRaster", "SpatVector"))
}


is_sf <- function(x) {
  inherits(x, "sf")
}


local_cdf <- function(raster, path = tempfile()) {
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

  do.call(
    on.exit,
    list(substitute(unlink(temp_file)), add = TRUE),
    envir = parent.frame()
  )
}


# Raster processing helpers -----------------------------------------------

#' Fix timestamp in raster files downloaded from CDS
#'
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
.raster_timestamp <- function(raster, days, months, years) {
  terra::depth(raster) <- NULL
  valid_dates <- make_dates(years, months, days)
  valid_dates <- format(sort(valid_dates), "%Y-%m-%d")
  terra::time(raster) <- as.Date(valid_dates)
  raster
}


#' Function to sanitize the metadata of a SpatRaster, i.e., remove all metags
#' that contain disallowed characters.
#' @param raster A SpatRaster
#' @returns A SpatRaster
#' @noRd
metags_sanitize <- function(raster) {
  meta <- terra::metags(raster)
  empty <- meta

  if (utils::packageVersion("terra") > "1.8-42") {
    empty[, 2] <- ""
    terra::metags(raster) <- empty
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta$value), ]
  } else {
    terra::metags(raster) <- NULL
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta)]
  }

  raster
}


# Data extraction helpers -------------------------------------------------

#' Focal extraction
#' @noRd
.toi_extract <- function(raster,
                         raster_path,
                         data_sf,
                         time_span = 0,
                         parallel = FALSE,
                         chunk_size = 50) {
  if (parallel) {
    chunks <- split(
      seq_len(nrow(data_sf)),
      ceiling(seq_len(nrow(data_sf)) / chunk_size)
    )
  }

  # Extract values from raster for each observation and add to dataframe
  if (length(unique(data_sf$link_date)) == 1 && time_span == 0) {
    # All observations have the same link date and direct link to focal month
    terra::extract(
      raster,
      data_sf,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else if (length(unique(data_sf$link_date)) > 1 && time_span == 0) {
    # All observations have different link dates and direct link to focal month
    if (!parallel) {
      .toi_extract_impl(raster, data_sf)
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(raster_path, data_sf, idx = chunk),
        future.seed = TRUE
      )
      unlist(raster_values, recursive = FALSE)
    }

  } else if (length(unique(data_sf$link_date)) >= 1 & time_span > 0) {
    # All observations have different link dates and mean calculation of focal months
    if (!parallel) {
      .toi_extract_impl(raster, data_sf, agg = TRUE)
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path,
          data_sf,
          idx = chunk,
          agg = TRUE
        ),
        future.seed = TRUE
      )
      unlist(raster_values, recursive = FALSE)
    }
  }
}


#' Focal extraction for gridded data input
#' @param ind_raster Raster containing EOD indicator data
#' @param obs_raster Raster containing input observations
#' @param temporals Dataframe containing time information constructed by
#'   .transform_time()
#' @returns A SpatRaster
#' @noRd
.toi_extract_grid <- function(ind_raster,
                              obs_raster,
                              temporals,
                              time_span = 0,
                              parallel = FALSE,
                              chunk_size = 50,
                              method = "bilinear") {
  dates <- terra::time(ind_raster)

  if (parallel) {
    chunks <- terra::split(
      obs_raster,
      ceiling(ncell(obs_raster) / chunk_size)
    )
  }

  if (time_span == 0) {
    # For a single date, all grid cells share the same link_date.
    target_date <- unique(temporals$link_date)
    if (length(target_date) != 1) {
      cli::cli_abort(c(
        paste(
          "For gridded data with `timespan == 0`, the time",
          "dimension cannot vary across the grid."
        ),
        "i" = "This means, that `terra::time()` should return a single unique time."
      ))
    }
    lyr_idx <- which(dates == target_date)[[1]]
    toi_layer <- ind_raster[[lyr_idx]]
    terra::resample(toi_layer, obs_raster, method = method)
  } else {
    # For time_span > 0, all grid cells share the same time_span_seq.
    seq_dates <- unique(temporals$time_span_seq)
    if (length(unique_seq) != 1) {
      cli::cli_abort(paste(
        "For gridded data with `timespan > 0`,",
        "all cells should share the same time span."
      ))
    }

    lyr_idx <- which(raster_dates %in% seq_dates)
    subset_avg <- terra::app(ind_raster[[lyr_idx]], mean, na.rm = TRUE)
    terra::resample(subset_avg, obs_raster, method = method)
  }
}


#' Low-level extraction function
#' @param raster SpatRaster of path to a raster file. For parallelization,
#' a path must be provided.
#' @param vector An sf dataframe containing polygons or points and a column
#' `link_date`.
#' @param idx A vector of indices in `vector` to extract. Only necessary
#' for parallelized runs.
#' @param agg Whether to aggregate the years in a given time span. Requires
#' a column `time_span_seq` in `vector`.
#' @param baseline Whether to aggregate across baseline years.
#' @returns A named list.
#' @noRd
.toi_extract_impl <- function(raster,
                              vector,
                              idx = NULL,
                              agg = FALSE,
                              baseline = FALSE) {
  if (is.character(raster)) {
    raster <- terra::rast(raster)
  }

  dates <- terra::time(raster)

  if (baseline) {
    month <- month(dates)
    day <- day(dates)
  }

  vals <- lapply(seq_len(nrow(vector)), function(i) {
    vector_sliced <- vector[i, ]
    if (agg) {
      # if a time span is specified, compute the average across
      lyr_idx <- which(dates %in% vector_sliced$time_span_seq)
      raster <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
    } else if (baseline) {
      # for baseline calculations, compute the average across the same dates
      # of different years, e.g. 2014-01-01, 2015-01-01, ...
      target_dates <- unlist(vector_sliced$time_span_seq)
      target_md <- paste(month(target_dates), day(target_dates), sep = "-")
      baseline_md <- paste(month, day, sep = "-")
      lyr_idx <- which(baseline_md %in% target_md)
      raster <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
    } else {
      lyr_idx <- which(dates == vector_sliced$link_date)
      raster <- raster[[lyr_idx]]
    }

    terra::extract(
      raster,
      vector_sliced,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  })

  unlist(vals, recursive = FALSE)
}
