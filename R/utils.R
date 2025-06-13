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
as_sf_tibble <- function(x) {
  if (rlang::is_installed("tibble")) {
    sf::st_as_sf(tibble::as_tibble(x))
  } else {
    sf::st_as_sf(x)
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
raster_timestamp <- function(raster, days, months, years) {
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
  if (isTRUE(getOption("__gxc_fail_on_request__", FALSE))) {
    stop("Code has ben run in a test where this code should not be running!")
  }
}
