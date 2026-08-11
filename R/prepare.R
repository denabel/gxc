#' Creates the bbox of an sf object in order north, west, south, east
#' @param x sf dataframe
#' @returns A vector of length 4
#' @noRd
.get_extent <- function(.data, grid_resolution = 0.1) {
  .snap_to_grid <- function(x, resolution, direction = c("floor", "ceiling")) {
    direction <- match.arg(direction)
    if (direction == "floor") floor(x / resolution) * resolution
    else ceiling(x / resolution) * resolution
  }

  if (is_sf(.data)) {
    box  <- sf::st_bbox(.data)
    ymax <- .snap_to_grid(box$ymax, grid_resolution, "ceiling")
    xmin <- .snap_to_grid(box$xmin, grid_resolution, "floor")
    ymin <- .snap_to_grid(box$ymin, grid_resolution, "floor")
    xmax <- .snap_to_grid(box$xmax, grid_resolution, "ceiling")
  } else if (is_terra(.data)) {
    box  <- terra::ext(.data)
    ymax <- .snap_to_grid(box[4], grid_resolution, "ceiling")
    xmin <- .snap_to_grid(box[1], grid_resolution, "floor")
    ymin <- .snap_to_grid(box[3], grid_resolution, "floor")
    xmax <- .snap_to_grid(box[2], grid_resolution, "ceiling")
  }

  # Point case: bbox has no extent → one grid step in each direction
  if (xmin >= xmax) { xmin <- xmin - grid_resolution; xmax <- xmax + grid_resolution }
  if (ymin >= ymax) { ymin <- ymin - grid_resolution; ymax <- ymax + grid_resolution }

  c(ymax, xmin, ymin, xmax)
}


#' Resolve explicit month vector relative to a reference date
#' @param date A Date object
#' @param months Integer vector of months (e.g. c(5, 6, 7))
#' @returns A Date vector (first day of each resolved month)
#' @noRd
.resolve_months <- function(date, months) {
  current_month <- lubridate::month(date)
  current_year  <- lubridate::year(date)

  # If any requested month overlaps with current month, shift window one year back
  ref_year <- if (any(months %in% current_month)) {
    current_year - 1L
  } else {
    current_year
  }

  # Assign years sequentially, detecting year boundary crossings
  years <- integer(length(months))
  y     <- ref_year
  for (i in seq_along(months)) {
    if (i > 1L && months[i] < months[i - 1L]) y <- y + 1L
    years[i] <- y
  }

  as.Date(paste(years, months, "01", sep = "-"))
}


#' Construct time columns for linking
#' @param .data An sf or SpatRaster object
#' @param date_var Name of the date column (sf only)
#' @param time_span Integer time span
#' @param time_lag Integer time lag
#' @param months Optional integer vector of months
#' @param by Sequence interval
#' @returns Modified .data with time columns added
#' @noRd
.transform_time <- function(.data,
                            date_var        = "date",
                            time_span       = 0,
                            time_lag        = 0,
                            months          = NULL,
                            by              = "1 day",
                            daily_expansion = FALSE,
                            time_lag_unit   = c("days", "months")) {
  time_lag_unit <- match.arg(time_lag_unit)

  if (is_sf(.data)) {
    .data$link_date <- .data[[date_var]]
  } else if (is_terra(.data)) {
    .data <- data.frame(link_date = terra::time(.data))
  }

  .data$link_date <- as_date(.data$link_date)

  # link_daily() shifts by DAYS (unchanged); link_monthly() shifts by
  # calendar MONTHS instead -- shifting by "time_lag days" made no sense
  # for monthly-resolution matching: since the shifted date gets
  # normalized to its first-of-month afterward, a day-based shift either
  # had NO effect (if it didn't cross a month boundary) or jumped a full
  # month (if it did, e.g. any date on the 1st) -- unpredictable
  # depending on which day of the month the input date happened to fall
  # on. Implemented manually via year/month/day arithmetic (not
  # lubridate's period constructors/`%m-%`, to avoid depending on the
  # exact set of period-constructor functions lubridate happens to
  # export in a given version) with day clamped to the last valid day of
  # the resulting month, for correct calendar-aware rollback at
  # month-end dates (e.g. shifting back a month from March 31 lands on
  # the last day of February, rather than producing NA).
  .shift_months_back <- function(date, n) {
    y <- year(date)
    m <- month(date)
    d <- day(date)

    total_months <- (y * 12L + (m - 1L)) - n
    new_y <- total_months %/% 12L
    new_m <- total_months %% 12L + 1L

    # Last valid day of the resulting month = one day before the 1st of
    # the FOLLOWING month -- computed via plain integer arithmetic on
    # year/month (not months()/period constructors), then a single
    # days(1) subtraction (days() is already used elsewhere in this file
    # and is definitely exported, unlike lubridate's month-period
    # constructors).
    next_total <- new_y * 12L + (new_m - 1L) + 1L
    next_y     <- next_total %/% 12L
    next_m     <- next_total %% 12L + 1L
    last_day   <- as.integer(format(
      as_date(sprintf("%04d-%02d-01", next_y, next_m)) - days(1), "%d"
    ))

    new_d <- pmin(d, last_day)

    as_date(sprintf("%04d-%02d-%02d", new_y, new_m, new_d))
  }

  .data$link_date <- if (time_lag_unit == "months") {
    .shift_months_back(.data$link_date, time_lag)
  } else {
    .data$link_date - days(time_lag)
  }

  if (!is.null(months)) {
    if (daily_expansion) {
      # link_daily() case: full daily sequence spanning the resolved
      # months, e.g. months = c(3,4,5) -> every day from March 1 through
      # May 31 (year-adjusted per .resolve_months()'s existing logic).
      .data$time_span_seq <- lapply(.data$link_date, function(d) {
        resolved   <- .resolve_months(d, months)
        start_date <- min(resolved)
        end_date   <- lubridate::ceiling_date(max(resolved), "month") - 1
        format(seq(start_date, end_date, by = "1 day"), "%Y-%m-%d")
      })
      .data$link_date_end <- as_date(
        sapply(.data$time_span_seq, function(x) x[length(x)])
      )
    } else {
      # link_monthly() case: unchanged -- one anchor date per month,
      # sufficient for matching against monthly-resolution rasters.
      .data$time_span_seq <- lapply(.data$link_date, function(d) {
        resolved <- .resolve_months(d, months)
        format(resolved, "%Y-%m-%d")
      })
      .data$link_date_end <- as_date(
        sapply(.data$time_span_seq, function(x) x[1])
      )
    }
  } else {
    .data$link_date_end <- .data$link_date - days(time_span)
    .data$time_span_seq <- Map(
      .data$link_date_end,
      .data$link_date,
      f = function(end, start) {
        format(seq(end, start, by = by), "%Y-%m-%d")
      }
    )
  }

  .data
}


.align_crs_raster <- function(x, y) {
  if (!identical(terra::crs(x), terra::crs(y))) {
    info("Reprojecting indicator raster to match input data", level = "warning")
    y <- terra::project(y, y = x)
  }
  y
}


.align_crs_vector <- function(x, y) {
  if (!identical(terra::crs(x), terra::crs(y))) {
    x <- sf::st_transform(x, crs = terra::crs(y))
  }
  x
}
