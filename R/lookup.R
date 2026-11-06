#' List all available indicators
#'
#' @description Returns a dataframe of all indicators available in gxc,
#'   grouped by catalogue.
#'
#' @returns A dataframe with columns `catalogue`, `source`, and `indicator`.
#'
#' @export
#'
#' @examples
#' gxc_indicators()
gxc_indicators <- function() {
  rows <- lapply(names(allowed_indicators_by_catalogue), function(cat) {
    data.frame(
      catalogue  = cat,
      source     = .catalogue_source(cat),
      indicator  = allowed_indicators_by_catalogue[[cat]],
      stringsAsFactors = FALSE
    )
  })
  as_data_frame(do.call(rbind, rows))
}


#' Look up metadata for a specific indicator
#'
#' @description Returns metadata for a given indicator and how it differs
#'   between the catalogues that provide it: temporal and spatial resolution,
#'   coverage, available daily statistics, and the unit of the returned
#'   values.
#'
#' @param indicator Character string giving the indicator name,
#'   e.g. `"2m_temperature"`.
#'
#' @returns A list with indicator metadata, printed in a human-readable format.
#'   The element `details` is a dataframe with one row per catalogue.
#'
#' @export
#'
#' @examples
#' lookup("2m_temperature")
#' lookup("air_temperature_mean")
#' lookup("total_precipitation")
lookup <- function(indicator) {
  # Find all catalogues that contain this indicator
  catalogues <- names(Filter(
    function(inds) indicator %in% inds,
    allowed_indicators_by_catalogue
  ))

  if (length(catalogues) == 0) {
    cli::cli_abort(c(
      "Indicator {.val {indicator}} not found.",
      "i" = "Use {.fn gxc_indicators} to see all available indicators."
    ))
  }

  details <- do.call(rbind, lapply(catalogues, .catalogue_details, indicator = indicator))

  out <- list(
    indicator  = indicator,
    unit       = .indicator_units[[indicator]] %||% NA_character_,
    source     = unique(sapply(catalogues, .catalogue_source)),
    catalogues = catalogues,
    details    = as_data_frame(details)
  )
  class(out) <- "gxc_indicator"
  out
}


# One row of catalogue-specific metadata for an indicator.
.catalogue_details <- function(catalogue, indicator) {
  daily_era5 <- catalogue %in% allowed_catalogues_daily &&
    .catalogue_source(catalogue) == "era5"
  accum      <- indicator %in% .accumulated_indicators

  statistics <- if (daily_era5) {
    if (accum) allowed_statistic else setdiff(allowed_statistic, "daily_sum")
  } else {
    character(0)
  }

  unit <- .indicator_unit(indicator, catalogue, if (daily_era5) "daily_mean")
  if (daily_era5 && accum) {
    unit <- paste0(unit, " (daily_sum: ", .indicator_unit(indicator, catalogue, "daily_sum"), ")")
  }

  data.frame(
    catalogue  = catalogue,
    source     = .catalogue_source(catalogue),
    temporal   = if (catalogue %in% allowed_catalogues_daily) "daily" else "monthly",
    resolution = .catalogue_resolution[[catalogue]] %||% NA_character_,
    coverage   = .catalogue_coverage[[catalogue]] %||% NA_character_,
    statistics = if (length(statistics)) paste(statistics, collapse = ", ") else NA_character_,
    unit       = unit,
    note       = .unit_note(indicator, catalogue),
    stringsAsFactors = FALSE
  )
}


# Explains what values of accumulated quantities represent per catalogue.
.unit_note <- function(indicator, catalogue) {
  if (indicator %in% .accumulated_indicators) {
    if (catalogue %in% allowed_catalogues_monthly) return("Monthly mean of daily totals.")
    return("Hourly amounts; use `statistic = \"daily_sum\"` for daily totals.")
  }
  if (indicator == "precipitation") {
    return(if (catalogue == "dwd-monthly") "Monthly totals." else "Daily totals.")
  }
  NA_character_
}


#' @export
format.gxc_indicator <- function(x, ...) {
  label <- function(name, value) {
    if (is.na(value)) return(NULL)
    paste0("    ", cli::style_bold(formatC(paste0(name, ":"), width = -12)), value)
  }

  blocks <- vapply(seq_len(nrow(x$details)), function(i) {
    d <- x$details[i, ]
    paste(c(
      paste0("  ", cli::style_bold(d$catalogue), " (", d$source, ", ", d$temporal, ")"),
      label("Resolution", paste0(d$resolution, ", ", d$coverage)),
      label("Statistics", d$statistics),
      label("Unit", d$unit),
      label("Note", d$note)
    ), collapse = "\n")
  }, character(1))

  paste0(
    cli::style_bold("Indicator: "), x$indicator, "\n",
    cli::style_bold("Catalogues:"), "\n",
    paste(blocks, collapse = "\n\n")
  )
}

#' @export
print.gxc_indicator <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}



#' lookup <- function(indicator) {
#'   index <- read_indicators()
#'   out <- index[[indicator]]
#'   class(out) <- "gxc_indicator"
#'   out
#' }
#'
#'
#' gxc_indicators <- function() {
#'   index <- read_indicators()
#'   cats <- vapply(index, \(x) x$category, character(1))
#'   out <- data.frame(category = cats, indicator = names(index), row.names = NULL)
#'   out <- out[order(out$category, decreasing = TRUE), ]
#'   as_df(out)
#' }
#'
#'
#' read_indicators <- function() {
#'   index_path <- system.file("indicators.json", package = "gxc")
#'   index <- jsonlite::read_json(index_path)
#' }
#'
#'
#' #' @export
#' format.gxc_indicator <- function(x, ...) {
#'   showcase <- c(
#'     "name", "category", "data_type", "unit", "spatial_resolution",
#'     "spatial_coverage", "temporal_resolution", "temporal_coverage"
#'   )
#'   showcase_fmt <- c(
#'     "Indicator", "Category", "Data type", "Unit", "Spatial resolution",
#'     "Spatial coverage", "Temporal resolution", "Temporal coverage"
#'   )
#'   max_nc <- max(nchar(showcase_fmt))
#'
#'   foi <- x[showcase]
#'   foi <- lapply(seq_along(foi), function(i) {
#'     x <- foi[[i]]
#'
#'     if (length(x) < 1) {
#'       x <- "N/A"
#'     }
#'
#'     if (length(x) > 1) {
#'       spaces <- strrep(" ", max_nc + 2)
#'       x[1] <- paste0("- ", x[1])
#'       x[-1] <- paste0(spaces, "- ", x[-1])
#'       x <- paste(x, collapse = "\n")
#'     } else {
#'       spaces <- strrep(" ", max_nc - nchar(showcase_fmt[i]))
#'       x <- paste0(spaces, x)
#'     }
#'
#'     x
#'   })
#'
#'   showcase_fmt <- cli::style_bold(paste0(showcase_fmt, ":"))
#'   paste(showcase_fmt, foi, collapse = "\n")
#' }
#'
#'
#'
#' #' @export
#' print.gxc_indicator <- function(x, ...) {
#'   cat(format(x), "\n", ...)
#'   invisible(x)
#' }
