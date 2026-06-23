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
#' @description Returns metadata for a given indicator, including its physical
#'   unit, available catalogues, and temporal resolution.
#'
#' @param indicator Character string giving the indicator name,
#'   e.g. `"2m_temperature"`.
#'
#' @returns A list with indicator metadata, printed in a human-readable format.
#'
#' @export
#'
#' @examples
#' lookup("2m_temperature")
#' lookup("air_temperature_mean")
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

  unit <- .indicator_units[[indicator]] %||% NA_character_

  out <- list(
    indicator  = indicator,
    unit       = unit,
    source     = unique(sapply(catalogues, .catalogue_source)),
    catalogues = catalogues
  )
  class(out) <- "gxc_indicator"
  out
}


#' @export
format.gxc_indicator <- function(x, ...) {
  catalogues_fmt <- paste0("  - ", x$catalogues, collapse = "\n")
  paste0(
    cli::style_bold("Indicator:  "), x$indicator, "\n",
    cli::style_bold("Unit:       "), x$unit %||% "unknown", "\n",
    cli::style_bold("Source:     "), paste(x$source, collapse = ", "), "\n",
    cli::style_bold("Catalogues:"), "\n",
    catalogues_fmt
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
