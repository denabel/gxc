lookup <- function(indicator) {
  index <- read_indicators()
  out <- index[[indicator]]
  class(out) <- "gxc_indicator"
  out
}


gxc_indicators <- function() {
  index <- read_indicators()
  cats <- vapply(index, \(x) x$category, character(1))
  out <- data.frame(category = cats, indicator = names(index), row.names = NULL)
  out <- out[order(out$category, decreasing = TRUE), ]
  as_df(out)
}


read_indicators <- function() {
  index_path <- system.file("indicators.json", package = "gxc")
  index <- jsonlite::read_json(index_path)
}


#' @export
format.gxc_indicator <- function(x, ...) {
  showcase <- c(
    "name", "category", "data_type", "unit", "spatial_resolution",
    "spatial_coverage", "temporal_resolution", "temporal_coverage"
  )
  showcase_fmt <- c(
    "Indicator", "Category", "Data type", "Unit", "Spatial resolution",
    "Spatial coverage", "Temporal resolution", "Temporal coverage"
  )
  max_nc <- max(nchar(showcase_fmt))

  foi <- x[showcase]
  foi <- lapply(seq_along(foi), function(i) {
    x <- foi[[i]]

    if (length(x) < 1) {
      x <- "N/A"
    }

    if (length(x) > 1) {
      spaces <- strrep(" ", max_nc + 2)
      x[1] <- paste0("- ", x[1])
      x[-1] <- paste0(spaces, "- ", x[-1])
      x <- paste(x, collapse = "\n")
    } else {
      spaces <- strrep(" ", max_nc - nchar(showcase_fmt[i]))
      x <- paste0(spaces, x)
    }

    x
  })

  showcase_fmt <- cli::style_bold(paste0(showcase_fmt, ":"))
  paste(showcase_fmt, foi, collapse = "\n")
}



#' @export
print.gxc_indicator <- function(x, ...) {
  cat(format(x), "\n", ...)
  invisible(x)
}
