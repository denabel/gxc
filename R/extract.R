.toi_extract_baseline <- function(.data,
                                  raster,
                                  baseline_fun,
                                  stat_wrangling = "deviation",
                                  focal_values   = NULL,
                                  parallel       = FALSE,
                                  chunk_size     = 50) {
  if (!parallel) {
    .toi_extract_impl(
      raster, .data,
      baseline       = TRUE,
      baseline_fun   = baseline_fun,
      stat_wrangling = stat_wrangling,
      focal_values   = focal_values
    )
  } else {
    chunks <- split(
      seq_len(nrow(.data)),
      ceiling(seq_len(nrow(.data)) / chunk_size)
    )
    raster_values <- future.apply::future_lapply(
      chunks,
      function(chunk) {
        .toi_extract_impl(
          raster,
          .data[chunk, ],
          baseline       = TRUE,
          baseline_fun   = baseline_fun,
          stat_wrangling = stat_wrangling,
          focal_values   = focal_values[chunk]
        )
      },
      future.seed = TRUE
    )
    do.call(c, raster_values)
  }
}


.toi_extract_grid_baseline <- function(.data,
                                       raster,
                                       temporals,
                                       time_span      = 0,
                                       stat_wrangling = "deviation",
                                       parallel       = FALSE,
                                       chunk_size     = 50) {
  .toi_extract_grid(
    .data,
    raster,
    temporals,
    agg            = time_span > 0,
    baseline       = TRUE,
    stat_wrangling = stat_wrangling,
    parallel       = parallel,
    chunk_size     = chunk_size
  )
}


#' Focal extraction
#' @noRd
.toi_extract <- function(.data,
                         raster,
                         raster_path,
                         time_span      = 0,
                         parallel       = FALSE,
                         chunk_size     = 50,
                         baseline_fun,
                         stat_wrangling = "deviation") {
  if (parallel) {
    chunks <- split(
      seq_len(nrow(.data)),
      ceiling(seq_len(nrow(.data)) / chunk_size)
    )
  }

  if (length(unique(.data$link_date)) == 1 && time_span == 0) {
    # All observations share the same link date — select correct layer first
    dates     <- as_date(terra::time(raster))
    link_date <- unique(.data$link_date)

    # Try exact match first, then normalize to first of month (for monthly rasters)
    lyr_idx <- which(dates == link_date)
    if (length(lyr_idx) == 0) {
      link_month <- as.Date(format(link_date, "%Y-%m-01"))
      lyr_idx    <- which(dates == link_month)
    }

    if (length(lyr_idx) == 0) {
      cli::cli_abort(c(
        "Could not find a matching layer for date {.val {link_date}}.",
        "i" = "Available dates: {.val {as.character(dates)}}"
      ))
    }

    terra::extract(
      raster[[lyr_idx[[1]]]],
      .data,
      fun   = mean,
      na.rm = TRUE,
      ID    = FALSE
    )

  } else if (length(unique(.data$link_date)) > 1 && time_span == 0) {
    if (!parallel) {
      .toi_extract_impl(
        raster, .data,
        baseline_fun   = baseline_fun,
        stat_wrangling = stat_wrangling
      )
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path, .data[chunk, ],
          baseline_fun   = baseline_fun,
          stat_wrangling = stat_wrangling
        ),
        future.seed     = TRUE,
        future.packages = "sf"
      )
      do.call(c, raster_values)
    }

  } else if (length(unique(.data$link_date)) >= 1 && time_span > 0) {
    if (!parallel) {
      .toi_extract_impl(
        raster, .data,
        agg            = TRUE,
        baseline_fun   = baseline_fun,
        stat_wrangling = stat_wrangling
      )
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path, .data[chunk, ],
          agg            = TRUE,
          baseline_fun   = baseline_fun,
          stat_wrangling = stat_wrangling
        ),
        future.seed     = TRUE,
        future.packages = "sf"
      )
      do.call(c, raster_values)
    }
  }
}


#' Focal extraction for gridded data input
#' @param .data Raster containing input observations
#' @param ind_raster Raster containing EOD indicator data
#' @param temporals Dataframe containing time information constructed by
#'   .transform_time()
#' @returns A SpatRaster
#' @noRd
.toi_extract_grid <- function(.data,
                              raster,
                              temporals,
                              agg            = FALSE,
                              baseline       = FALSE,
                              stat_wrangling = "deviation",
                              parallel       = FALSE,
                              chunk_size     = 50,
                              method         = "bilinear") {
  dates <- as_date(terra::time(raster))

  # Detect whether raster has monthly or daily resolution
  is_monthly <- all(as.integer(format(dates, "%d")) == 1) &&
    length(unique(format(dates, "%Y-%m"))) == length(dates)

  # Normalize raster dates for matching
  dates_norm <- if (is_monthly) {
    as.Date(format(dates, "%Y-%m-01"))
  } else {
    dates
  }

  if (parallel) {
    chunks <- terra::split(.data, ceiling(terra::ncell(.data) / chunk_size))
  }

  if (agg) {
    seq_dates <- unique(temporals$time_span_seq)
    if (length(seq_dates) != 1) {
      cli::cli_abort(paste(
        "For gridded data with `timespan > 0`,",
        "all cells should share the same time span."
      ))
    }
  } else {
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
  }

  if (agg && !baseline) {
    target_dates <- as_date(unlist(seq_dates))
    target_norm  <- if (is_monthly) {
      as.Date(format(target_dates, "%Y-%m-01"))
    } else {
      target_dates
    }
    lyr_idx <- which(dates_norm %in% target_norm)

    if (stat_wrangling %in% c("count_above", "count_below")) {
      toi_layer <- raster[[lyr_idx]]
    } else {
      toi_layer <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
    }

  } else if (agg && baseline) {
    target_dates <- as_date(unlist(seq_dates))
    if (is_monthly) {
      target_m    <- month(target_dates)
      baseline_m  <- month(dates_norm)
      lyr_idx     <- which(baseline_m %in% target_m)
    } else {
      seq_md      <- paste(month(target_dates), day(target_dates), sep = "-")
      baseline_md <- paste(month(dates), day(dates), sep = "-")
      lyr_idx     <- which(baseline_md %in% seq_md)
    }
    toi_layer <- raster[[lyr_idx]]

  } else if (!agg && baseline) {
    target_dates <- as_date(unlist(temporals$time_span_seq))
    if (is_monthly) {
      target_m    <- month(target_dates)
      baseline_m  <- month(dates_norm)
      lyr_idx     <- which(baseline_m %in% target_m)
    } else {
      target_md   <- paste(month(target_dates), day(target_dates), sep = "-")
      baseline_md <- paste(month(dates), day(dates), sep = "-")
      lyr_idx     <- which(baseline_md %in% target_md)
    }
    toi_layer <- raster[[lyr_idx]]

  } else {
    # Single layer — normalize link_date for monthly rasters
    target_norm <- if (is_monthly) {
      as.Date(format(target_date, "%Y-%m-01"))
    } else {
      target_date
    }
    lyr_idx   <- which(dates_norm == target_norm)[[1]]
    toi_layer <- raster[[lyr_idx]]
  }

  terra::resample(toi_layer, .data, method = method)
}


#' Low-level extraction function
#' @param raster SpatRaster or path to a raster file. For parallelization,
#'   a path must be provided.
#' @param vector An sf dataframe containing polygons or points and a column
#'   `link_date`.
#' @param agg Whether to aggregate the years in a given time span. Requires
#'   a column `time_span_seq` in `vector`.
#' @param baseline Whether to aggregate across baseline years.
#' @returns A named list.
#' @noRd
.toi_extract_impl <- function(raster,
                              vector,
                              agg            = FALSE,
                              baseline_fun,
                              baseline       = FALSE,
                              stat_wrangling = "deviation",
                              focal_values   = NULL) {
  requireNamespace("sf", quietly = TRUE)

  if (is.character(raster)) {
    raster <- terra::rast(raster)
  }

  dates <- as_date(terra::time(raster))

  # Detect whether raster has monthly or daily resolution
  is_monthly <- all(as.integer(format(dates, "%d")) == 1) &&
    length(unique(format(dates, "%Y-%m"))) == length(dates)

  # Normalize dates for matching
  dates_norm <- if (is_monthly) {
    as.Date(format(dates, "%Y-%m-01"))
  } else {
    dates
  }

  if (baseline) {
    month <- month(dates)
    day   <- day(dates)
  }

  # Helper: check if all rows share the same time_span_seq
  .all_same_seq <- function(v) {
    length(unique(sapply(v$time_span_seq, paste, collapse = "-"))) == 1
  }

  if (agg) {
    if (.all_same_seq(vector)) {
      target_dates <- as_date(unlist(vector$time_span_seq[[1]]))
      target_norm  <- if (is_monthly) {
        as.Date(format(target_dates, "%Y-%m-01"))
      } else {
        target_dates
      }
      lyr_idx <- which(dates_norm %in% target_norm)

      if (length(lyr_idx) == 0) {
        return(lapply(seq_len(nrow(vector)), function(i) NA_real_))
      }

      if (stat_wrangling %in% c("count_above", "count_below")) {
        # Return uncollapsed focal values per point
        lapply(seq_len(nrow(vector)), function(i) {
          sapply(lyr_idx, function(idx) {
            terra::extract(
              raster[[idx]],
              vector[i, ],
              fun   = mean,
              na.rm = TRUE,
              ID    = FALSE
            )[1, 1]
          })
        })
      } else {
        # Aggregate once, extract all points at once
        raster_agg <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
        result     <- terra::extract(
          raster_agg, vector, fun = mean, na.rm = TRUE, ID = FALSE
        )
        lapply(seq_len(nrow(result)), function(i) result[i, , drop = FALSE])
      }

    } else {
      # Different time_span_seq per row — row-wise loop
      lapply(seq_len(nrow(vector)), function(i) {
        vector_sliced <- vector[i, ]
        target_dates  <- as_date(unlist(vector_sliced$time_span_seq))
        target_norm   <- if (is_monthly) {
          as.Date(format(target_dates, "%Y-%m-01"))
        } else {
          target_dates
        }
        lyr_idx <- which(dates_norm %in% target_norm)

        if (length(lyr_idx) == 0) return(NA_real_)

        if (stat_wrangling %in% c("count_above", "count_below")) {
          sapply(lyr_idx, function(idx) {
            terra::extract(
              raster[[idx]],
              vector_sliced,
              fun   = mean,
              na.rm = TRUE,
              ID    = FALSE
            )[1, 1]
          })
        } else {
          raster_agg <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
          terra::extract(
            raster_agg,
            vector_sliced,
            fun   = mean,
            na.rm = TRUE,
            ID    = FALSE
          )
        }
      })
    }

  } else if (baseline) {
    if (.all_same_seq(vector)) {
      target_dates <- as_date(unlist(vector$time_span_seq[[1]]))

      if (is_monthly) {
        lyr_idx <- which(month(dates_norm) %in% month(target_dates))
      } else {
        target_md   <- paste(month(target_dates), day(target_dates), sep = "-")
        baseline_md <- paste(month, day, sep = "-")
        lyr_idx     <- which(baseline_md %in% target_md)
      }

      if (length(lyr_idx) == 0) {
        return(lapply(seq_len(nrow(vector)), function(i) {
          list(reference_stat = NA_real_, result = NA_real_)
        }))
      }

      # Extract all baseline layers for all points at once
      baseline_matrix <- terra::extract(
        raster[[lyr_idx]],
        vector,
        fun   = NULL,
        na.rm = TRUE,
        ID    = FALSE
      )

      lapply(seq_len(nrow(vector)), function(i) {
        baseline_values <- as.numeric(baseline_matrix[i, ])
        focal_val <- if (
          stat_wrangling %in% c("count_above", "count_below") &&
          !is.null(focal_values)
        ) {
          focal_values[[i]]
        } else {
          vector$.linked[i]
        }
        compute_stat_wrangling(
          baseline_values = baseline_values,
          focal_value     = focal_val,
          stat_wrangling  = stat_wrangling,
          baseline_fun    = baseline_fun
        )
      })

    } else {
      # Different time_span_seq per row — row-wise loop
      lapply(seq_len(nrow(vector)), function(i) {
        vector_sliced <- vector[i, ]
        target_dates  <- as_date(unlist(vector_sliced$time_span_seq))

        if (is_monthly) {
          target_m    <- month(target_dates)
          baseline_m  <- month(dates_norm)
          lyr_idx     <- which(baseline_m %in% target_m)
        } else {
          target_md   <- paste(month(target_dates), day(target_dates), sep = "-")
          baseline_md <- paste(month, day, sep = "-")
          lyr_idx     <- which(baseline_md %in% target_md)
        }

        if (length(lyr_idx) == 0) {
          return(list(reference_stat = NA_real_, result = NA_real_))
        }

        baseline_values <- sapply(lyr_idx, function(idx) {
          terra::extract(
            raster[[idx]],
            vector_sliced,
            fun   = mean,
            na.rm = TRUE,
            ID    = FALSE
          )[1, 1]
        })

        focal_val <- if (
          stat_wrangling %in% c("count_above", "count_below") &&
          !is.null(focal_values)
        ) {
          focal_values[[i]]
        } else {
          vector_sliced$.linked
        }

        compute_stat_wrangling(
          baseline_values = baseline_values,
          focal_value     = focal_val,
          stat_wrangling  = stat_wrangling,
          baseline_fun    = baseline_fun
        )
      })
    }

  } else {
    lapply(seq_len(nrow(vector)), function(i) {
      vector_sliced <- vector[i, ]

      # Normalize link_date for monthly rasters
      link_date <- if (is_monthly) {
        as.Date(format(vector_sliced$link_date, "%Y-%m-01"))
      } else {
        vector_sliced$link_date
      }

      lyr_idx <- which(dates_norm == link_date)

      if (length(lyr_idx) == 0) return(NA_real_)

      terra::extract(
        raster[[lyr_idx[[1]]]],
        vector_sliced,
        fun   = mean,
        na.rm = TRUE,
        ID    = FALSE
      )
    })
  }
}
