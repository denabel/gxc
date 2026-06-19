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
    dates   <- as_date(terra::time(raster))
    lyr_idx <- which(dates == unique(.data$link_date))[[1]]
    terra::extract(
      raster[[lyr_idx]],
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
    lyr_idx   <- which(dates %in% unlist(seq_dates))
    if (stat_wrangling %in% c("count_above", "count_below")) {
      # Return uncollapsed stack for count operations
      toi_layer <- raster[[lyr_idx]]
    } else {
      toi_layer <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
    }
  } else if (agg && baseline) {
    seq_md      <- paste(month(unlist(seq_dates)), day(unlist(seq_dates)), sep = "-")
    baseline_md <- paste(month(dates), day(dates), sep = "-")
    lyr_idx     <- which(baseline_md %in% seq_md)
    toi_layer   <- raster[[lyr_idx]]
  } else if (!agg && baseline) {
    target_dates <- unlist(temporals$time_span_seq)
    target_md    <- paste(month(target_dates), day(target_dates), sep = "-")
    baseline_md  <- paste(month(dates), day(dates), sep = "-")
    lyr_idx      <- which(baseline_md %in% target_md)
    toi_layer    <- raster[[lyr_idx]]
  } else {
    lyr_idx   <- which(dates == target_date)[[1]]
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

  if (baseline) {
    month <- month(dates)
    day   <- day(dates)
  }

  vals <- lapply(seq_len(nrow(vector)), function(i) {
    vector_sliced <- vector[i, ]

    if (agg) {
      target_dates <- as_date(unlist(vector_sliced$time_span_seq))
      lyr_idx      <- which(dates %in% target_dates)

      if (stat_wrangling %in% c("count_above", "count_below")) {
        # Return individual focal day values uncollapsed for count operations
        focal_vals <- sapply(lyr_idx, function(idx) {
          terra::extract(
            raster[[idx]],
            vector_sliced,
            fun   = mean,
            na.rm = TRUE,
            ID    = FALSE
          )[1, 1]
        })
        return(focal_vals)
      } else {
        raster_agg <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
        return(terra::extract(
          raster_agg,
          vector_sliced,
          fun   = mean,
          na.rm = TRUE,
          ID    = FALSE
        ))
      }

    } else if (baseline) {
      target_dates <- as_date(unlist(vector_sliced$time_span_seq))
      target_md    <- paste(month(target_dates), day(target_dates), sep = "-")
      baseline_md  <- paste(month, day, sep = "-")
      lyr_idx      <- which(baseline_md %in% target_md)

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

      # Use pre-extracted focal values for count operations
      focal_val <- if (
        stat_wrangling %in% c("count_above", "count_below") &&
        !is.null(focal_values)
      ) {
        focal_values[[i]]
      } else {
        vector_sliced$.linked
      }

      return(
        compute_stat_wrangling(
          baseline_values = baseline_values,
          focal_value     = focal_val,
          stat_wrangling  = stat_wrangling,
          baseline_fun    = baseline_fun
        )
      )

    } else {
      lyr_idx <- which(dates == vector_sliced$link_date)
      raster  <- raster[[lyr_idx]]
    }

    terra::extract(
      raster,
      vector_sliced,
      fun   = mean,
      na.rm = TRUE,
      ID    = FALSE
    )
  })

  vals
}
