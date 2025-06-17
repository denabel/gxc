.toi_extract_baseline <- function(.data,
                                  raster,
                                  path = NULL,
                                  parallel = FALSE,
                                  chunk_size = 50) {
  if (length(unique(.data$link_date)) == 1) {
    # All observations have the same link date
    raster <- terra::app(raster, mean)
    terra::extract(
      raster,
      .data,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else {
    # All observations have different link dates and mean calculation of months
    if (!parallel) {
      .toi_extract_impl(raster, .data, baseline = TRUE)
    } else {
      chunks <- split(
        seq_len(nrow(.data)),
        ceiling(seq_len(nrow(.data)) / chunk_size)
      )

      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          path,
          .data,
          idx = chunk,
          baseline = TRUE
        ),
        future.seed = TRUE
      )

      unlist(raster_values, recursive = FALSE)
    }
  }
}


.toi_extract_grid_baseline <- function(.data,
                                       raster,
                                       temporals,
                                       path = NULL,
                                       time_span = 0,
                                       parallel = FALSE,
                                       chunk_size = 50) {
  .toi_extract_grid(
    .data,
    raster,
    temporals,
    agg = time_span > 0,
    baseline = TRUE,
    parallel = parallel,
    chunk_size = chunk_size
  )
}


#' Focal extraction
#' @noRd
.toi_extract <- function(.data,
                         raster,
                         raster_path,
                         time_span = 0,
                         parallel = FALSE,
                         chunk_size = 50) {
  if (parallel) {
    chunks <- split(
      seq_len(nrow(.data)),
      ceiling(seq_len(nrow(.data)) / chunk_size)
    )
  }

  # Extract values from raster for each observation and add to dataframe
  if (length(unique(.data$link_date)) == 1 && time_span == 0) {
    # All observations have the same link date and direct link to focal month
    terra::extract(
      raster,
      .data,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else if (length(unique(.data$link_date)) > 1 && time_span == 0) {
    # All observations have different link dates and direct link to focal month
    if (!parallel) {
      .toi_extract_impl(raster, .data)
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(raster_path, .data, idx = chunk),
        future.seed = TRUE
      )
      unlist(raster_values, recursive = FALSE)
    }

  } else if (length(unique(.data$link_date)) >= 1 & time_span > 0) {
    # All observations have different link dates and mean calculation of focal months
    if (!parallel) {
      .toi_extract_impl(raster, .data, agg = TRUE)
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path,
          .data,
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
#' @param .data Raster containing input observations
#' @param ind_raster Raster containing EOD indicator data
#' @param temporals Dataframe containing time information constructed by
#'   .transform_time()
#' @returns A SpatRaster
#' @noRd
.toi_extract_grid <- function(.data,
                              raster,
                              temporals,
                              agg = FALSE,
                              baseline = FALSE,
                              parallel = FALSE,
                              chunk_size = 50,
                              method = "bilinear") {
  dates <- as_date(terra::time(raster))

  if (parallel) {
    chunks <- terra::split(.data, ceiling(terra::ncell(.data) / chunk_size))
  }

  # Times cannot vary across grids because SpatRasters do not support it
  # If the temporals dataframe suggests this, throw an error
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
    # if a timespan is specified, aggregate across all dates
    lyr_idx <- which(dates %in% unlist(seq_dates))
    toi_layer <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
  } else if (agg && baseline) {
    seq_md <- paste(month(seq_dates), day(seq_dates), sep = "-")
    baseline_md <- paste(month(dates), day(dates), sep = "-")
    lyr_idx <- which(baseline_md %in% seq_md)
    toi_layer <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
  } else if (!agg && baseline) {
    target_dates <- unlist(temporals$time_span_seq)
    target_md <- paste(month(target_dates), day(target_dates), sep = "-")
    baseline_md <- paste(month(dates), day(dates), sep = "-")
    lyr_idx <- which(baseline_md %in% target_md)
    toi_layer <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
  } else {
    lyr_idx <- which(dates == target_date)[[1]]
    toi_layer <- raster[[lyr_idx]]
  }

  terra::resample(toi_layer, .data, method = method)
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

  dates <- as_date(terra::time(raster))

  if (baseline) {
    month <- month(dates)
    day <- day(dates)
  }

  vals <- lapply(seq_len(nrow(vector)), function(i) {
    vector_sliced <- vector[i, ]
    if (agg) {
      # if a time span is specified, compute the average across
      target_dates <- as_date(unlist(vector_sliced$time_span_seq))
      lyr_idx <- which(dates %in% target_dates)
      raster <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
    } else if (baseline) {
      # for baseline calculations, compute the average across the same dates
      # of different years, e.g. 2014-01-01, 2015-01-01, ...
      target_dates <- as_date(unlist(vector_sliced$time_span_seq))
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
