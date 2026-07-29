# .drop_heavy_columns ----
# Removes list-type attribute columns (e.g. `time_span_seq`, a nested list
# of ~93 dates per row) from an sf object before it's handed to
# terra::extract()/exactextractr::exact_extract(). Those extraction
# functions only ever need the geometry -- but empirically, having such a
# heavy list column along for the ride made extraction ~100x slower
# (~1.4s vs. ~107-123s for the exact same raster + points), presumably due
# to sf/terra's internal conversion trying to carry it along.
#
# Stays a normal sf object throughout (not bare geometry, not a
# SpatVector) -- so there's no separate geometry-type dispatch quirk to
# worry about (terra::extract()/exact_extract() both handle sf directly,
# for any geometry type).
.drop_heavy_columns <- function(vector) {
  geom_col   <- attr(vector, "sf_column")
  attr_names <- setdiff(names(vector), geom_col)

  is_list_col <-
    vapply(sf::st_drop_geometry(vector)[attr_names], is.list, logical(1))

  heavy <- attr_names[is_list_col]

  if (length(heavy) > 0) vector[setdiff(names(vector), heavy)] else vector
}

# .extract_values ----
# Dispatches to the appropriate extraction method based on geometry type,
# and always returns a data.frame (nrow = number of features, ncol =
# number of raster layers) -- the same type terra::extract()/
# exact_extract() already return natively, so downstream code that
# branches on is.data.frame() (e.g. .extract_study_values() in utils.R)
# keeps taking the same code path as before. Expects `geom` to already be
# "light" (see .drop_heavy_columns() above).
#
# - POINT geometries (buffer = 0, given the fix in link_daily.sf()/
#   link_monthly.sf() that skips st_buffer() entirely when buffer = 0):
#   terra::extract(raster, geom, fun=mean, na.rm=TRUE, ID=FALSE). Cheap:
#   one cell lookup per point per layer.
# - POLYGON geometries (real buffers > 0): exactextractr::exact_extract(
#   raster, geom, fun="mean"). Computes each polygon's cell-coverage
#   fractions ONCE and reuses them across all layers. Empirically
#   confirmed via a head-to-head benchmark (20 points, 5000m buffer, 276
#   layers): terra::extract() took 4.04s, exact_extract() took 0.88s --
#   ~4.6x faster, with near-identical results (differences only in the
#   3rd-4th decimal, from a slightly different area-weighting
#   implementation). terra::extract() appears to redo the polygon-raster
#   intersection per layer rather than reusing it across a multi-layer
#   stack, so the gap should widen further at your real scale (2790
#   layers x 1243 points) -- this was the actual cause of the
#   std::bad_alloc crash for large buffers, and of extraction still being
#   the dominant cost (~4571s of ~4631s total) even after fixing the
#   fun=NULL correctness issue and the heavy-column issue.
.extract_values <- function(raster, geom) {

  # exactextractr::exact_extract() requires unique layer names (unlike
  # terra::extract(), which doesn't care) -- layers selected via
  # raster[[lyr_idx]] for a multi-year baseline can end up with duplicate
  # names if the underlying files are named by day-of-year without the
  # year (e.g. 30 layers all named "tas_59", one per baseline year).
  # Downstream code always indexes results by column POSITION
  # (baseline_matrix[i, ], focal_matrix[i, ]), never by name, so forcing
  # unique placeholder names here is always safe.
  names(raster) <- paste0("layer_", seq_len(terra::nlyr(raster)))

  geom_type <- as.character(sf::st_geometry_type(geom, by_geometry = FALSE))

  if (geom_type == "POINT") {
    terra::extract(raster, geom, fun = mean, na.rm = TRUE, ID = FALSE)
  } else {
    exactextractr::exact_extract(raster, geom, fun = "mean", progress = FALSE)
  }
}


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

    # Decompose .data into plain, state-free pieces ONCE, before splitting
    # into chunks -- WKT text and a plain data.frame have no C++ objects
    # attached (unlike sf's geometry column, which can carry cached
    # "prepared geometry"/S2 state that doesn't survive serialize() /
    # unserialize() across a future worker process boundary cleanly).
    # WKT works for ANY geometry type (point or polygon), unlike a raw
    # coordinate matrix, which only works cleanly for points.
    data_wkt  <- sf::st_as_text(sf::st_geometry(.data))
    data_crs  <- sf::st_crs(.data)
    data_attr <- sf::st_drop_geometry(.data)

    # Rebuilds a fresh sf object for just one chunk, INSIDE the worker --
    # the geometry gets created new, in-process, from plain text, so there
    # is no foreign/inherited C++ state to crash on.
    rebuild_chunk <- function(chunk) {
      geom <- sf::st_as_sfc(data_wkt[chunk], crs = data_crs)
      sf::st_sf(data_attr[chunk, , drop = FALSE], geometry = geom)
    }
  }

  if (length(unique(.data$link_date)) == 1 && time_span == 0) {
    # All observations share the same link date — select correct layer first
    dates     <- as_date(terra::time(raster))
    link_date <- unique(.data$link_date)

    # For monthly rasters, ERA5 may store values on a different day than
    # requested (e.g. total_precipitation for July -> June 30). Fall back to
    # the nearest layer within 31 days if no exact match is found.
    lyr_idx <- which(dates == link_date)
    if (length(lyr_idx) == 0) {
      gaps    <- abs(as.integer(dates - link_date))
      nearest <- which.min(gaps)
      if (gaps[[nearest]] <= 31L) lyr_idx <- nearest
    }

    if (length(lyr_idx) == 0) {
      cli::cli_abort(c(
        "Could not find a matching layer for date {.val {link_date}}.",
        "i" = "Available dates: {.val {as.character(dates)}}"
      ))
    }

    # Same fix as .toi_extract_impl(): drop heavy list-columns (time_span_seq
    # etc.) before extraction, and dispatch point vs. polygon appropriately.
    .extract_values(
      raster[[lyr_idx[[1]]]],
      .drop_heavy_columns(.data)
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
        function(chunk) {
          sf::sf_use_s2(FALSE)
          .toi_extract_impl(
            raster_path, rebuild_chunk(chunk),
            baseline_fun   = baseline_fun,
            stat_wrangling = stat_wrangling
          )
        },
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
        function(chunk) {
          sf::sf_use_s2(FALSE)
          .toi_extract_impl(
            raster_path, rebuild_chunk(chunk),
            agg            = TRUE,
            baseline_fun   = baseline_fun,
            stat_wrangling = stat_wrangling
          )
        },
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

  # Detect whether raster has monthly or daily resolution.
  # Do not rely on day == 1: some ERA5 indicators store values on the last
  # day of the month. Instead check that each year-month appears only once.
  is_monthly <- length(unique(format(dates, "%Y-%m"))) == length(dates) &&
    (length(dates) == 1L || median(as.numeric(diff(sort(dates)))) >= 20)

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

  # Strip heavy list-columns (time_span_seq etc.) before any extraction --
  # `vector` itself (with those columns intact) is still used everywhere
  # else in this function (e.g. `vector$time_span_seq`, `vector$.linked`),
  # only `vector_geom` (passed to .extract_values()) is the lightened copy.
  vector_geom <- .drop_heavy_columns(vector)

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
        # All rows share the same lyr_idx here (same time_span_seq), so
        # this is one extraction call for all points/polygons x all layers
        # at once, instead of nrow(vector) * length(lyr_idx) separate
        # calls.
        focal_matrix <- .extract_values(raster[[lyr_idx]], vector_geom)
        lapply(seq_len(nrow(focal_matrix)), function(i) {
          as.numeric(focal_matrix[i, ])
        })
      } else {
        # Aggregate once, extract all points/polygons at once
        raster_agg <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
        result     <- .extract_values(raster_agg, vector_geom)
        lapply(seq_len(nrow(result)), function(i) result[i, , drop = FALSE])
      }

    } else {
      # Different time_span_seq per row -- each row can need a different
      # lyr_idx, so this stays a per-row loop, but each iteration now does
      # ONE .extract_values() call across all of that row's layers at once
      # (instead of one terra::extract() call PER LAYER as before).
      lapply(seq_len(nrow(vector)), function(i) {
        vector_sliced      <- vector[i, ]
        vector_sliced_geom <- vector_geom[i, ]
        target_dates  <- as_date(unlist(vector_sliced$time_span_seq))
        target_norm   <- if (is_monthly) {
          as.Date(format(target_dates, "%Y-%m-01"))
        } else {
          target_dates
        }
        lyr_idx <- which(dates_norm %in% target_norm)

        if (length(lyr_idx) == 0) return(NA_real_)

        if (stat_wrangling %in% c("count_above", "count_below")) {
          as.numeric(.extract_values(raster[[lyr_idx]], vector_sliced_geom))
        } else {
          raster_agg <- terra::app(raster[[lyr_idx]], mean, na.rm = TRUE)
          .extract_values(raster_agg, vector_sliced_geom)
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

      # Extract all baseline layers for all points/polygons at once.
      # NOTE: this used to call terra::extract(..., fun = NULL, ...) to get
      # raw per-cell values -- which only makes sense for points (exactly
      # one cell per point). For polygons that returns a variable number of
      # cells per feature, incompatible with `baseline_matrix[i, ]` below.
      # .extract_values() always collapses to one (area-weighted, for
      # polygons) mean value per feature per layer, for both geometry types.
      baseline_matrix <- .extract_values(raster[[lyr_idx]], vector_geom)

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
        .compute_stat_wrangling(
          baseline_values = baseline_values,
          focal_value     = focal_val,
          stat_wrangling  = stat_wrangling,
          baseline_fun    = baseline_fun
        )
      })

    } else {
      # Different time_span_seq per row -- same per-row loop as above, but
      # now one .extract_values() call per row across all of that row's
      # baseline layers, instead of one terra::extract() call per layer.
      lapply(seq_len(nrow(vector)), function(i) {
        vector_sliced      <- vector[i, ]
        vector_sliced_geom <- vector_geom[i, ]
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

        baseline_values <- as.numeric(.extract_values(raster[[lyr_idx]], vector_sliced_geom))

        focal_val <- if (
          stat_wrangling %in% c("count_above", "count_below") &&
          !is.null(focal_values)
        ) {
          focal_values[[i]]
        } else {
          vector_sliced$.linked
        }

        .compute_stat_wrangling(
          baseline_values = baseline_values,
          focal_value     = focal_val,
          stat_wrangling  = stat_wrangling,
          baseline_fun    = baseline_fun
        )
      })
    }

  } else {
    lapply(seq_len(nrow(vector)), function(i) {
      vector_sliced      <- vector[i, ]
      vector_sliced_geom <- vector_geom[i, ]

      # Normalize link_date for monthly rasters
      link_date <- if (is_monthly) {
        as.Date(format(vector_sliced$link_date, "%Y-%m-01"))
      } else {
        vector_sliced$link_date
      }

      lyr_idx <- which(dates_norm == link_date)

      if (length(lyr_idx) == 0) return(NA_real_)

      as.numeric(.extract_values(raster[[lyr_idx[[1]]]], vector_sliced_geom))
    })
  }
}
