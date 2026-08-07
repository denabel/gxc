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

# .sub_layers ----
# Subsets a raster to specific layers (raster[[idx]]), AND correctly
# re-subsets its "coarse" attribute (attached by .load_climate_raster()
# when downsample_factor is set) with the SAME indices.
#
# This matters because attaching an attribute to a SpatRaster does NOT
# make it automatically track subsetting -- attr(raster[[idx]], "coarse")
# is just the SAME full, unfiltered coarse raster carried along unchanged,
# not coarse[[idx]]. Using it directly in .extract_values() would silently
# extract from the wrong (full) set of coarse layers instead of the ones
# actually requested. Every raster[[lyr_idx]] subsetting operation that
# feeds into .extract_values() must go through this helper instead of
# subsetting directly, whenever downsampling might be in play.
.sub_layers <- function(raster, idx) {
  sub    <- raster[[idx]]
  coarse <- attr(raster, "coarse")
  if (!is.null(coarse)) {
    attr(sub, "coarse") <- coarse[[idx]]
  }
  sub
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
#   one cell lookup per point per layer. Always full resolution -- own
#   measurement showed point extraction's relative difference from an
#   aggregated grid is ~10x higher than for any tested real buffer
#   (5-100km), so downsampling is never applied here.
# - POLYGON geometries (real buffers > 0): exactextractr::exact_extract(
#   raster, geom, fun="mean"). Computes each polygon's cell-coverage
#   fractions ONCE and reuses them across all layers. Empirically
#   confirmed via a head-to-head benchmark (20 points, 5000m buffer, 276
#   layers): terra::extract() took 4.04s, exact_extract() took 0.88s --
#   ~4.6x faster, with near-identical results (differences only in the
#   3rd-4th decimal, from a slightly different area-weighting
#   implementation). If `raster` carries a "coarse" attribute (see
#   .load_climate_raster()/.sub_layers()) and `buffer >=
#   downsample_min_buffer`, the aggregated version is used instead --
#   own measurement (5-100km buffers) showed <0.1% relative difference
#   vs. full resolution, with a 10-32x extraction speedup depending on
#   buffer size.
.extract_values <- function(raster, geom, buffer = 0, downsample_min_buffer = 0) {

  geom_type <- as.character(sf::st_geometry_type(geom, by_geometry = FALSE))

  if (geom_type == "POINT") {
    names(raster) <- paste0("layer_", seq_len(terra::nlyr(raster)))
    return(terra::extract(raster, geom, fun = mean, na.rm = TRUE, ID = FALSE))
  }

  raster_coarse <- attr(raster, "coarse")
  use_coarse    <- !is.null(raster_coarse) && buffer >= downsample_min_buffer
  raster_to_use <- if (use_coarse) raster_coarse else raster

  # exactextractr::exact_extract() requires unique layer names (unlike
  # terra::extract(), which doesn't care) -- layers selected via
  # raster[[lyr_idx]] for a multi-year baseline can end up with duplicate
  # names if the underlying files are named by day-of-year without the
  # year (e.g. 30 layers all named "tas_59", one per baseline year).
  # Downstream code always indexes results by column POSITION
  # (baseline_matrix[i, ], focal_matrix[i, ]), never by name, so forcing
  # unique placeholder names here is always safe.
  names(raster_to_use) <- paste0("layer_", seq_len(terra::nlyr(raster_to_use)))

  result <- exactextractr::exact_extract(raster_to_use, geom, fun = "mean", progress = FALSE)

  # Pre-existing gxc bug, unrelated to downsampling: exact_extract()
  # returns a plain numeric vector (not a data.frame) when the raster has
  # exactly one layer -- e.g. after terra::app(..., mean) collapses a
  # focal window to a single layer for stat_wrangling = "deviation"/
  # "sd_deviation" with buffer > 0. Downstream code (.toi_extract_impl())
  # always assumes a data.frame (nrow(result), result[i, , drop = FALSE]),
  # which fails with a cryptic "seq_len(nrow(result))" error on a bare
  # vector. Force data.frame coercion here so the return type is always
  # consistent, regardless of layer count.
  if (is.null(dim(result))) {
    result <- data.frame(layer_1 = result)
  }

  result
}


# .toi_extract_baseline() -- baseline_fun/stat_wrangling are now expected
# to be LISTS (length 1 in the normal single-combo case, length N for N
# combinations sharing the same extraction -- see .toi_extract_impl()'s
# baseline branch below, where the actual sharing happens). buffer/
# downsample_min_buffer are new passthrough parameters for downsampling
# (see .extract_values()). Pure passthrough here, no logic change.
.toi_extract_baseline <- function(.data,
                                  raster,
                                  baseline_fun,    # list of resolved functions
                                  stat_wrangling         = "deviation",  # list of strings
                                  focal_values           = NULL,
                                  parallel               = FALSE,
                                  chunk_size             = 50,
                                  buffer                 = 0,
                                  downsample_min_buffer  = 0) {
  if (!parallel) {
    .toi_extract_impl(
      raster, .data,
      baseline               = TRUE,
      baseline_fun           = baseline_fun,
      stat_wrangling         = stat_wrangling,
      focal_values           = focal_values,
      buffer                 = buffer,
      downsample_min_buffer  = downsample_min_buffer
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
          baseline               = TRUE,
          baseline_fun           = baseline_fun,
          stat_wrangling         = stat_wrangling,
          focal_values           = focal_values[chunk],
          buffer                 = buffer,
          downsample_min_buffer  = downsample_min_buffer
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
                         time_span              = 0,
                         parallel               = FALSE,
                         chunk_size             = 50,
                         baseline_fun,
                         stat_wrangling         = "deviation",
                         buffer                 = 0,
                         downsample_min_buffer  = 0) {
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
    # .sub_layers() (not raw [[ ]]) so a "coarse" attribute, if present,
    # gets correctly re-subset to the same layer.
    .extract_values(
      .sub_layers(raster, lyr_idx[[1]]),
      .drop_heavy_columns(.data),
      buffer                 = buffer,
      downsample_min_buffer  = downsample_min_buffer
    )

  } else if (length(unique(.data$link_date)) > 1 && time_span == 0) {
    if (!parallel) {
      .toi_extract_impl(
        raster, .data,
        baseline_fun           = baseline_fun,
        stat_wrangling         = stat_wrangling,
        buffer                 = buffer,
        downsample_min_buffer  = downsample_min_buffer
      )
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path, .data[chunk, ],
          baseline_fun           = baseline_fun,
          stat_wrangling         = stat_wrangling,
          buffer                 = buffer,
          downsample_min_buffer  = downsample_min_buffer
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
        agg                    = TRUE,
        baseline_fun           = baseline_fun,
        stat_wrangling         = stat_wrangling,
        buffer                 = buffer,
        downsample_min_buffer  = downsample_min_buffer
      )
    } else {
      raster_values <- future.apply::future_lapply(
        chunks,
        function(chunk) .toi_extract_impl(
          raster_path, .data[chunk, ],
          agg                    = TRUE,
          baseline_fun           = baseline_fun,
          stat_wrangling         = stat_wrangling,
          buffer                 = buffer,
          downsample_min_buffer  = downsample_min_buffer
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
#' @param baseline Whether to aggregate across baseline years. `baseline_fun`
#'   and `stat_wrangling` are LISTS here (length 1 in the normal case, length
#'   N for N baseline_fun/stat_wrangling combinations sharing the same
#'   extracted baseline_values/focal_value -- see the `baseline` branch
#'   below).
#' @param buffer,downsample_min_buffer Passed through to .extract_values()
#'   for downsampling dispatch -- see .load_climate_raster()/.extract_values().
#' @returns A named list.
#' @noRd
.toi_extract_impl <- function(raster,
                              vector,
                              agg                    = FALSE,
                              baseline_fun,
                              baseline               = FALSE,
                              stat_wrangling         = "deviation",
                              focal_values           = NULL,
                              buffer                 = 0,
                              downsample_min_buffer  = 0) {
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

  # Wrapper binding buffer/downsample_min_buffer, so every call site below
  # is a plain extract_fn(raster_sub, geom) -- one less thing to get wrong
  # by forgetting to pass the two new arguments somewhere.
  extract_fn <- function(r, g) {
    .extract_values(r, g, buffer = buffer, downsample_min_buffer = downsample_min_buffer)
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

      # stat_wrangling can be a LIST here (multi-combo case) -- if ANY
      # combination needs raw per-day values (count_above/count_below),
      # we extract raw values for ALL combinations in this call, since
      # that's a safe superset: a combination that actually wants the
      # aggregated mean can still compute it from the raw values
      # downstream, but the reverse (recovering raw values from an
      # already-aggregated mean) is impossible. A plain scalar `%in%`
      # check here would return a vector of length > 1 for a list,
      # which `if()` cannot evaluate.
      if (any(unlist(stat_wrangling) %in% c("count_above", "count_below"))) {
        # All rows share the same lyr_idx here (same time_span_seq), so
        # this is one extraction call for all points/polygons x all layers
        # at once, instead of nrow(vector) * length(lyr_idx) separate
        # calls. .sub_layers() correctly carries a re-subset "coarse"
        # attribute along, if present.
        focal_matrix <- extract_fn(.sub_layers(raster, lyr_idx), vector_geom)
        lapply(seq_len(nrow(focal_matrix)), function(i) {
          as.numeric(focal_matrix[i, ])
        })
      } else {
        # Aggregate once, extract all points/polygons at once. NOTE:
        # terra::app() here collapses to ONE layer via `mean` -- this is
        # the *temporal* aggregation across the focal window, unrelated
        # to the *spatial* downsampling in .extract_values(); the
        # resulting single-layer raster still carries a (correctly
        # subset) "coarse" attribute if .sub_layers() was used to build
        # its input, but terra::app()'s output does NOT automatically
        # inherit input attributes -- so the coarse attribute has to be
        # reattached manually here to still enable downsampling at the
        # subsequent extract_fn() call.
        raster_sub <- .sub_layers(raster, lyr_idx)
        raster_agg <- terra::app(raster_sub, mean, na.rm = TRUE)
        coarse_sub <- attr(raster_sub, "coarse")
        if (!is.null(coarse_sub)) {
          attr(raster_agg, "coarse") <- terra::app(coarse_sub, mean, na.rm = TRUE)
        }
        result <- extract_fn(raster_agg, vector_geom)
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

        # Same list-safe check as in the .all_same_seq() branch above.
        if (any(unlist(stat_wrangling) %in% c("count_above", "count_below"))) {
          as.numeric(extract_fn(.sub_layers(raster, lyr_idx), vector_sliced_geom))
        } else {
          raster_sub <- .sub_layers(raster, lyr_idx)
          raster_agg <- terra::app(raster_sub, mean, na.rm = TRUE)
          coarse_sub <- attr(raster_sub, "coarse")
          if (!is.null(coarse_sub)) {
            attr(raster_agg, "coarse") <- terra::app(coarse_sub, mean, na.rm = TRUE)
          }
          extract_fn(raster_agg, vector_sliced_geom)
        }
      })
    }

  } else if (baseline) {
    # baseline_fun/stat_wrangling are LISTS here (length 1 in the normal
    # single-combo case). The expensive part -- baseline_values/focal_val,
    # extracted from the raster -- is computed ONCE per observation and
    # SHARED across all combinations; only the final
    # .compute_stat_wrangling() call is repeated per combination, on
    # already-extracted numbers.
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
          lapply(baseline_fun, function(bf) list(reference_stat = NA_real_, result = NA_real_))
        }))
      }

      # Extract all baseline layers for all points/polygons at once.
      # NOTE: this used to call terra::extract(..., fun = NULL, ...) to get
      # raw per-cell values -- which only makes sense for points (exactly
      # one cell per point). For polygons that returns a variable number of
      # cells per feature, incompatible with `baseline_matrix[i, ]` below.
      # .extract_values() always collapses to one (area-weighted, for
      # polygons) mean value per feature per layer, for both geometry types.
      baseline_matrix <- extract_fn(.sub_layers(raster, lyr_idx), vector_geom)

      needs_raw_focal <- any(unlist(stat_wrangling) %in% c("count_above", "count_below"))

      lapply(seq_len(nrow(vector)), function(i) {
        baseline_values <- as.numeric(baseline_matrix[i, ])

        # Both representations are computed here, but focal_val_raw is
        # evaluated LAZILY (only if needs_raw_focal) -- this matters
        # because focal_values can be a single-column data.frame in the
        # common case (one layer, all observations share one date), where
        # focal_values[[i]] for i > 1 is a COLUMN index into a 1-column
        # data.frame, not a row lookup, and throws "subscript out of
        # bounds" for i > 1. The ORIGINAL code only ever evaluated this
        # when stat_wrangling was actually count_above/count_below (where
        # focal_values has the right shape); eagerly evaluating it
        # unconditionally (to support mixing count_above with deviation
        # in one combination list) broke every plain deviation/
        # sd_deviation call with more than one observation.
        focal_val_scalar <- vector$.linked[i]
        focal_val_raw    <- if (needs_raw_focal && !is.null(focal_values)) focal_values[[i]] else NULL

        purrr::map2(baseline_fun, stat_wrangling, function(bf, sw) {
          focal_val <- if (sw %in% c("count_above", "count_below") && !is.null(focal_val_raw)) {
            focal_val_raw
          } else {
            focal_val_scalar
          }
          .compute_stat_wrangling(
            baseline_values = baseline_values,
            focal_value     = focal_val,
            stat_wrangling  = sw,
            baseline_fun    = bf
          )
        })
      })

    } else {
      # Different time_span_seq per row -- same per-row loop as above, but
      # now one .extract_values() call per row across all of that row's
      # baseline layers, instead of one terra::extract() call per layer.
      # Same combination-sharing as the .all_same_seq() branch above:
      # baseline_values computed once per row, reused across all
      # baseline_fun/stat_wrangling combinations. Same focal-value-type
      # caveat as above also applies here -- focal_val_raw is evaluated
      # lazily, only if actually needed (see comment in the other branch).
      needs_raw_focal <- any(unlist(stat_wrangling) %in% c("count_above", "count_below"))

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
          return(lapply(baseline_fun, function(bf) list(reference_stat = NA_real_, result = NA_real_)))
        }

        baseline_values <- as.numeric(extract_fn(.sub_layers(raster, lyr_idx), vector_sliced_geom))

        focal_val_scalar <- vector_sliced$.linked
        focal_val_raw    <- if (needs_raw_focal && !is.null(focal_values)) focal_values[[i]] else NULL

        purrr::map2(baseline_fun, stat_wrangling, function(bf, sw) {
          focal_val <- if (sw %in% c("count_above", "count_below") && !is.null(focal_val_raw)) {
            focal_val_raw
          } else {
            focal_val_scalar
          }
          .compute_stat_wrangling(
            baseline_values = baseline_values,
            focal_value     = focal_val,
            stat_wrangling  = sw,
            baseline_fun    = bf
          )
        })
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

      as.numeric(extract_fn(.sub_layers(raster, lyr_idx[[1]]), vector_sliced_geom))
    })
  }
}
