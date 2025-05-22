info <- function(text,
                 ...,
                 level = "info",
                 verbose = NULL,
                 .envir = parent.frame()) {
  verbose <- verbose %||% get0("verbose", envir = .envir, ifnotfound = TRUE)
  if (!verbose) {
    return()
  }

  fun <- switch(
    level,
    info = cli::cli_alert_info,
    warning = cli::cli_alert_warning,
    danger = cli::cli_alert_danger,
    success = cli::cli_alert_success,
    step = cli::cli_progress_step
  )

  fun(text, ..., .envir = .envir)
}


with_cli <- function(expr, .envir = parent.frame()) {
  withCallingHandlers(
    expr,
    message = function(m) cli::cli_inform(conditionMessage(m), .envir = .envir),
    warning = function(w) cli::cli_warn(conditionMessage(w), .envir = .envir),
    error = function(e) cli::cli_abort(conditionMessage(e), .envir = .envir)
  )
}


dquote <- function(x) {
  dQuote(x, q = FALSE)
}


obj_name <- function(x, env = parent.frame()) {
  deparse(substitute(x, env))
}


days <- function(x = 1) {
  as.difftime(x, units = "days")
}


date_component <- function(x, unit = c("year", "month", "day")) {
  unit <- switch(unit, month = "mon", day = "mday", unit)
  add <- switch(unit, year = 1900, mon = 1, 0)
  tz <- attr(x, "tzone") %||% ""
  x <- as.POSIXlt(x, tz = tz)[, unit] + add
  as.character(sort(unique(x)))
}


# Spatial processing of input ---------------------------------------------

#' @title Helper function for making a bbox and create the spatial extent of the
#' sf dataset
#' @noRd
.prep_poly <- function(data) {

  # Check sf
  if (!inherits(data, "sf")) {
    stop("Data must be a sf object.")
  }

  # Check CRS and transform to WGS84 if necessary
  if (sf::st_crs(data)$epsg != 4326) {
    message("Transforming data to WGS84 (EPSG:4326).")
    data <- sf::st_transform(data, crs = 4326)
  }

  # Extract bounding box
  box <- sf::st_bbox(data)

  # Create extent in order: north, west, south, east
  extent <- c(
    ceiling(box$ymax),
    floor(box$xmin),
    floor(box$ymin),
    ceiling(box$xmax)
  )

  # Return data and extent
  list(data_sf = data, extent = extent)
}


#' @title Helper function for making a bbox and create the spatial extent of
#' the gridded dataset
#' @noRd
.prep_grid <- function(data) {

  # Check SpatRaster
  if (!inherits(data, "SpatRaster")) {
    stop("Data must be a SpatRaster object for gridded input.")
  }

  # Check CRS and transform to WGS84 if necessary for gridded data
  crs_info <- terra::crs(data, describe = TRUE)
  if (is.null(crs_info$code) || crs_info$code != "EPSG:4326") {
    message("Transforming raster data to WGS84 (EPSG:4326).")
    data <- terra::project(data, "EPSG:4326")
  }

  # Extract extent
  e <- terra::ext(data)

  # Create an extent vector in order: north, west, south, east
  extent <- c(ceiling(e[4]), floor(e[1]), floor(e[3]), ceiling(e[2]))

  list(grid = data, extent = extent)
}


# Raster processing helpers -----------------------------------------------

#' Process raster file to add timestamp and update the stored file
#'
#' This function takes a SpatRaster object and a set of days, months, and years,
#' constructs a valid date vector, assigns it as the raster's time dimension,
#' writes the raster to a temporary NetCDF file using terra::writeCDF, removes the
#' original file, and renames the temporary file to the original file path.
#'
#' @param raster A SpatRaster object to be processed.
#' @param days Vector of days (numeric or character) used to build the date vector.
#' @param months Vector of months (numeric or character).
#' @param years Vector of years (numeric or character).
#' @param path Directory where the raster file is stored.
#'
#' @return The input SpatRaster with its time dimension updated. On disk, the original
#'         file is replaced by the new file with time information.
#' @noRd
.raster_timestamp <- function(raster, days, months, years, path) {
  file_path <- terra::sources(raster)

  # Build a vector of valid date strings
  valid_date_strings <- expand_grid(
    day = as.numeric(days),
    month = as.numeric(months),
    year = as.numeric(years)
  ) |>
    mutate(date = make_date(year, month, day)) |>
    filter(!is.na(date)) |>
    arrange(date) |>
    dplyr::pull(date) |>
    format("%Y-%m-%d")

  # Assign the date vector as the raster's time dimension
  terra::time(raster) <- as.Date(valid_date_strings)

  # Extract the original variable names and units from the raster
  orig_units <- terra::units(raster)
  orig_varnames <- terra::varnames(raster)

  # Clean raster metadata from illegal characters
  raster <- metags_sanitize(raster)

  # Create a temporary file path for the updated raster file
  temp_file <- file.path(path, paste0("temp_", basename(file_path)))

  # Write the raster to the temporary file (NetCDF format)
  terra::writeCDF(raster,
                  filename = temp_file,
                  varname = orig_varnames,
                  unit = orig_units,
                  overwrite = TRUE)

  # Replace original file with temporary file
  file.remove(file_path)
  file.rename(temp_file, file_path)
  raster
}


#' Function to sanitize the metadata of a SpatRaster, i.e., remove all metags
#' that contain disallowed characters.
#' @param raster A SpatRaster
#' @returns A SpatRaster
#' @noRd
metags_sanitize <- function(raster) {
  meta <- terra::metags(raster)
  empty <- meta

  if (utils::packageVersion("terra") > "1.8-42") {
    empty[, 2] <- ""
    terra::metags(raster) <- empty
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta$value), ]
  } else {
    terra::metags(raster) <- NULL
    terra::metags(raster) <- meta[grepl("^[A-Za-z0-9._-]*$", meta)]
  }

  raster
}


# Data extraction helpers -------------------------------------------------

#' Focal extraction
#' @noRd
.focal_extract <- function(raster,
                           focal_path,
                           data_sf,
                           time_span = 0,
                           parallel = FALSE,
                           chunk_size = 50
                           ) {
  # Extract values from raster for each observation and add to dataframe
  if(length(unique(data_sf$link_date)) == 1 & time_span == 0){
    # All observations have the same link date and direct link to focal month
    raster_values <- terra::extract(
      raster,
      data_sf,
      fun = mean,
      na.rm = TRUE,
      ID = FALSE
    )
  } else if (length(unique(data_sf$link_date)) > 1 & time_span == 0){
    # All observations have different link dates and direct link to focal month
    if (!parallel) {
      # Sequential approach
      raster_dates <- as.Date(terra::time(raster))
      raster_values <- lapply(seq_len(nrow(data_sf)), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          target_date <- data_sf[i,]$link_date
          layer_index <- which(raster_dates == target_date)
          if (length(layer_index) == 0) return(NA)
          terra::extract(
            raster[[layer_index]],
            data_sf[i,],
            fun = mean,
            na.rm = TRUE,
            ID = FALSE
          )
        } else {
          NA
        }
      })
      raster_values <- unlist(raster_values, recursive = FALSE)
    } else {
      # Parallelization approach
      chunks <- split(seq_len(nrow(data_sf)),
                      ceiling(seq_len(nrow(data_sf)) / chunk_size))

      raster_values_list <- future.apply::future_lapply(chunks, function(idx) {
        local_raster <- terra::rast(focal_path)
        local_dates <- as.Date(terra::time(local_raster))

        sapply(idx, function(i) {
          if (!is.na(data_sf[i,]$link_date)) {
            target_date <- data_sf[i,]$link_date
            layer_index <- which(local_dates == target_date)
            if(length(layer_index) == 0) return(NA)
            terra::extract(
              local_raster[[layer_index]],
              data_sf[i,],
              fun = mean,
              na.rm = TRUE,
              ID = FALSE
            )
          } else {
            NA
          }
        })
      }, future.seed = TRUE)

      raster_values <- unlist(raster_values_list, recursive = FALSE)
    }

  } else if (length(unique(data_sf$link_date)) >= 1 & time_span > 0){
    # All observations have different link dates and mean calculation of focal months
    if (!parallel) {
      # Sequential approach
      raster_dates <- as.Date(terra::time(raster))
      raster_values <- lapply(seq_len(nrow(data_sf)), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          target_dates <- lubridate::ymd(unlist(data_sf[i,]$time_span_seq))
          layer_index <- which(raster_dates %in% target_dates)
          if (length(layer_index) == 0) return(NA)
          raster_subset <- terra::app(raster[[layer_index]], mean)
          terra::extract(
            raster_subset,
            data_sf[i,],
            fun = mean,
            na.rm = TRUE,
            ID = FALSE
          )
        } else {
          NA
        }
      })
      raster_values <- unlist(raster_values, recursive = FALSE)
    } else {
      # Parallel approach with chunking
      chunks <- split(seq_len(nrow(data_sf)),
                      ceiling(seq_len(nrow(data_sf)) / chunk_size))

      raster_values_list <- future.apply::future_lapply(chunks, function(idx) {
        local_raster <- terra::rast(focal_path)
        local_dates <- as.Date(terra::time(local_raster))

        sapply(idx, function(i) {
          if (!is.na(data_sf[i,]$link_date)) {
            target_dates <- lubridate::ymd(unlist(data_sf[i,]$time_span_seq))
            layer_index <- which(local_dates %in% target_dates)
            if(length(layer_index) == 0) return(NA)
            raster_subset <- terra::app(local_raster[[layer_index]], mean)
            terra::extract(
              raster_subset,
              data_sf[i,],
              fun = mean,
              na.rm = TRUE,
              ID = FALSE
            )
          } else {
            NA
          }
        })
      }, future.seed = TRUE)

      raster_values <- unlist(raster_values_list, recursive = FALSE)
    }
  }
}


#' Focal extraction for gridded data input
#' @noRd
.focal_extract_grid <- function(raster,
                                data_sf,
                                grid_df,
                                time_span = 0,
                                parallel = FALSE,
                                chunk_size = 50,
                                method = "bilinear"
                                ) {
  # Get focal raster dates
  raster_dates <- as.Date(terra::time(raster))

  if (time_span == 0) {
    # For a single date, all grid cells share the same link_date.
    unique_dates <- unique(grid_df$link_date)
    if (length(unique_dates) != 1) {
      stop("For gridded data with time_span == 0, the link_date should be unique across the grid.")
    }
    target_date <- unique_dates[1]
    match_idx <- which(raster_dates == target_date)
    if (length(match_idx) == 0) {
      warning("No matching focal layer found for the target date.")
      return(NA)
    }
    # Use the first matching layer
    focal_layer <- raster[[match_idx[1]]]

    # Resample the focal layer to match grid_data
    extracted <- terra::resample(focal_layer, data_sf, method = method)
    return(extracted)

  } else {
    # For time_span > 0, all grid cells share the same time_span_seq.
    unique_seq <- unique(grid_df$time_span_seq)
    if (length(unique_seq) != 1) {
      stop("For gridded data with time_span > 0, all cells should share the same time_span_seq.")
    }
    seq_dates <- lubridate::ymd(unique_seq[[1]])
    match_idx <- which(raster_dates %in% seq_dates)
    if (length(match_idx) == 0) {
      warning("No matching focal layers found for the target time span.")
      return(NA)
    }
    # Compute the average of the matching layers
    subset_avg <- terra::app(raster[[match_idx]], mean, na.rm = TRUE)

    # Resample the averaged raster to grid_data
    extracted <- terra::resample(subset_avg, data_sf, method = method)
    return(extracted)
  }
}

