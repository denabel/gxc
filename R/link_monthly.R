#' Link with ERA5 monthly indicators
#'
#' @description Augments spatio-temporal data with indicators from the
#' Copernicus earth observation database (ERA5).
#' The function performs the following pre-/post-processing steps:
#'
#' \itemize{
#'  \item{Construct time adjustments (time aggregations, time lags)}
#'  \item{Compute space adjustments (spatial buffers)}
#'  \item{Download monthly statistics from Copernicus database}
#'  \item{Link raster statistics back to input}
#'  \item{Optionally, add comparative statistics based on a baseline period}
#' }
#'
#' This function interfaces the monthly means of ERA5 indicators. For daily
#' statistics see \code{\link{link_daily}}.
#'
#' @param catalogue Character string specifying which ERA5 catalogue to use.
#'   Options are `"reanalysis-era5-land-monthly-means"`
#'   or `"reanalysis-era5-single-levels-monthly-means"`. The first provides
#'   higher spatial resolution at 0.1x0.1 degrees but is only available from
#'   1950 onwards. If you need data before 1950 or if you are working with large
#'   spatial extents where finer resolution is not required, you can switch to
#'   the latter.
#' @param by_hour Logical or character. If `FALSE` (default), the monthly
#'   averaged values are derived from the entire day
#'   (`"monthly_averaged_reanalysis"`). If a character string specifying an
#'   hour (e.g., `"03:00"`), then the dataset
#'   `"monthly_averaged_reanalysis_by_hour_of_day"` is used, and only values
#'   from that hour of the day are included.
#' @inherit link_daily
#'
#' @details
#' This function interacts with the Copernicus Climate Data Store (CDS) API to
#' download ERA5 monthly reanalysis data for a specified climate indicator and
#' time period. The input spatial points (an sf object) are first optionally
#' buffered (using the `buffer` argument) to expand the extraction area. The
#' function then determines the geographic extent from the (possibly buffered)
#' points and adjusts the time dimension based on the specified `date_var`,
#' `time_lag`, and `time_span` (all in months). Monthly time sequences are
#' constructed assuming that dates correspond to the first day of each month.
#' The function downloads the corresponding monthly data (or hourly-based
#' monthly averages if `by_hour` is specified) and extracts these values for
#' each point—using a direct cell match when `buffer = 0` or aggregating over
#' the buffer area when `buffer > 0`. If a baseline period is provided (e.g.,
#' `baseline = c("1980", "2010")`), baseline monthly statistics are downloaded
#' for the specified period and appended as an additional attribute.
#' Optionally, deviations between the focal and baseline values may be
#' computed.
#'
#' The following indicators are currently supported:
#'
#' `r rd_indicators("link_monthly")`
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create sample point data (sf object)
#' pts <- data.frame(
#'   lon = c(13.4, 11.6, 9.9),
#'   lat = c(52.5, 51.3, 50.1),
#'   date = c("2014-08-01", "2014-08-01", "2014-08-01")
#' )
#' pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
#'
#' # Example 1: Direct extraction (buffer = 0)
#' result1 <- point_link_monthly(pts_sf, indicator = "2m_temperature")
#'
#' # Example 2: Aggregated extraction with a 5 km buffer and a baseline period
#' result2 <- point_link_monthly(
#'   pts_sf,
#'   indicator = "2m_temperature",
#'   buffer = 5,
#'   baseline = c("1980", "2010")
#' )
#'
#' # View the results:
#' head(result1)
#' head(result2)}
#'
#' @export
link_monthly <- function(.data,
                         indicator,
                         ...,
                         cache = TRUE,
                         path = NULL,
                         parallel = FALSE,
                         chunk_size = 50,
                         verbose = TRUE) {
  UseMethod("link_monthly")
}


#' @rdname link_monthly
#' @export
link_monthly.sf <- function(.data,
                            indicator,
                            date_var = "date",
                            time_span = 0,
                            time_lag = 0,
                            buffer = 0,
                            baseline = FALSE,
                            catalogue = "reanalysis-era5-land-monthly-means",
                            by_hour = FALSE,
                            cache = TRUE,
                            path = NULL,
                            parallel = FALSE,
                            chunk_size = 50,
                            verbose = TRUE) {
  .check_valid_catalogue(catalogue, temp_res = "monthly")
  .check_valid_indicator(indicator, catalogue)
  .check_valid_by_hour(by_hour)
  .check_baseline(baseline)
  .check_column(.data, date_var)
  .check_api_key("ecmwfr")
  path <- path %||% .default_download_dir(cache, service = "ecmwfr")

  if (chunk_size > nrow(.data) && isTRUE(parallel)) {
    info("Chunk size is higher than number of input rows. Disabling parallelization.")
    parallel <- FALSE
  }

  # Prep data and create extent
  prepared <- sf::st_transform(.data, 4326)
  prepared <- sf::st_buffer(.data, buffer)
  prepared <- .transform_time(
    prepared,
    date_var = date_var,
    time_span = time_span,
    time_lag = time_lag,
    by = "1 month"
  )
  extent <- .get_extent(.data)

  span <- unlist(prepared$time_span_seq)
  years <- num_keys(year(span))
  months <- num_keys(month(span))

  # Average across entire day or by hour
  if (isFALSE(by_hour)) {
    product_type <- "monthly_averaged_reanalysis"
    request_time <- "00:00"
  } else {
    product_type <- "monthly_averaged_reanalysis_by_hour_of_day"
    request_time <- by_hour
  }

  obs_path <- .request_era5_monthly(
    indicator,
    catalogue = catalogue,
    extent = extent,
    years = years,
    months = months,
    cache = cache,
    path = path,
    prefix = "observation",
    product_type = product_type,
    request_time = request_time,
    verbose = verbose
  )

  raster <- terra::rast(obs_path)

  # Check CRS of both datasets and adjust if necessary
  crs <- terra::crs(prepared)
  if (!identical(crs, terra::crs(raster))) {
    prepared <- sf::st_transform(prepared, crs = terra::crs(raster))
  }

  info(
    "Extracting values from raster",
    msg_done = "Extracted values from raster.",
    msg_failed = "Failed to extract values from raster.",
    level = "step"
  )

  # Extract focal values
  raster_values <- .toi_extract(
    prepared,
    raster,
    obs_path,
    time_span = time_span,
    parallel = parallel,
    chunk_size = chunk_size
  )

  # Create new variable in dataframe
  prepared$.linked <- unlist(raster_values)

  if (!isFALSE(baseline)) {
    prepared <- .add_baseline(
      prepared,
      baseline = baseline,
      requester = .request_era5_monthly,
      request_args = list(
        indicator = indicator,
        years = years,
        months = months,
        extent = extent,
        catalogue = catalogue,
        product_type = product_type,
        request_time = request_time
      ),
      cache = cache,
      path = path,
      parallel = parallel,
      chunk_size = chunk_size,
      verbose = verbose
    )
  }

  if (!cache) {
    unlink(obs_path)
    unlink(baseline_path)
  }

  # cleanup
  prepared <- sf::st_transform(prepared, crs = crs)
  prepared[c("link_date", "link_date_end", "time_span_seq")] <- NULL
  prepared <- move_to_back(prepared, attr(prepared, "sf_column"))
  as_sf_tibble(prepared)
}


point_link_monthly <- function(
    indicator,
    data,
    date_var,
    time_span = 0,
    time_lag = 0,
    buffer = 0,
    baseline = FALSE,
    order = "my",
    path = "./data/raw",
    catalogue = "reanalysis-era5-land-monthly-means",
    by_hour = FALSE,
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
) {
  with_progress({
    p <- progressor(steps = 4)

    # Validate catalogue, indicator choice, and by_hour
    .check_valid_catalogue(catalogue, temp_res = "monthly")
    .check_valid_indicator(indicator, catalogue)
    .check_valid_by_hour(by_hour)

    # Access to API
    api_key <- key_get("wf_api_key")
    wf_set_key(key = api_key)

    # Prep data and create extent
    prepared <- .prep_points(data, buffer)
    data_sf <- prepared$data_sf
    extent <- prepared$extent

    # Transform date-variable and extract relevant time points
    data_sf <- data_sf |>
      mutate(
        link_date = parse_date_time(x=!!sym(date_var), orders=order)
      ) |>
      mutate(
        link_date = link_date - months(time_lag)
      ) |>
      mutate(
        link_date_end = link_date - months(time_span)
      )
    data_sf$time_span_seq <- future_lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date) & !is.na(data_sf[i,]$link_date_end)) {
        seq_dates <- seq(data_sf[i,]$link_date_end, data_sf[i,]$link_date, by = "1 month")
        format(seq_dates, "%Y-%m-%d")
      } else{
        NA_character_
      }
    }, future.seed = TRUE)
    years <- as.character(sort(unique(year(unlist(data_sf$time_span_seq)))))
    months <- as.character(sort(unique(month(unlist(data_sf$time_span_seq)))))
    p(amount = 1, message = "Preprocessing complete")

    # Average across entire day or by hour
    if (isFALSE(by_hour)) {
      product_type <- "monthly_averaged_reanalysis"
      request_time <- "00:00"
    } else {
      product_type <- "monthly_averaged_reanalysis_by_hour_of_day"
      request_time <- by_hour
    }

    # Download data from API
    focal_path <- .make_request_monthly(indicator, catalogue, extent, years,
                                        months, path, prefix = "focal",
                                        product_type, request_time)
    p(amount = 1, message = "Download complete")

    # Load raster file
    raster <- terra::rast(focal_path)

    # Check CRS of both datasets and adjust if necessary
    if(!identical(crs(data_sf), terra::crs(raster))) {
      data_sf <- data_sf |>
        st_transform(crs=st_crs(raster))
    }

    # Extract focal values
    raster_values <- .toi_extract(raster,
                                    focal_path,
                                    data_sf,
                                    time_span = time_span,
                                    parallel = parallel,
                                    chunk_size = chunk_size
    )

    # Create new variable in dataframe
    data_sf$focal_value <- unlist(raster_values)
    p(amount = 1, message = "Focal extraction complete")

    # Check baseline argument
    # If no baseline requested, transform back to longitude and latitude and
    # export final output
    if(isFALSE(baseline)){
      data_sf <- sf::st_transform(data_sf, crs = 4326)

      # Remove files if keep_raw = FALSE
      if (!keep_raw) {
        file.remove(focal_path)
        message("Raw file has been removed.")
      } else {
        message("Raw file has been stored at: ", focal_path)
      }
      p(amount = 1, message = "Baseline skipped")
      p(amount = 0)
      return(data_sf)

    } else if (is.vector(baseline) && length(baseline) == 2) {

      # Extract minimum and maximum baseline years
      min_year <- baseline[1]
      max_year <- baseline[2]

      # Translate baseline years into sequence
      min_baseline <- parse_date_time(paste0(min_year, "-01-01"), order="ymd")
      max_baseline <- parse_date_time(paste0(max_year, "-01-01"), order="ymd")
      baseline_years <- seq(min_baseline, max_baseline, by = "1 year")
      baseline_years <- format(baseline_years, "%Y")


      # Download data from API
      baseline_path <- .make_request_monthly(indicator, catalogue, extent,
                                             baseline_years, months, path,
                                             prefix = "baseline", product_type,
                                             request_time
      )

      # Load data
      baseline_raster <- terra::rast(baseline_path)

      # Extract values from raster for each observation and add to dataframe
      if(length(unique(data_sf$link_date)) == 1){
        # All observations have the same link date
        baseline_raster <- app(baseline_raster, mean)
        baseline_values <- terra::extract(
          baseline_raster,
          data_sf,
          fun = mean,
          na.rm = TRUE,
          ID = FALSE
        )
      } else {
        # All observations have different link dates and mean calculation of focal months
        if (!parallel) {
          # Sequential approach
          baseline_months <- month(time(baseline_raster))
          baseline_values <- lapply(seq_len(nrow(data_sf)), function(i) {
            if (!is.na(data_sf[i,]$link_date)) {
              target_months <- month(ymd(unlist(data_sf[i,]$time_span_seq)))
              layer_index <- which(baseline_months %in% target_months)
              if (length(layer_index) == 0) return(NA)
              raster_subset <- app(baseline_raster[[layer_index]], mean)
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
          baseline_values <- unlist(baseline_values, recursive = FALSE)
        } else {
          # Parallelization approach
          chunks <- split(seq_len(nrow(data_sf)),
                          ceiling(seq_len(nrow(data_sf)) / chunk_size))

          baseline_values_list <- future_lapply(chunks, function(idx) {
            local_baseline_raster <- terra::rast(baseline_path)
            baseline_months <- month(time(local_baseline_raster))

            sapply(idx, function(i) {
              if (!is.na(data_sf[i,]$link_date)) {
                target_months <- month(ymd(unlist(data_sf[i,]$time_span_seq)))
                layer_index <- which(baseline_months %in% target_months)
                if (length(layer_index) == 0) return(NA)
                raster_subset <- app(local_baseline_raster[[layer_index]], mean)
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

          baseline_values <- unlist(baseline_values_list, recursive = FALSE)
        }
      }

      # Add variable to dataframe
      data_sf$baseline_value <- unlist(baseline_values)

      # Calculate absolute deviation between focal and baseline values
      data_sf <- data_sf |>
        mutate(
          deviation = focal_value - baseline_value
        )

      # Transform back to longitude and latitude WGS84
      data_sf <- st_transform(data_sf, crs = 4326)
      p(amount = 1, message = "Baseline complete")
      p(amount = 0)
      # Remove files if keep_raw = FALSE
      if (!keep_raw) {
        file.remove(focal_path)
        file.remove(baseline_path)
        message("Raw files have been removed.")
      } else {
        message("Raw files have been stored in: ", path)
      }
      return(data_sf)

    } else {
      stop("Baseline argument must be either FALSE or a vector of length 2 specifying min and max baseline years.")
    }
  })
}


#' @rdname link_monthly
#' @export
link_monthly.stars <- function(.data, indicator, ...) {
  .data <- terra::rast(.data)
  stars::st_as_stars(link_monthly(.data, indicator, ...))
}


#' @rdname link_monthly
#' @export
link_monthly.SpatVector <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  terra::vect(link_monthly(.data, indicator, ...))
}


#' @rdname link_monthly
#' @export
link_monthly.Spatial <- function(.data, indicator, ...) {
  .data <- sf::st_as_sf(.data)
  sf::as_Spatial(link_monthly(.data, indicator, ...))
}
