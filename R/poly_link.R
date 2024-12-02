poly_link <- function(
    indicator,
    data,
    date_var,
    time_span = 0,
    time_lag = 0,
    baseline = FALSE,
    min_year,
    max_year,
    order = "my",
    path = "./data/raw"
  ) {

  # Create bounding box
  data_sf <- st_transform(data, crs = 4326)
  box <- st_bbox(data_sf)
  extent <- c(ceiling(box$ymax), floor(box$xmin),
              floor(box$ymin), ceiling(box$xmax))

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
  data_sf$time_span_seq <- lapply(1:nrow(data_sf), function(i) {
    if (!is.na(data_sf[i,]$link_date) & !is.na(data_sf[i,]$link_date_end)) {
      seq_dates <- seq(data_sf[i,]$link_date_end, data_sf[i,]$link_date, by = "1 month")
      format(seq_dates, "%Y-%m-%d")
    } else{
      NA_character_
    }
  })
  years <- as.character(sort(unique(year(unlist(data_sf$time_span_seq)))))
  months <- as.character(sort(unique(month(unlist(data_sf$time_span_seq)))))

  # Access to API
  wf_set_key()

  # Specify API request
  focal_request <- list(
    data_format = "grib",
    variable = indicator,
    product_type = "monthly_averaged_reanalysis",
    time = "00:00",
    year = years,
    month = months,
    area = extent,
    dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
    target = paste0(indicator, "_", date_var, ".grib")
  )

  # Download data from C3S
  focal_path <- wf_request(
    request = focal_request,
    transfer = TRUE,
    path = path,
    verbose = FALSE
  )

  # Load raster file
  raster <- terra::rast(paste0(path, "/", indicator, "_", date_var, ".grib"))

  # Check CRS of both datasets and adjust if necessary
  if(!identical(crs(data_sf), terra::crs(raster))) {
    data_sf <- data_sf |>
      st_transform(crs=st_crs(raster))
  }

  # Extract values from raster for each observation and add to dataframe
  # Different approaches for different data structures to maximize performance(?)
  if(length(unique(data_sf$link_date)) == 1 & time_span == 0){
    # All observations have the same link date and direct link to focal month
    raster_values <- terra::extract(
      raster,
      data_sf,
      fun = "mean", # for buffer
      ID = FALSE
    )
  } else if (length(unique(data_sf$link_date)) > 1 & time_span == 0){
    # All observations have different link dates and direct link to focal month
    raster_values <- lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date)) {
        raster_value <- terra::extract(
          raster[[as.Date(time(raster))==data_sf[i,]$link_date]],
          data_sf[i,],
          fun = "mean", # for buffer
          ID = FALSE
        )
      } else {
        raster_value <- NA
      }
    })
  } else if (length(unique(data_sf$link_date)) >= 1 & time_span > 0){
    # All observations have different link dates and mean calculation of focal months
    raster_values <- lapply(1:nrow(data_sf), function(i) {
      if (!is.na(data_sf[i,]$link_date)) {
        raster_subset <- app(
          raster[[as.Date(time(raster)) %in% ymd(unlist(data_sf[i,]$time_span_seq))]], mean)
        raster_value <- terra::extract(
          raster_subset,
          data_sf[i,],
          fun = "mean", # for buffer
          ID = FALSE
        )
      } else {
        raster_value <- NA
      }
    })
  }

  # Create new variable in dataframe
  data_sf$focal_value <- unlist(raster_values)

  if(baseline==FALSE){
    # If no baseline requested, transform back to longitude and latitude and final output
    data_sf <- st_transform(data_sf, crs = 4326)
    return(data_sf)

  } else{

    # Translate user specified baseline years into sequence
    min_baseline <- parse_date_time(paste0(min_year, "-01-01"), order="ymd")
    max_baseline <- parse_date_time(paste0(max_year, "-01-01"), order="ymd")
    baseline_years <- seq(min_baseline, max_baseline, by = "1 year")
    baseline_years <- format(baseline_years, "%Y")

    # Specify API request
    baseline_request <- list(
      data_format = "grib",
      variable = indicator,
      product_type = "monthly_averaged_reanalysis",
      time = "00:00",
      year = baseline_years,
      month = months,
      area = extent,
      dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
      target = paste0(indicator, "_", date_var, "_baseline_", ".grib")
    )

    # Download data from C3S
    baseline_path <- wf_request(
      request = baseline_request,
      transfer = TRUE,
      path = path,
      verbose = FALSE
    )

    # Load data
    baseline_raster <- terra::rast(paste0(path, "/", indicator, "_", date_var,
                                          "_baseline_", ".grib"))

    # Extract values from raster for each observation and add to dataframe
    # Different approaches for different data structures to maximize performance(?)
    if(length(unique(data_sf$link_date)) == 1){
      # All observations have the same link date
      baseline_raster <- app(baseline_raster, mean)
      baseline_values <- terra::extract(
        baseline_raster,
        data_sf,
        fun = "mean", # for buffer
        ID = FALSE
      )
    } else {
      # All observations have different link dates and mean calculation of focal months
      baseline_values <- lapply(1:nrow(data_sf), function(i) {
        if (!is.na(data_sf[i,]$link_date)) {
          raster_subset <- app(
            baseline_raster[[month(time(baseline_raster)) %in%
                               month(ymd(unlist(data_sf[i,]$time_span_seq)))]], mean)
          baseline_value <- terra::extract(
            raster_subset,
            data_sf[i,],
            fun = "mean", # for buffer
            ID = FALSE
          )
        } else {
          baseline_value <- NA
        }
      })
    }

    # Add variable to dataframe
    data_sf$baseline_value <- unlist(baseline_values)

    # Calculate absolute deviation between focal and baseline values
    data_sf <- data_sf |>
      mutate(
        deviation = focal_value - baseline_value
      )

    # Transform back to longitude and latitude and final output
    data_sf <- st_transform(data_sf, crs = 4326)
    return(data_sf)

  }

}
