catalogues <- list(
  era5 = list(
    hourly = list(
      default =  "reanalysis-era5-single-levels",
      only_land = "reanalysis-era5-land",
      pressure_levels = "reanalysis-era5-pressure-levels"
    ),

    daily = list(
      default = "derived-era5-single-levels-daily-statistics",
      only_land = "derived-era5-land-daily-statistics",
      pressure_levels = "derived-era5-pressure-levels-daily-statistics"
    ),

    monthly = list(
      default = "reanalysis-era5-single-levels-monthly-means",
      only_land = "reanalysis-era5-land-monthly-means",
      pressure_levels = "reanalysis-era5-pressure-levels-monthly-means"
    )
  )
)


# Allowed catalogues
allowed_catalogues_monthly <- c(
  "reanalysis-era5-land-monthly-means",
  "reanalysis-era5-single-levels-monthly-means"
)

allowed_catalogues_daily <- c(
  "derived-era5-land-daily-statistics",
  "derived-era5-single-levels-daily-statistics"
)

# Allowed indicators
allowed_indicators_by_catalogue <- list(
  `reanalysis-era5-land-monthly-means` = c(
    "2m_temperature",
    "total_precipitation",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    #"10m_wind_speed", # does not exist in catalogue
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation",
    #"snow_cover", # time stamp first day of NEXT month, if by_hour, then LAST day of focal month
    "snowfall"
  ),
  `reanalysis-era5-single-levels-monthly-means` = c(
    "2m_temperature",
    #"total_precipitation", # time stamp last day of previous month, if by_hour, time stamp one hour earlier
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "10m_wind_speed",
    #"instantaneous_10m_wind_gust", # time stamp last day of previous month, but if by_hour, time stamp correct
    #"downward_uv_radiation_at_the_surface", # time stamp last day of previous month, if by_hour, time stamp one hour earlier
    "total_cloud_cover",
    #"k_index", # Time stamp LAST day of focal month (e.g. 2014-08-31) when requested August 2014
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation"
    #"snowfall" # time stamp last day of previous month
  ),
  `derived-era5-land-daily-statistics` = c(
    "2m_temperature"
    #"snow_cover",
    #"10m_u_component_of_wind",
    #"10m_v_component_of_wind",
    #"leaf_area_index_high_vegetation",
    #"leaf_area_index_low_vegetation"
  ),
  `derived-era5-single-levels-daily-statistics` = c(
    "2m_temperature"
    #"total_precipitation",
    #"10m_u_component_of_wind",
    #"10m_v_component_of_wind",
    #"10m_wind_speed",
    #"instantaneous_10m_wind_gust",
    #"downward_uv_radiation_at_the_surface",
    #"total_cloud_cover",
    #"k_index",
    #"leaf_area_index_high_vegetation",
    #"leaf_area_index_low_vegetation"
    #"snowfall"
  )
)


# Allowed input hours
allowed_hours <- sprintf("%02d:00", 0:23)

# Allowed statistic
allowed_statistic <- c("daily_mean", "daily_maximum", "daily_minimum")

# Allowed time-zone
allowed_time_zone <- sprintf("utc%+03d:00", -12:14)


#' Check if a given catalogue is in the list of allowed catalogues
#' @param catalogue A string giving a catalogue.
#' @param temp_res Temporal resolution. Allowed catalogues depend on this value
#' @returns NULL or an error.
#' @noRd
.check_valid_catalogue <- function(catalogue, temp_res = "monthly") {
  allowed_catalogues <- switch(
    temp_res,
    monthly = allowed_catalogues_monthly,
    daily = allowed_catalogues_daily,
    cli::cli_abort(c(
      "Invalid temporal resolution",
      "i" = "Choose either {.val monthly} or {.val daily}."
    ))
  )

  if (!catalogue %in% allowed_catalogues) {
    names(allowed_catalogues) <- rep("*", length(allowed_catalogues))
    cli::cli_abort(c(
      "Invalid `catalogue` argument.",
      "i" = "Please choose one of the following:",
      allowed_catalogues
    ))
  }
}

#' Check if a given indicator is in the allowed values for indicators inside
#' a catalogue.
#' @param indicator A string giving an indicator.
#' @param catalogue A string giving a catalogue.
#' @param temp_res Temporal resolution. Allowed catalogues depend on this value
#' @returns NULL or an error.
#' @noRd
.check_valid_indicator <- function(indicator, catalogue) {
  inds_in_catalogue <- allowed_indicators_by_catalogue[[catalogue]]

  if (!indicator %in% inds_in_catalogue) {
    names(inds_in_catalogue) <- rep("*", length(inds_in_catalogue))
    cli::cli_abort(c(
      "Invalid `indicator` for catalogue {.val {catalogue}}.",
      "i" = "Please choose one of the following:",
      inds_in_catalogue
    ))
  }
}


#' Check if a given hour specification is a full hour.
#' @param by_hour FALSE or a string giving an hour.
#' @returns NULL or an error.
#' @noRd
.check_valid_by_hour <- function(by_hour) {
  if (!isFALSE(by_hour) && !by_hour %in% allowed_hours) {
    cli::cli_abort(c(
      "Invalid `by_hour` argument.",
      "i" = "Please choose a full hour between 00:00 and 23:00."
    ))
  }
}


#' Check if a given statistic is in the list of allowed statistics.
#' @param statistic A string giving a statistic type.
#' @returns NULL or an error.
#' @noRd
.check_valid_statistic <- function(statistic) {
  if (!statistic %in% allowed_statistic)  {
    cli::cli_abort(c(
      "Invalid `statistic` argument.",
      "i" = "Please choose one of {.val {allowed_statistic}}."
    ))
  }
}


#' Check if a given time zone is in the list of valid time zones.
#' @param time_zone A string giving a time zone.
#' @returns NULL or an error.
#' @noRd
.check_valid_time_zone <- function(time_zone) {
  if (!time_zone %in% allowed_time_zone)  {
    cli::cli_abort(c(
      "Invalid `time_zone` argument.",
      "i" = "Please choose a time zone between utc-12:00 and utc+14:00."
    ))
  }
}


.check_parallel <- function(parallel) {
  if (parallel) {
    rlang::check_installed("future.apply", "for parallel processing.")
    rlang::check_installed("ncdf4", "to write raster files during parallel processing.")
  }
}


.check_baseline <- function(baseline) {
  if (!isFALSE(baseline) && !length(baseline) == 2) {
    cli::cli_abort(c(
      "Invalid `baseline` argument.",
      "i" = paste(
        "Must be either FALSE or a length-2 vector giving",
        "a range of years to compare your observations to."
      )
    ))
  }
}


.check_class <- function(x, cls, name = obj_name(x)) {
  if (!inherits(x, cls)) {
    cli::cli_abort("`{name}` must of class {.cls {cls}}.")
  }
}


.check_column <- function(x, col, name = obj_name(x)) {
  .check_class(x, "data.frame", name = name)
  if (!all(col %in% names(x))) {
    cli::cli_abort("`{name}` must contain {cli::qty(col)} column{?s} {.val {col}}.")
  }
}


.check_terra_time <- function(x, name = obj_name(x)) {
  if (all(is.na(terra::time(x)))) {
    cli::cli_abort("`{name}` must contain a time dimension (`terra::time()`).")
  }
}
