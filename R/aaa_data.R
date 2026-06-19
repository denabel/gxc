# Internal data objects — loaded first due to filename prefix
# Shared by check.R and rd.R

allowed_catalogues_monthly <- c(
  "reanalysis-era5-land-monthly-means",
  "reanalysis-era5-single-levels-monthly-means"
)

allowed_catalogues_daily <- c(
  "derived-era5-land-daily-statistics",
  "derived-era5-single-levels-daily-statistics"
)

allowed_indicators_by_catalogue <- list(
  `reanalysis-era5-land-monthly-means` = c(
    "2m_temperature",
    "total_precipitation",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation",
    "snow_cover",
    "snowfall"
  ),
  `reanalysis-era5-single-levels-monthly-means` = c(
    "2m_temperature",
    "total_precipitation",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "instantaneous_10m_wind_gust",
    "downward_uv_radiation_at_the_surface",
    "total_cloud_cover",
    "k_index",
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation",
    "snowfall"
  ),
  `derived-era5-land-daily-statistics` = c(
    "2m_temperature",
    "snow_cover",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation"
  ),
  `derived-era5-single-levels-daily-statistics` = c(
    "2m_temperature",
    "total_precipitation",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "instantaneous_10m_wind_gust",
    "downward_uv_radiation_at_the_surface",
    "total_cloud_cover",
    "k_index",
    "leaf_area_index_high_vegetation",
    "leaf_area_index_low_vegetation",
    "snowfall"
  )
)

allowed_hours     <- sprintf("%02d:00", 0:23)
allowed_statistic <- c("daily_mean", "daily_maximum", "daily_minimum")
allowed_time_zone <- sprintf("utc%+03d:00", -12:14)
