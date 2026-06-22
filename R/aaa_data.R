# Internal data objects — loaded first due to filename prefix
# Shared by check.R and rd.R

allowed_catalogues_monthly <- c(
  "reanalysis-era5-land-monthly-means",
  "reanalysis-era5-single-levels-monthly-means",
  "dwd-monthly"
)

allowed_catalogues_daily <- c(
  "derived-era5-land-daily-statistics",
  "derived-era5-single-levels-daily-statistics",
  "dwd-hyras-daily"
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
  ),
  `dwd-hyras-daily` = c(
    "air_temperature_mean",
    "air_temperature_max",
    "air_temperature_min",
    "precipitation"
  ),
  `dwd-monthly` = c(
    "air_temperature_mean",
    "air_temperature_max",
    "air_temperature_min",
    "precipitation",
    "drought_index"
  )
)

allowed_hours     <- sprintf("%02d:00", 0:23)
allowed_statistic <- c("daily_mean", "daily_maximum", "daily_minimum")
allowed_time_zone <- sprintf("utc%+03d:00", -12:14)

# DWD URL templates
.dwd_url_templates <- list(
  daily = list(
    air_temperature_mean = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/daily/hyras_de/air_temperature_mean/",
      "tas_hyras_1_{year}_v6-1_de.nc"
    ),
    air_temperature_max = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/daily/hyras_de/air_temperature_max/",
      "tasmax_hyras_1_{year}_v6-1_de.nc"
    ),
    air_temperature_min = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/daily/hyras_de/air_temperature_min/",
      "tasmin_hyras_1_{year}_v6-1_de.nc"
    ),
    precipitation = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/daily/hyras_de/precipitation/",
      "pr_hyras_1_{year}_v6-1_de.nc"
    )
  ),
  monthly = list(
    air_temperature_mean = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/monthly/air_temperature_mean/",
      "{month_folder}/grids_germany_monthly_air_temp_mean_{yearmonth}.asc.gz"
    ),
    air_temperature_max = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/monthly/air_temperature_max/",
      "{month_folder}/grids_germany_monthly_air_temp_max_{yearmonth}.asc.gz"
    ),
    air_temperature_min = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/monthly/air_temperature_min/",
      "{month_folder}/grids_germany_monthly_air_temp_min_{yearmonth}.asc.gz"
    ),
    precipitation = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/monthly/precipitation/",
      "{month_folder}/grids_germany_monthly_precipitation_{yearmonth}.asc.gz"
    ),
    drought_index = paste0(
      "https://opendata.dwd.de/climate_environment/CDC/",
      "grids_germany/monthly/drought_index/",
      "{month_folder}/grids_germany_monthly_drought_index_{yearmonth}.asc.gz"
    )
  )
)

# Derive data source from catalogue name
.catalogue_source <- function(catalogue) {
  if (grepl("^dwd", catalogue)) "dwd" else "era5"
}

# Month folder name for DWD URLs (e.g. "01_Jan")
.dwd_month_folder <- function(month) {
  folders <- c(
    "01_Jan", "02_Feb", "03_Mar", "04_Apr", "05_May", "06_Jun",
    "07_Jul", "08_Aug", "09_Sep", "10_Oct", "11_Nov", "12_Dec"
  )
  folders[as.integer(month)]
}
