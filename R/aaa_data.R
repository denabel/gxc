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

# Physical units per indicator
.indicator_units <- list(
  # ERA5
  `2m_temperature`                       = "K",
  `total_precipitation`                  = "m",
  `10m_u_component_of_wind`              = "m/s",
  `10m_v_component_of_wind`              = "m/s",
  `instantaneous_10m_wind_gust`          = "m/s",
  `downward_uv_radiation_at_the_surface` = "J/m2",
  `total_cloud_cover`                    = "0-1",
  `k_index`                              = "K",
  `leaf_area_index_high_vegetation`      = "m2/m2",
  `leaf_area_index_low_vegetation`       = "m2/m2",
  `snow_cover`                           = "%",
  `snowfall`                             = "m",
  # DWD
  air_temperature_mean                   = "degC",
  air_temperature_max                    = "degC",
  air_temperature_min                    = "degC",
  precipitation                          = "mm",
  drought_index                          = "mm/degC"
)

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

# Spatial resolution of the underlying raster data per catalogue
.catalogue_resolution <- list(
  "derived-era5-land-daily-statistics"          = "0.1\u00b0 x 0.1\u00b0",
  "derived-era5-single-levels-daily-statistics" = "0.25\u00b0 x 0.25\u00b0",
  "reanalysis-era5-land-monthly-means"          = "0.1\u00b0 x 0.1\u00b0",
  "reanalysis-era5-single-levels-monthly-means" = "0.25\u00b0 x 0.25\u00b0",
  "dwd-hyras-daily"                             = "1 km x 1 km",
  "dwd-monthly"                                 = "1 km x 1 km"
)

# Returns the citation string for a given catalogue and indicator.
# ERA5 citations include the access date dynamically.
# DWD daily (HYRAS) and monthly citations differ per indicator.
.catalogue_citation <- function(catalogue, indicator = NULL) {
  year <- format(Sys.Date(), "%Y")
  date <- format(Sys.Date(), "%d.%m.%Y")

  era5 <- list(
    "derived-era5-land-daily-statistics" = paste0(
      "Copernicus Climate Change Service (C3S) (", year, "): ",
      "ERA5-Land post-processed daily statistics from 1950 to present. ",
      "Copernicus Climate Change Service Climate Data Store (CDS). ",
      "Accessed ", date, ". doi:10.24381/cds.e2161bac"
    ),
    "derived-era5-single-levels-daily-statistics" = paste0(
      "Copernicus Climate Change Service (C3S) (", year, "): ",
      "ERA5 post-processed daily statistics on single levels from 1940 to present. ",
      "Copernicus Climate Change Service Climate Data Store (CDS). ",
      "Accessed ", date, ". doi:10.24381/cds.adbb2d47"
    ),
    "reanalysis-era5-land-monthly-means" = paste0(
      "Copernicus Climate Change Service (C3S) (", year, "): ",
      "ERA5-Land monthly averaged data from 1950 to present. ",
      "Copernicus Climate Change Service Climate Data Store (CDS). ",
      "Accessed ", date, ". doi:10.24381/cds.68d2bb30"
    ),
    "reanalysis-era5-single-levels-monthly-means" = paste0(
      "Copernicus Climate Change Service (C3S) (", year, "): ",
      "ERA5 monthly averaged data on single levels from 1940 to present. ",
      "Copernicus Climate Change Service Climate Data Store (CDS). ",
      "Accessed ", date, ". doi:10.24381/cds.f17050d7"
    )
  )

  dwd_daily <- list(
    air_temperature_mean =
      "Raster data set of mean temperature in \u00b0C for Germany - HYRAS-DE-TAS v6-1, Version v6-1",
    air_temperature_max  =
      "Raster data set of maximum temperature in \u00b0C for Germany - HYRAS-DE-TASMAX v6-1, Version v6-1",
    air_temperature_min  =
      "Raster data set of minimum temperature in \u00b0C for Germany - HYRAS-DE-TASMIN v6-1, Version v6-1",
    precipitation        =
      "Raster data set precipitation sums in mm for Germany - HYRAS-DE-PR v6-1, Version v6-1"
  )

  dwd_monthly <- list(
    air_temperature_mean =
      "DWD Climate Data Center (CDC): Grids of monthly averaged daily air temperature (2m) over Germany, version v1.0",
    air_temperature_max  =
      "DWD Climate Data Center (CDC): Grids of monthly averaged daily maximum air temperature (2m) over Germany, version v1.0",
    air_temperature_min  =
      "DWD Climate Data Center (CDC): Grids of monthly averaged daily minimum air temperature (2m) over Germany, version v1.0",
    precipitation        =
      "DWD Climate Data Center (CDC): Grids of monthly total precipitation over Germany, version v1.0",
    drought_index        =
      "DWD Climate Data Center (CDC): Grids of monthly drought index (de Martonne) over Germany, version v1.0"
  )

  if (.catalogue_source(catalogue) == "era5") {
    era5[[catalogue]] %||% NA_character_
  } else if (catalogue == "dwd-hyras-daily") {
    dwd_daily[[indicator]] %||% NA_character_
  } else {
    dwd_monthly[[indicator]] %||% NA_character_
  }
}
