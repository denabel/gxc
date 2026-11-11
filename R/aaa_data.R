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
  "dwd-hyras-daily",
  "arco-era5-land-hourly"
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
  `arco-era5-land-hourly` = c(
    "2m_temperature",
    "2m_dewpoint_temperature",
    "total_precipitation",
    "surface_pressure",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "surface_solar_radiation_downwards",
    "surface_thermal_radiation_downwards",
    "skin_temperature",
    "soil_temperature_level_1",
    "soil_temperature_level_2",
    "soil_temperature_level_3",
    "soil_temperature_level_4",
    "volumetric_soil_water_layer_1",
    "volumetric_soil_water_layer_2",
    "volumetric_soil_water_layer_3",
    "volumetric_soil_water_layer_4",
    "snow_cover",
    "snow_depth",
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
allowed_statistic <- c("daily_mean", "daily_maximum", "daily_minimum", "daily_sum")
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
  # ERA5-Land (ARCO)
  `2m_dewpoint_temperature`              = "K",
  `surface_pressure`                     = "Pa",
  `surface_solar_radiation_downwards`    = "J/m2",
  `surface_thermal_radiation_downwards`  = "J/m2",
  `skin_temperature`                     = "K",
  `soil_temperature_level_1`             = "K",
  `soil_temperature_level_2`             = "K",
  `soil_temperature_level_3`             = "K",
  `soil_temperature_level_4`             = "K",
  `volumetric_soil_water_layer_1`        = "m3/m3",
  `volumetric_soil_water_layer_2`        = "m3/m3",
  `volumetric_soil_water_layer_3`        = "m3/m3",
  `volumetric_soil_water_layer_4`        = "m3/m3",
  `snow_depth`                           = "m",
  # DWD
  air_temperature_mean                   = "degC",
  air_temperature_max                    = "degC",
  air_temperature_min                    = "degC",
  precipitation                          = "mm",
  drought_index                          = "mm/degC"
)

# ERA5 indicators stored as hourly accumulations. Daily statistics of these
# are amounts per hour; daily_sum gives the daily total. Monthly means are
# mean daily totals.
.accumulated_indicators <- c(
  "total_precipitation",
  "downward_uv_radiation_at_the_surface",
  "snowfall",
  "surface_solar_radiation_downwards",
  "surface_thermal_radiation_downwards"
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
  if (grepl("^dwd", catalogue)) "dwd" else if (grepl("^arco", catalogue)) "arco" else "era5"
}

# ERA5-Land ARCO Zarr stores (geo- and time-chunked) and array names per
# indicator. Several indicators share a store.
.arco_url <- function(id, group, layout) {
  sprintf(
    "https://arco.datastores.ecmwf.int/cadl-arco-%s-%s/arco/reanalysis_era5_land/%s/%sChunked.zarr",
    layout, id, group, layout
  )
}

.arco_store <- function(id, group, array) {
  list(
    geo   = .arco_url(id, group, "geo"),
    time  = .arco_url(id, group, "time"),
    array = array
  )
}

.arco_stores <- list(
  `2m_temperature`                      = .arco_store("007", "sfc-2m-temperature", "t2m"),
  `2m_dewpoint_temperature`             = .arco_store("007", "sfc-2m-temperature", "d2m"),
  total_precipitation                   = .arco_store("009", "sfc-pressure-precipitation", "tp"),
  surface_pressure                      = .arco_store("009", "sfc-pressure-precipitation", "sp"),
  `10m_u_component_of_wind`             = .arco_store("008", "sfc-wind", "u10"),
  `10m_v_component_of_wind`             = .arco_store("008", "sfc-wind", "v10"),
  surface_solar_radiation_downwards     = .arco_store("010", "sfc-radiation-heat", "ssrd"),
  surface_thermal_radiation_downwards   = .arco_store("010", "sfc-radiation-heat", "strd"),
  skin_temperature                      = .arco_store("043", "sfc-skin-temperature", "skt"),
  soil_temperature_level_1              = .arco_store("006", "sfc-soil-temperature", "stl1"),
  soil_temperature_level_2              = .arco_store("006", "sfc-soil-temperature", "stl2"),
  soil_temperature_level_3              = .arco_store("006", "sfc-soil-temperature", "stl3"),
  soil_temperature_level_4              = .arco_store("006", "sfc-soil-temperature", "stl4"),
  volumetric_soil_water_layer_1         = .arco_store("005", "sfc-soil-water", "swvl1"),
  volumetric_soil_water_layer_2         = .arco_store("005", "sfc-soil-water", "swvl2"),
  volumetric_soil_water_layer_3         = .arco_store("005", "sfc-soil-water", "swvl3"),
  volumetric_soil_water_layer_4         = .arco_store("005", "sfc-soil-water", "swvl4"),
  snow_cover                            = .arco_store("030", "sfc-snow", "snowc"),
  snow_depth                            = .arco_store("030", "sfc-snow", "sde"),
  snowfall                              = .arco_store("030", "sfc-snow", "sf")
)

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
  "dwd-monthly"                                 = "1 km x 1 km",
  "arco-era5-land-hourly"                       = "0.1\u00b0 x 0.1\u00b0"
)

# Spatial coverage per catalogue
.catalogue_coverage <- list(
  "derived-era5-land-daily-statistics"          = "global (land only)",
  "derived-era5-single-levels-daily-statistics" = "global",
  "reanalysis-era5-land-monthly-means"          = "global (land only)",
  "reanalysis-era5-single-levels-monthly-means" = "global",
  "dwd-hyras-daily"                             = "Germany",
  "dwd-monthly"                                 = "Germany",
  "arco-era5-land-hourly"                       = "global (land only)"
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
      "Accessed ", date, ". doi:10.24381/cds.e9c9c792"
    ),
    "arco-era5-land-hourly" = paste0(
      "Copernicus Climate Change Service (C3S) (", year, "): ",
      "ERA5-Land hourly data from 1950 to present, ",
      "Analysis Ready Cloud Optimised (ARCO) version. ",
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

  if (.catalogue_source(catalogue) %in% c("era5", "arco")) {
    era5[[catalogue]] %||% NA_character_
  } else if (catalogue == "dwd-hyras-daily") {
    dwd_daily[[indicator]] %||% NA_character_
  } else {
    dwd_monthly[[indicator]] %||% NA_character_
  }
}
