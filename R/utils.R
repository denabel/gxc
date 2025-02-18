# R/utils.R

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
  "reanalysis-era5-land-monthly-means" = c("2m_temperature",
                                           "total_precipitation",
                                           "10m_u_component_of_wind",
                                           "10m_v_component_of_wind",
                                           #"10m_wind_speed", # does not exist in catalogue
                                           "leaf_area_index_high_vegetation",
                                           "leaf_area_index_low_vegetation",
                                           #"snow_cover", # time stamp first day of NEXT month, if by_hour, then LAST day of focal month
                                           "snowfall"
  ),
  "reanalysis-era5-single-levels-monthly-means" = c("2m_temperature",
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
  "derived-era5-land-daily-statistics" = c("2m_temperature"
                                           #"snow_cover",
                                           #"10m_u_component_of_wind",
                                           #"10m_v_component_of_wind",
                                           #"leaf_area_index_high_vegetation",
                                           #"leaf_area_index_low_vegetation"
  ),
  "derived-era5-single-levels-daily-statistics" = c("2m_temperature"
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

