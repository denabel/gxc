# tests/testthat/setup-fixtures.R
if (FALSE) {

  library(gxc)

  go_on_request()

  cache_era5 <- file.path("tests", "testthat", "fixtures", "era5")
  cache_dwd  <- file.path("tests", "testthat", "fixtures", "dwd")
  dir.create(cache_era5, showWarnings = FALSE, recursive = TRUE)
  dir.create(cache_dwd,  showWarnings = FALSE, recursive = TRUE)

  pts     <- test_pts(seq = FALSE)
  pts_seq <- test_pts(seq = TRUE)
  pts_lag <- test_pts(seq = FALSE)
  pts_lag$date <- pts_lag$date + days(1)

  grid <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt

  # -----------------------------------------------------------------------
  # ERA5 fixtures
  # -----------------------------------------------------------------------

  # Simple — also used by ground truth tests
  link_daily(pts, indicator = "2m_temperature",
             cache = TRUE, path = cache_era5)

  # Deviation baseline — shared by multiple tests
  link_daily(pts, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_era5)

  # sd_deviation — same request as deviation, no new download
  link_daily(pts, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "sd_deviation",
             cache = TRUE, path = cache_era5)

  # count_above — needs time_span > 0
  link_daily(pts_lag, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "count_above", time_span = 1,
             cache = TRUE, path = cache_era5)

  # Sequential dates
  link_daily(pts_seq, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_era5)

  # time_span deviation — same files as count_above
  link_daily(pts_lag, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation", time_span = 1,
             cache = TRUE, path = cache_era5)

  # time_lag — no new download, same files as simple
  link_daily(pts, indicator = "2m_temperature",
             time_lag = 1,
             cache = TRUE, path = cache_era5)

  # buffer — no new download, same files as simple
  link_daily(pts, indicator = "2m_temperature",
             buffer = 5000,
             cache = TRUE, path = cache_era5)

  # prefix — no new download
  link_daily(pts, indicator = "2m_temperature",
             prefix = "temp",
             cache = TRUE, path = cache_era5)

  # SpatRaster
  link_daily(grid, indicator = "2m_temperature",
             cache = TRUE, path = cache_era5)
  link_daily(grid, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_era5)

  # Monthly — simple
  link_monthly(pts, indicator = "2m_temperature",
               cache = TRUE, path = cache_era5)

  # Monthly — deviation baseline
  link_monthly(pts, indicator = "2m_temperature",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_era5)

  # Monthly — explicit months
  link_monthly(pts, indicator = "2m_temperature",
               months = c(6, 7, 8), baseline = c(1980, 1981),
               baseline_fun = "mean", stat_wrangling = "deviation",
               cache = TRUE, path = cache_era5)

  # Monthly — buffer
  link_monthly(pts, indicator = "2m_temperature",
               buffer = 5000,
               cache = TRUE, path = cache_era5)

  # Monthly — SpatRaster
  link_monthly(grid, indicator = "2m_temperature",
               cache = TRUE, path = cache_era5)
  link_monthly(grid, indicator = "2m_temperature",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_era5)

  reset_test_index(cache_era5, service = "ecmwfr")

  # -----------------------------------------------------------------------
  # DWD fixtures
  # -----------------------------------------------------------------------

  # Daily — simple
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             cache = TRUE, path = cache_dwd)

  # Daily — deviation baseline
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_dwd)

  # Daily — time_span
  link_daily(pts_lag, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation", time_span = 1,
             cache = TRUE, path = cache_dwd)

  # Daily — SpatRaster
  link_daily(grid, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             cache = TRUE, path = cache_dwd)

  # Monthly — simple
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               cache = TRUE, path = cache_dwd)

  # Monthly — deviation baseline
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_dwd)

  # Monthly — explicit months
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               months = c(6, 7, 8), baseline = c(1980, 1981),
               baseline_fun = "mean", stat_wrangling = "deviation",
               cache = TRUE, path = cache_dwd)

  # Monthly — SpatRaster
  link_monthly(grid, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               cache = TRUE, path = cache_dwd)

  reset_test_index(cache_dwd, service = "dwd")

  message("All fixtures generated successfully.")
}
