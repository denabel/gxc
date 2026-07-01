# tests/testthat/setup-fixtures.R
if (FALSE) {

  library(gxc)

  go_on_request()

  cache_path <- file.path("tests", "testthat", "fixtures")
  dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(cache_path,  showWarnings = FALSE, recursive = TRUE)

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
             cache = TRUE, path = cache_path)

  # Deviation baseline — shared by multiple tests
  link_daily(pts, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_path)

  # sd_deviation — same request as deviation, no new download
  link_daily(pts, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "sd_deviation",
             cache = TRUE, path = cache_path)

  # count_above — needs time_span > 0
  link_daily(pts_lag, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "count_above", time_span = 1,
             cache = TRUE, path = cache_path)

  # Sequential dates
  link_daily(pts_seq, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_path)

  # time_span deviation — same files as count_above
  link_daily(pts_lag, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation", time_span = 1,
             cache = TRUE, path = cache_path)

  # time_lag — no new download, same files as simple
  link_daily(pts, indicator = "2m_temperature",
             time_lag = 1,
             cache = TRUE, path = cache_path)

  # buffer — no new download, same files as simple
  link_daily(pts, indicator = "2m_temperature",
             buffer = 5000,
             cache = TRUE, path = cache_path)

  # prefix — no new download
  link_daily(pts, indicator = "2m_temperature",
             prefix = "temp",
             cache = TRUE, path = cache_path)

  # SpatRaster
  link_daily(grid, indicator = "2m_temperature",
             cache = TRUE, path = cache_path)
  link_daily(grid, indicator = "2m_temperature",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_path)

  # Monthly — simple
  link_monthly(pts, indicator = "2m_temperature",
               cache = TRUE, path = cache_path)

  # Monthly — deviation baseline
  link_monthly(pts, indicator = "2m_temperature",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_path)

  # Monthly — explicit months
  link_monthly(pts, indicator = "2m_temperature",
               months = c(6, 7, 8), baseline = c(1980, 1981),
               baseline_fun = "mean", stat_wrangling = "deviation",
               cache = TRUE, path = cache_path)

  # Monthly — buffer
  link_monthly(pts, indicator = "2m_temperature",
               buffer = 5000,
               cache = TRUE, path = cache_path)

  # Monthly — SpatRaster
  link_monthly(grid, indicator = "2m_temperature",
               cache = TRUE, path = cache_path)
  link_monthly(grid, indicator = "2m_temperature",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_path)

  reset_test_index(cache_path, service = "ecmwfr")

  # -----------------------------------------------------------------------
  # DWD fixtures
  # -----------------------------------------------------------------------

  # Daily — simple
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             cache = TRUE, path = cache_path)

  # Daily — deviation baseline
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_path)

  # Daily — time_span
  link_daily(pts_lag, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation", time_span = 1,
             cache = TRUE, path = cache_path)

  # Daily — SpatRaster
  link_daily(grid, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             cache = TRUE, path = cache_path)

  # Monthly — simple
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               cache = TRUE, path = cache_path)

  # Monthly — deviation baseline
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               cache = TRUE, path = cache_path)

  # Monthly — explicit months
  link_monthly(pts, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               months = c(6, 7, 8), baseline = c(1980, 1981),
               baseline_fun = "mean", stat_wrangling = "deviation",
               cache = TRUE, path = cache_path)

  # Monthly — SpatRaster
  link_monthly(grid, indicator = "air_temperature_mean",
               catalogue = "dwd-monthly",
               cache = TRUE, path = cache_path)

  reset_test_index(cache_path, service = "dwd")

  # -----------------------------------------------------------------------
  # Catalogue coverage fixtures — one request per indicator per catalogue,
  # as small as possible (single point, single date)
  # -----------------------------------------------------------------------
  #
  failed <- character(0)

  for (cat in names(allowed_indicators_by_catalogue)) {
    fn      <- if (grepl("daily", cat)) "link_daily"   else "link_monthly"
    service <- if (grepl("dwd",   cat)) "dwd"          else "ecmwfr"

    for (ind in allowed_indicators_by_catalogue[[cat]]) {
      message(paste0(cat, ": ", ind, "\n"))
      tryCatch(
        do.call(fn, list(pts, indicator = ind, catalogue = cat,
                         cache = TRUE, path = cache_path, verbose = FALSE)),
        error = function(e) {
          failed <<- c(failed, sprintf("%s / %s", cat, ind))
          message("  FAILED: ", cat, " / ", ind, "\n  ", conditionMessage(e))
        }
      )
    }
  }

  reset_test_index(cache_path, service = "ecmwfr")
  reset_test_index(cache_path, service = "dwd")

  if (length(failed) > 0) {
    message("The following catalogue/indicator combinations failed:\n",
            paste0("  - ", failed, collapse = "\n"))
  }

  message("All fixtures generated successfully.")
}
