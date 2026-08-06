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

  # Two distinct months, for the row-order regression test on link_monthly()
  # (analogous to pts_seq for link_daily()) -- see test-consistency.R
  pts_seq_monthly <- test_pts(seq = TRUE)
  pts_seq_monthly$date <- as_date(c("2014-08-01", "2014-09-01"))

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

  # baseline_fun/stat_wrangling combination list — no new download, reuses
  # the deviation baseline files already fetched above. Only the final
  # aggregation differs between combinations, so no separate fixture call
  # is strictly required -- included here anyway for clarity/documentation.
  link_daily(pts, indicator = "2m_temperature",
             baseline = c(1980, 1981),
             baseline_fun = list("mean", "median"),
             stat_wrangling = list("deviation", "deviation"),
             cache = TRUE, path = cache_path)

  # downsample_factor — no new download, aggregates already-fetched files
  # in memory. Buffer > 0 needed (downsampling never applies to points).
  link_daily(pts, indicator = "2m_temperature",
             buffer = 5000,
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             downsample_factor = 5, downsample_min_buffer = 0,
             cache = TRUE, path = cache_path)

  # months — needs a FULL month of daily files (31 days), unlike the
  # time_span-based fixtures above (0 or 1 day). pts$date is 2014-08-01;
  # months = c(8) includes the observation's own month, so the window is
  # shifted back one year to 2013-08-01 through 2013-08-31.
  link_daily(pts, indicator = "2m_temperature",
             months = c(8),
             cache = TRUE, path = cache_path)

  link_daily(pts, indicator = "2m_temperature",
             months = c(8), baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             cache = TRUE, path = cache_path)

  # count_above with months — same files as the deviation/months fixture
  # above, no new download
  link_daily(pts, indicator = "2m_temperature",
             months = c(8), baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "count_above",
             cache = TRUE, path = cache_path)

  # Note: the year-boundary (winter, months = c(12,1,2)) case is covered by
  # a pure unit test on .transform_time() instead of an end-to-end fixture
  # download here -- that logic is pure date arithmetic, independent of
  # any actual raster data, so a ~90-day download would only add fixture
  # size without adding real test coverage.

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

  # Monthly — sequential dates (two distinct months), for the row-order
  # regression test in test-consistency.R
  link_monthly(pts_seq_monthly, indicator = "2m_temperature",
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

  # Monthly — downsample_factor — no new download, aggregates
  # already-fetched files in memory
  link_monthly(pts, indicator = "2m_temperature",
               buffer = 5000,
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               downsample_factor = 5, downsample_min_buffer = 0,
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

  # Daily — months — minimal smoke test only (no baseline). The `months`
  # logic itself (.transform_time()/.resolve_months()) is catalogue-
  # independent and already fully verified via the ERA5 fixtures above;
  # this just confirms the DWD code path doesn't error, without paying for
  # a second full month of (much larger, whole-Germany) DWD daily files.
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             months = c(8),
             cache = TRUE, path = cache_path)

  # Daily — downsample_factor — no new download, aggregates already-fetched
  # files in memory
  link_daily(pts, indicator = "air_temperature_mean",
             catalogue = "dwd-hyras-daily",
             buffer = 5000,
             baseline = c(1980, 1981), baseline_fun = "mean",
             stat_wrangling = "deviation",
             downsample_factor = 5, downsample_min_buffer = 0,
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
