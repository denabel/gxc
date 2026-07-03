# test-ground_truth.R

local_key(service = "ecmwfr")
local_key(service = "dwd")

# -------------------------------------------------------------------------
# ERA5 daily ground truth
# -------------------------------------------------------------------------

test_that("link_daily.sf study values match direct terra::extract (ERA5)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)
  extent <- .test_extent(pts)

  result <- link_daily(pts, indicator = "2m_temperature",
                       cache = TRUE, path = cache)

  expected <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = "2014", months = "08", days = "01",
    pts       = pts
  )

  expect_equal(round(result$.study, 4), round(expected, 4))
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_daily.sf baseline matches direct terra::extract (ERA5)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)
  extent <- .test_extent(pts)

  result <- link_daily(pts, indicator = "2m_temperature",
                       baseline = c(1980, 1981), baseline_fun = "mean",
                       stat_wrangling = "deviation",
                       cache = TRUE, path = cache)

  expected_study <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = "2014", months = "08", days = "01",
    pts       = pts
  )

  expected_baseline <- .extract_baseline_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = c("1980", "1981"),
    months    = c("08", "08"),
    days      = c("01", "01"),
    pts       = pts
  )

  expect_equal(round(result$.study,    4), round(expected_study,    4))
  expect_equal(round(result$.baseline, 4), round(expected_baseline, 4))
  expect_equal(round(result$.result,   4),
               round(expected_study - expected_baseline, 4))
})


test_that("link_daily.sf with time_span matches aggregated terra::extract (ERA5)", {
  skip_on_cran()
  fail_on_request()
  pts_lag <- test_pts(seq = FALSE)
  pts_lag$date <- pts_lag$date + days(1)
  cache      <- test_cache()
  local_test_index(cache)
  extent <- .test_extent(pts_lag, buffer = 0)

  result <- link_daily(pts_lag, indicator = "2m_temperature",
                       baseline = c(1980, 1981), baseline_fun = "mean",
                       stat_wrangling = "deviation", time_span = 1,
                       cache = TRUE, path = cache)

  all_obs_span <- seq(as_date("2014-08-01"), as_date("2014-08-02"), by = "1 day")

  day1 <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(all_obs_span, "%Y"),
    months    = format(all_obs_span, "%m"),
    days      = format(all_obs_span, "%d"),
    pts       = pts_lag,
    layer     = 1
  )
  day2 <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(all_obs_span, "%Y"),
    months    = format(all_obs_span, "%m"),
    days      = format(all_obs_span, "%d"),
    pts       = pts_lag,
    layer     = 2
  )
  expected_study <- (day1 + day2) / 2

  expect_equal(round(result$.study, 4), round(expected_study, 4))
})


# -------------------------------------------------------------------------
# ERA5 monthly ground truth
# -------------------------------------------------------------------------

test_that("link_monthly.sf study values match direct terra::extract (ERA5)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)
  extent <- .test_extent(pts)

  result <- link_monthly(pts, indicator = "2m_temperature",
                         cache = TRUE, path = cache)

  expected <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "reanalysis-era5-land-monthly-means",
    extent    = extent,
    years     = "2014", months = "08",
    pts       = pts
  )

  expect_equal(round(result$.study, 4), round(expected, 4))
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_monthly.sf baseline matches direct terra::extract (ERA5)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)
  extent <- .test_extent(pts)

  result <- link_monthly(pts, indicator = "2m_temperature",
                         baseline = c(1980, 1981), baseline_fun = "mean",
                         stat_wrangling = "deviation",
                         cache = TRUE, path = cache)

  expected_study <- .extract_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "reanalysis-era5-land-monthly-means",
    extent    = extent,
    years     = "2014", months = "08",
    pts       = pts
  )

  expected_baseline <- .extract_baseline_direct(
    cache,
    indicator = "2m_temperature",
    catalogue = "reanalysis-era5-land-monthly-means",
    extent    = extent,
    years     = c("1980", "1981"),
    months    = c("08", "08"),
    pts       = pts
  )

  expect_equal(round(result$.study,    4), round(expected_study,    4))
  expect_equal(round(result$.baseline, 4), round(expected_baseline, 4))
  expect_equal(round(result$.result,   4),
               round(expected_study - expected_baseline, 4))
})


# -------------------------------------------------------------------------
# DWD daily ground truth
# -------------------------------------------------------------------------

test_that("link_daily.sf study values match direct terra::extract (DWD)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts_dwd(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(pts, indicator = "air_temperature_mean",
                       catalogue = "dwd-hyras-daily",
                       cache = TRUE, path = cache)

  # Direct extraction from cached .tif
  cached_file <- list.files(
    file.path(cache, "dwd", "daily"),
    pattern     = "observation.*20240101",
    full.names  = TRUE
  )
  obs_raster <- terra::rast(cached_file)
  pts_r      <- sf::st_transform(pts, terra::crs(obs_raster))
  expected   <- terra::extract(
    obs_raster[[1]], pts_r, fun = mean, na.rm = TRUE, ID = FALSE
  )[[1]]

  expect_equal(round(result$.study, 4), round(expected, 4))
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_daily.sf baseline matches direct terra::extract (DWD)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts_dwd(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(pts, indicator = "air_temperature_mean",
                       catalogue      = "dwd-hyras-daily",
                       baseline       = c(1980, 1981),
                       baseline_fun   = "mean",
                       stat_wrangling = "deviation",
                       cache = TRUE, path = cache)

  # Direct extraction from cached observation
  obs_file   <- list.files(file.path(cache, "dwd", "daily"),
                           pattern = "observation.*20240101", full.names = TRUE)
  obs_raster <- terra::rast(obs_file)
  pts_obs    <- sf::st_transform(pts, terra::crs(obs_raster))
  expected_study <- terra::extract(
    obs_raster[[1]], pts_obs, fun = mean, na.rm = TRUE, ID = FALSE
  )[[1]]

  # Direct extraction from cached baseline
  bl_files <- sort(list.files(file.path(cache, "dwd", "daily"),
                              pattern = "baseline.*198[01]0101", full.names = TRUE))
  bl_rasters <- lapply(bl_files, terra::rast)
  bl_values  <- sapply(bl_rasters, function(r) {
    pts_r <- sf::st_transform(pts, terra::crs(r))
    terra::extract(r[[1]], pts_r, fun = mean, na.rm = TRUE, ID = FALSE)[[1]]
  })
  expected_baseline <- apply(bl_values, 1, mean)

  expect_equal(round(result$.study,    4), round(expected_study,    4))
  expect_equal(round(result$.baseline, 4), round(expected_baseline, 4))
  expect_equal(round(result$.result,   4),
               round(expected_study - expected_baseline, 4))
})


# -------------------------------------------------------------------------
# DWD monthly ground truth
# -------------------------------------------------------------------------

test_that("link_monthly.sf study values match direct terra::extract (DWD)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts_dwd(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(pts, indicator = "air_temperature_mean",
                         catalogue = "dwd-monthly",
                         cache = TRUE, path = cache)

  obs_file   <- list.files(file.path(cache, "dwd", "monthly"),
                           pattern = "observation.*202401", full.names = TRUE)
  obs_raster <- terra::rast(obs_file)
  pts_r      <- sf::st_transform(pts, terra::crs(obs_raster))
  expected   <- terra::extract(
    obs_raster[[1]], pts_r, fun = mean, na.rm = TRUE, ID = FALSE
  )[[1]]

  expect_equal(round(result$.study, 4), round(expected, 4))
})


test_that("link_monthly.sf baseline matches direct terra::extract (DWD)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts_dwd(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(pts, indicator = "air_temperature_mean",
                         catalogue      = "dwd-monthly",
                         baseline       = c(1980, 1981),
                         baseline_fun   = "mean",
                         stat_wrangling = "deviation",
                         cache = TRUE, path = cache)

  obs_file   <- list.files(file.path(cache, "dwd", "monthly"),
                           pattern = "observation.*202401", full.names = TRUE)
  obs_raster <- terra::rast(obs_file)
  pts_obs    <- sf::st_transform(pts, terra::crs(obs_raster))
  expected_study <- terra::extract(
    obs_raster[[1]], pts_obs, fun = mean, na.rm = TRUE, ID = FALSE
  )[[1]]

  bl_files <- sort(list.files(file.path(cache, "dwd", "monthly"),
                              pattern = "baseline.*198[01]01", full.names = TRUE))
  bl_rasters <- lapply(bl_files, terra::rast)
  bl_values  <- sapply(bl_rasters, function(r) {
    pts_r <- sf::st_transform(pts, terra::crs(r))
    terra::extract(r[[1]], pts_r, fun = mean, na.rm = TRUE, ID = FALSE)[[1]]
  })
  expected_baseline <- apply(bl_values, 1, mean)

  expect_equal(round(result$.study,    4), round(expected_study,    4))
  expect_equal(round(result$.baseline, 4), round(expected_baseline, 4))
  expect_equal(round(result$.result,   4),
               round(expected_study - expected_baseline, 4))
})

