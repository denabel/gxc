local_key(service = "dwd")

# -------------------------------------------------------------------------
# link_daily — sf
# -------------------------------------------------------------------------

test_that("link_daily.sf works with DWD simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(
    pts,
    indicator = "air_temperature_mean",
    catalogue = "dwd-hyras-daily",
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_named(
    result,
    c("date", ".study", ".baseline", ".result",
      ".indicator", ".unit", ".resolution", ".time_unit", ".result_unit",
      ".study_fun", ".baseline_fun", ".baseline_years",
      ".time_span", ".time_lag", ".buffer", ".source", "geometry")
  )
  expect_true(all(is.na(result$.baseline)))
  expect_equal(unique(result$.indicator), "air_temperature_mean")
  expect_equal(unique(result$.unit), "degC")
  expect_equal(unique(result$.time_unit), "days")
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_daily.sf works with DWD deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(
    pts,
    indicator      = "air_temperature_mean",
    catalogue      = "dwd-hyras-daily",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s3_class(result, "sf")
  expect_false(any(is.na(result$.result)))
  expect_equal(unique(result$.result_unit), "degC")
  expect_equal(round(result$.result, 4), round(result$.study - result$.baseline, 4))
})


test_that("link_daily.sf works with DWD time_span deviation", {
  fail_on_request()
  pts        <- test_pts(seq = FALSE)
  pts$date   <- pts$date + days(1)
  cache      <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(
    pts,
    indicator      = "air_temperature_mean",
    catalogue      = "dwd-hyras-daily",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    time_span      = 1,
    cache          = TRUE,
    path           = cache
  )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.time_span), 1)
  expect_false(any(is.na(result$.result)))
})


test_that("link_daily.SpatRaster works with DWD simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_daily(
    grid,
    indicator = "air_temperature_mean",
    catalogue = "dwd-hyras-daily",
    cache     = TRUE,
    path      = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(".study" %in% names(result))
  expect_equal(get_metag(result, "unit"), "degC")
})


# -------------------------------------------------------------------------
# link_monthly — sf
# -------------------------------------------------------------------------

test_that("link_monthly.sf works with DWD simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(
    pts,
    indicator = "air_temperature_mean",
    catalogue = "dwd-monthly",
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_true(all(is.na(result$.baseline)))
  expect_equal(unique(result$.unit), "degC")
  expect_equal(unique(result$.time_unit), "months")
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_monthly.sf works with DWD deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(
    pts,
    indicator      = "air_temperature_mean",
    catalogue      = "dwd-monthly",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s3_class(result, "sf")
  expect_false(any(is.na(result$.result)))
  expect_equal(round(result$.result, 4), round(result$.study - result$.baseline, 4))
})


test_that("link_monthly.sf works with DWD explicit months", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(
    pts,
    indicator      = "air_temperature_mean",
    catalogue      = "dwd-monthly",
    months         = c(6, 7, 8),
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.months), "6,7,8")
  expect_false(any(is.na(result$.result)))
})


test_that("link_monthly.SpatRaster works with DWD simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache, service = "dwd")

  result <- link_monthly(
    grid,
    indicator = "air_temperature_mean",
    catalogue = "dwd-monthly",
    cache     = TRUE,
    path      = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(".study" %in% names(result))
  expect_equal(get_metag(result, "time_unit"), "months")
})


# -------------------------------------------------------------------------
# Input validation
# -------------------------------------------------------------------------

test_that("link_daily errors on invalid DWD indicator", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "invalid", catalogue = "dwd-hyras-daily"),
    "Invalid"
  )
})
