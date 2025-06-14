local_key(service = "ecmwfr")

test_that("link_daily works with points", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = cache
  )

  expect_s3_class(result, "sf")
  expect_named(result, c("date", ".linked", ".baseline", ".deviation", "geometry"))
  expect_equal(round(result$.linked, 2), c(290.51, 293.18))
  expect_equal(round(result$.baseline, 2), c(290.19, 291.34))
})


test_that("link_daily works with intervals", {
  fail_on_request()
  pts <- test_era5(seq = TRUE)
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = cache
  )

  expect_s3_class(result, "sf")
  expect_named(result, c("date", ".linked", ".baseline", ".deviation", "geometry"))
  expect_equal(round(result$.linked, 2), c(290.51, 295.19))
  expect_equal(round(result$.baseline, 2), c(290.19, 292.99))
})


test_that("link_daily works with time spans", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)
  pts$date <- pts$date + days(1)
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    time_span = 1,
    cache = TRUE,
    path = cache
  )

  expect_s3_class(result, "sf")
  expect_named(result, c("date", ".linked", ".baseline", ".deviation", "geometry"))
  expect_equal(round(result$.linked, 2), c(291.51, 294.18))
  expect_equal(round(result$.baseline, 2), c(291.08, 292.16))
})


test_that("link_daily works with rasters", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)
  grid <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid) <- sf::st_crs(pts)$wkt
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_daily(
    grid,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_named(result, c(".linked", ".baseline", ".deviation"))
  means <- lapply(as.data.frame(result), mean)
  expect_equal(round(means$.linked, 2), 292.02)
  expect_equal(round(means$.baseline, 2), 290.67)
})


test_that("link_monthly works with points", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_monthly(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = cache
  )

  expect_s3_class(result, "sf")
  expect_named(result, c("date", ".linked", ".baseline", ".deviation", "geometry"))
  expect_equal(round(result$.linked, 2), c(287.8, 290.11))
  expect_equal(round(result$.baseline, 2), c(288.06, 289.73))
})


test_that("link_daily works with rasters", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)
  grid <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid) <- sf::st_crs(pts)$wkt
  cache <- test_cache("ecmwfr")
  local_test_index(cache)

  result <- link_monthly(
    grid,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_named(result, c(".linked", ".baseline", ".deviation"))
  means <- lapply(as.data.frame(result), mean)
  expect_equal(round(means$.linked, 2), 289.03)
  expect_equal(round(means$.baseline, 2), 288.95)
})
