test_that("link_daily works with points", {
  fail_on_request()
  pts <- test_era5(seq = FALSE)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = test_cache("ecmwfr")
  )

  expect_s3_class(result, "sf")
  expect_named(result, c(".linked", ".baseline", ".deviation"))
  expect_equal(round(result$.linked, 2), c(291.82, 295.06))
  expect_equal(round(result$.baseline, 2), c(291.29, 293.15))
})


test_that("link_daily works with time spans", {
  fail_on_request()
  pts <- test_era5(seq = TRUE)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = test_cache("ecmwfr")
  )

  expect_s3_class(result, "sf")
  expect_named(result, c(".linked", ".baseline", ".deviation"))
  expect_equal(round(result$.linked, 2), c(291.82, 295.06))
  expect_equal(round(result$.baseline, 2), c(291.29, 293.15))
})


test_that("link_daily works with rasters", {
  fail_on_request()
  pts <- test_era5()
  grid <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid) <- sf::st_crs(pts)$wkt

  result <- link_daily(
    grid,
    indicator = "2m_temperature",
    baseline = c(1980, 1981),
    cache = TRUE,
    path = test_cache("ecmwfr")
  )
})
