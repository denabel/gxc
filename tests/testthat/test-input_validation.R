test_that("link_daily errors on invalid catalogue", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", catalogue = "invalid"),
    "Invalid"
  )
})


test_that("link_daily errors on invalid indicator for catalogue", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "invalid"),
    "Invalid"
  )
})


test_that("link_daily errors on invalid statistic", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", statistic = "invalid"),
    "Invalid"
  )
})


test_that("link_daily errors on invalid time_zone", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", time_zone = "utc+99:00"),
    "Invalid"
  )
})


test_that("link_daily errors on count_above without time_span", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature",
               stat_wrangling = "count_above", time_span = 0),
    "requires"
  )
})


test_that("link_daily errors on invalid baseline length", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", baseline = c(1980)),
    "Invalid"
  )
})


test_that("link_daily errors when date_var is missing", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", date_var = "nonexistent"),
    "column"
  )
})


test_that("link_daily.SpatRaster errors when time dimension is missing", {
  pts  <- test_pts(seq = FALSE)
  grid <- terra::rast(nrows = 10, ncols = 10, xmin = 11, xmax = 14,
                      ymin = 48, ymax = 52)
  expect_error(
    link_daily(grid, indicator = "2m_temperature"),
    "time"
  )
})


test_that("link_monthly errors on months and time_span combined", {
  pts <- test_pts()
  expect_error(
    link_monthly(pts, indicator = "2m_temperature",
                 months = c(3, 4, 5), time_span = 3),
    "cannot both be specified"
  )
})


test_that("link_monthly errors on invalid months", {
  pts <- test_pts()
  expect_error(
    link_monthly(pts, indicator = "2m_temperature", months = c(0, 13)),
    "Invalid"
  )
})


test_that("link_monthly errors on invalid by_hour", {
  pts <- test_pts()
  expect_error(
    link_monthly(pts, indicator = "2m_temperature", by_hour = "25:00"),
    "Invalid"
  )
})


test_that("link_monthly errors on count_above without time_span or months", {
  pts <- test_pts()
  expect_error(
    link_monthly(pts, indicator = "2m_temperature",
                 stat_wrangling = "count_above", time_span = 0),
    "requires"
  )
})


test_that("link_daily errors on invalid DWD indicator", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "invalid", catalogue = "dwd-hyras-daily"),
    "Invalid"
  )
})

