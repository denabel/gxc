test_that("lookup lists one detail row per catalogue", {
  x <- lookup("total_precipitation")
  expect_s3_class(x, "gxc_indicator")
  expect_equal(nrow(x$details), length(x$catalogues))
  expect_setequal(x$details$catalogue, x$catalogues)
})

test_that("lookup reports statistics and units per catalogue", {
  d <- lookup("total_precipitation")$details
  daily <- d[d$catalogue == "derived-era5-single-levels-daily-statistics", ]
  expect_match(daily$statistics, "daily_sum")
  expect_equal(daily$unit, "m/h (daily_sum: m/day)")

  monthly <- d[d$catalogue == "reanalysis-era5-land-monthly-means", ]
  expect_true(is.na(monthly$statistics))
  expect_equal(monthly$unit, "m/day")
})

test_that("lookup offers no daily_sum for non-accumulated indicators", {
  d <- lookup("2m_temperature")$details
  expect_false(any(grepl("daily_sum", d$statistics)))
  expect_true(all(d$unit == "K"))
})

test_that("lookup prints without error", {
  expect_output(print(lookup("precipitation")), "dwd-hyras-daily")
})
