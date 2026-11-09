test_that("non-accumulated indicators keep their base unit", {
  expect_equal(.indicator_unit("2m_temperature", "derived-era5-land-daily-statistics", "daily_mean"), "K")
  expect_equal(.indicator_unit("2m_temperature", "reanalysis-era5-land-monthly-means"), "K")
  expect_equal(.indicator_unit("air_temperature_mean", "dwd-hyras-daily"), "degC")
})

test_that("ERA5 accumulations are labelled per hour or per day", {
  cat_daily <- "derived-era5-single-levels-daily-statistics"
  expect_equal(.indicator_unit("total_precipitation", cat_daily, "daily_mean"), "m/h")
  expect_equal(.indicator_unit("total_precipitation", cat_daily, "daily_maximum"), "m/h")
  expect_equal(.indicator_unit("total_precipitation", cat_daily, "daily_sum"), "m/day")
  expect_equal(.indicator_unit("total_precipitation", "reanalysis-era5-land-monthly-means"), "m/day")
  expect_equal(.indicator_unit("downward_uv_radiation_at_the_surface", cat_daily, "daily_sum"), "J/m2/day")
})

test_that("DWD precipitation is labelled per day or per month", {
  expect_equal(.indicator_unit("precipitation", "dwd-hyras-daily"), "mm/day")
  expect_equal(.indicator_unit("precipitation", "dwd-monthly"), "mm/month")
})

test_that("result unit follows the indicator unit for deviations", {
  expect_equal(
    .result_unit("deviation", "total_precipitation",
                 "derived-era5-single-levels-daily-statistics", "daily_sum"),
    "m/day"
  )
  expect_equal(
    .result_unit("count_above", "total_precipitation",
                 "derived-era5-single-levels-daily-statistics", "daily_sum"),
    "days"
  )
})

test_that("daily_sum is only allowed for accumulated ERA5 indicators", {
  cat_daily <- "derived-era5-single-levels-daily-statistics"
  expect_no_error(.check_valid_statistic("daily_sum", "total_precipitation", cat_daily))
  expect_error(.check_valid_statistic("daily_sum", "2m_temperature", cat_daily), "daily_sum")
  expect_no_error(.check_valid_statistic("daily_mean", "2m_temperature", cat_daily))
  expect_error(.check_valid_statistic("daily_median", "2m_temperature", cat_daily), "Invalid")
})

test_that("ARCO units follow the statistic", {
  expect_equal(.indicator_unit("total_precipitation", "arco-era5-land-hourly", "daily_mean"), "m/h")
  expect_equal(.indicator_unit("total_precipitation", "arco-era5-land-hourly", "daily_sum"), "m/day")
  expect_equal(.indicator_unit("2m_temperature", "arco-era5-land-hourly", "daily_mean"), "K")
})
