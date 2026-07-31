test_that("link_daily with months produces the correct full daily window", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  # pts$date is 2014-08-01; months = c(8) includes the observation's own
  # month, so .resolve_months() shifts the window back one year ->
  # expected window is 2013-08-01 through 2013-08-31 (31 days).
  result <- link_daily(
    pts, indicator = "2m_temperature",
    months = c(8),
    cache  = TRUE, path = cache
  )

  expect_s3_class(result, "sf")
  expect_true(all(!is.na(result$.study)))

  # .time_span is written as a regular column for sf output (unlike
  # SpatRaster output, which uses terra::metags()) -- should reflect the
  # number of days actually used (31 for August).
  expect_true(all(result$.time_span == 31))
})

test_that("link_daily months matches a manually constructed equivalent time_span/date_var", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_months <- link_daily(
    pts, indicator = "2m_temperature",
    months = c(8), baseline = c(1980, 1981), baseline_fun = "mean",
    stat_wrangling = "count_above",
    cache = TRUE, path = cache
  )

  # Manual equivalent: end date = last day of the resolved season
  # (2013-08-31), time_span = 31 (Aug 1 through Aug 31 inclusive)
  pts_manual       <- pts
  pts_manual$date  <- as_date("2013-08-31")

  result_manual <- link_daily(
    pts_manual, indicator = "2m_temperature", time_span = 31,
    baseline = c(1980, 1981), baseline_fun = "mean",
    stat_wrangling = "count_above",
    cache = TRUE, path = cache
  )

  expect_equal(result_months$.result, result_manual$.result)
  expect_equal(result_months$.study,  result_manual$.study)
})

test_that("months spanning a year boundary (winter) resolves correctly (no download needed)", {
  # Pure unit test on .transform_time()'s date arithmetic -- no raster
  # download required, since this only checks year-boundary DETECTION,
  # which the August fixture test above doesn't cover (August never
  # crosses a year boundary). Deliberately isolated from link_daily() to
  # avoid needing a full ~90-day fixture download just to verify date
  # arithmetic that doesn't depend on any actual raster data.
  pts <- test_pts(seq = FALSE)
  pts$date <- as_date(c("2014-06-15", "2014-06-15"))  # outside Dec-Jan-Feb, no shift expected

  result <- gxc:::.transform_time(
    pts, date_var = "date", months = c(12, 1, 2), daily_expansion = TRUE
  )

  seq1 <- as_date(result$time_span_seq[[1]])

  # Dec should resolve to 2013 (year before the resolved window's Jan/Feb),
  # Jan/Feb to 2014 -- i.e. the window spans 2013-12-01 through 2014-02-28
  expect_equal(min(seq1), as_date("2013-12-01"))
  expect_equal(max(seq1), as_date("2014-02-28"))
  expect_equal(length(seq1), 90L)  # 31 + 31 + 28, non-leap year

  # link_date_end should be the LAST day of the window for daily_expansion
  expect_equal(result$link_date_end, as_date("2014-02-28"))
})

test_that("link_daily errors when months and time_span are both specified", {
  skip_on_cran()
  fail_on_request()
  pts <- test_pts(seq = FALSE)

  expect_error(
    link_daily(pts, indicator = "2m_temperature", months = c(3, 4, 5), time_span = 10),
    class = "rlang_error"
  )
})
