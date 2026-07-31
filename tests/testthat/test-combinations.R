test_that("link_daily with baseline_fun list matches independent single calls", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_combo <- link_daily(
    pts, indicator = "2m_temperature",
    baseline       = c(1980, 1981), baseline_fun = list("mean", "median"),
    stat_wrangling = list("deviation", "deviation"),
    cache = TRUE, path = cache
  )

  expect_type(result_combo, "list")
  expect_named(result_combo, c("mean_deviation", "median_deviation"))

  result_mean <- link_daily(
    pts, indicator = "2m_temperature",
    baseline = c(1980, 1981), baseline_fun = "mean",
    stat_wrangling = "deviation",
    cache = TRUE, path = cache
  )

  result_median <- link_daily(
    pts, indicator = "2m_temperature",
    baseline = c(1980, 1981), baseline_fun = "median",
    stat_wrangling = "deviation",
    cache = TRUE, path = cache
  )

  expect_equal(result_combo$mean_deviation$.result,   result_mean$.result)
  expect_equal(result_combo$median_deviation$.result, result_median$.result)
})

test_that("link_daily with baseline_fun list of length 1 recycled against longer stat_wrangling", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  pts_lag       <- pts
  pts_lag$date  <- pts_lag$date + days(1)

  result_combo <- link_daily(
    pts_lag, indicator = "2m_temperature", time_span = 1,
    baseline       = c(1980, 1981), baseline_fun = "mean",
    stat_wrangling = list("deviation", "count_above"),
    cache = TRUE, path = cache
  )

  expect_named(result_combo, c("mean_deviation", "mean_count_above"))

  result_deviation <- link_daily(
    pts_lag, indicator = "2m_temperature", time_span = 1,
    baseline = c(1980, 1981), baseline_fun = "mean",
    stat_wrangling = "deviation",
    cache = TRUE, path = cache
  )

  expect_equal(result_combo$mean_deviation$.result, result_deviation$.result)
})

test_that("link_daily errors on mismatched baseline_fun/stat_wrangling list lengths", {
  skip_on_cran()
  fail_on_request()
  pts <- test_pts(seq = FALSE)

  expect_error(
    link_daily(
      pts, indicator = "2m_temperature",
      baseline       = c(1980, 1981),
      baseline_fun   = list("mean", "median", "p90"),
      stat_wrangling = list("deviation", "count_above")
    ),
    "same length"
  )
})
