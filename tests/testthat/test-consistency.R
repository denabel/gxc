local_key(service = "ecmwfr")

test_that("rbind works on two link_daily results with different specs", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_simple <- link_daily(
    pts,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )
  result_baseline <- link_daily(
    pts,
    indicator      = "2m_temperature",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  combined <- rbind(result_simple, result_baseline)
  expect_s3_class(combined, "sf")
  expect_equal(nrow(combined), 4)
  expect_equal(names(result_simple), names(result_baseline))
})


test_that("two prefix calls do not overwrite each other", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- pts |>
    link_daily(indicator = "2m_temperature",
               prefix = "temp_simple",
               cache = TRUE, path = cache) |>
    link_daily(indicator = "2m_temperature",
               baseline = c(1980, 1981), baseline_fun = "mean",
               stat_wrangling = "deviation",
               prefix = "temp_baseline",
               cache = TRUE, path = cache)

  expect_true(".study_temp_simple"    %in% names(result))
  expect_true(".study_temp_baseline"  %in% names(result))
  expect_true(".result_temp_baseline" %in% names(result))
  expect_false(".study" %in% names(result))
})


test_that("link_daily and link_monthly produce consistent column structure", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  daily <- link_daily(
    pts,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )
  monthly <- link_monthly(
    pts,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )

  # All columns except .time_unit and .months should match
  daily_cols   <- setdiff(names(daily),   c(".time_unit", "geometry"))
  monthly_cols <- setdiff(names(monthly), c(".time_unit", ".months", "geometry"))
  expect_equal(daily_cols, monthly_cols)
})


test_that("result with baseline is always study minus baseline for deviation", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator      = "2m_temperature",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_equal(
    round(result$.result, 6),
    round(result$.study - result$.baseline, 6)
  )
})


test_that("prefix produces same values as no prefix", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_no_prefix <- link_daily(
    pts,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )
  result_prefix <- link_daily(
    pts,
    indicator = "2m_temperature",
    prefix    = "temp",
    cache     = TRUE,
    path      = cache
  )

  expect_equal(result_no_prefix$.study, result_prefix$.study_temp)
})
