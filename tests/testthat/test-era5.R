local_key(service = "ecmwfr")

# -------------------------------------------------------------------------
# link_daily — sf
# -------------------------------------------------------------------------

test_that("link_daily.sf works with simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <-
    link_daily(
      pts,
      indicator = "2m_temperature",
      cache     = TRUE,
      path      = cache
    )

  expect_s3_class(result, "sf")
  expect_named(
    result,
    c("date", ".study", ".baseline", ".result",
      ".indicator", ".unit", ".resolution", ".time_unit", ".result_unit",
      ".study_fun", ".baseline_fun", ".baseline_years",
      ".time_span", ".time_lag", ".buffer", ".downsample_factor", ".source", "geometry")
  )
  expect_true(all(is.na(result$.baseline)))
  expect_true(all(is.na(result$.result)))
  expect_equal(unique(result$.indicator), "2m_temperature")
  expect_equal(unique(result$.unit), "K")
  expect_equal(unique(result$.time_unit), "days")
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_daily.sf works with deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <-
    link_daily(
      pts,
      indicator      = "2m_temperature",
      baseline       = c(1980, 1981),
      baseline_fun   = "mean",
      stat_wrangling = "deviation",
      cache          = TRUE,
      path           = cache
    )

  expect_s3_class(result, "sf")
  expect_false(any(is.na(result$.baseline)))
  expect_false(any(is.na(result$.result)))
  expect_equal(unique(result$.result_unit), "K")
  expect_equal(unique(result$.baseline_fun), "mean")
  expect_equal(unique(result$.baseline_years), "1980-1981")
  expect_equal(
    round(result$.result, 4), round(result$.study - result$.baseline, 4)
  )
})


test_that("link_daily.sf works with sd_deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <-
    link_daily(
      pts,
      indicator      = "2m_temperature",
      baseline       = c(1980, 1981),
      baseline_fun   = "mean",
      stat_wrangling = "sd_deviation",
      cache          = TRUE,
      path           = cache
    )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.result_unit), "sd")
  expect_false(any(is.na(result$.result)))
})


test_that("link_daily.sf works with count_above and time_span", {
  fail_on_request()
  pts        <- test_pts(seq = FALSE)
  pts$date   <- pts$date + days(1)
  cache      <- test_cache()
  local_test_index(cache)

  result <-
    link_daily(
      pts,
      indicator      = "2m_temperature",
      baseline       = c(1980, 1981),
      baseline_fun   = "mean",
      stat_wrangling = "count_above",
      time_span      = 1,
      cache          = TRUE,
      path           = cache
    )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.result_unit), "days")
  expect_true(all(result$.result >= 0))
})


test_that("link_daily.sf works with sequential dates", {
  fail_on_request()
  pts   <- test_pts(seq = TRUE)
  cache <- test_cache()
  local_test_index(cache)

  result <-
    link_daily(
      pts,
      indicator      = "2m_temperature",
      baseline       = c(1980, 1981),
      baseline_fun   = "mean",
      stat_wrangling = "deviation",
      cache          = TRUE,
      path           = cache
    )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_daily.sf works with time_span", {
  fail_on_request()
  pts        <- test_pts(seq = FALSE)
  pts$date   <- pts$date + days(1)
  cache      <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator      = "2m_temperature",
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


test_that("link_daily.sf works with time_lag", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    time_lag  = 1,
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.time_lag), 1)
})


test_that("link_daily.sf works with buffer", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    buffer    = 5000,
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.buffer), 5000)
})


test_that("link_daily.sf works with prefix", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    pts,
    indicator = "2m_temperature",
    prefix    = "temp",
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_true(".study_temp" %in% names(result))
  expect_true(".indicator_temp" %in% names(result))
  expect_false(".study" %in% names(result))
})


# -------------------------------------------------------------------------
# link_daily — SpatRaster
# -------------------------------------------------------------------------

test_that("link_daily.SpatRaster works with simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    grid,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(".study" %in% names(result))
  expect_equal(get_metag(result, "indicator"), "2m_temperature")
  expect_equal(get_metag(result, "unit"), "K")
  expect_equal(get_metag(result, "time_unit"), "days")
})


test_that("link_daily.SpatRaster works with deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache)

  result <- link_daily(
    grid,
    indicator      = "2m_temperature",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(all(c(".study", ".baseline", ".result") %in% names(result)))
  expect_equal(get_metag(result, "result_unit"), "K")
})


# -------------------------------------------------------------------------
# link_monthly — sf
# -------------------------------------------------------------------------

test_that("link_monthly.sf works with simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    pts,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_named(
    result,
    c("date", ".study", ".baseline", ".result",
      ".indicator", ".unit", ".resolution", ".time_unit", ".result_unit",
      ".study_fun", ".baseline_fun", ".baseline_years",
      ".time_span", ".time_lag", ".buffer", ".downsample_factor", ".source", "geometry")
  )
  expect_true(all(is.na(result$.baseline)))
  expect_equal(unique(result$.time_unit), "months")
  expect_false(result$.study[1] == result$.study[2])
})


test_that("link_monthly.sf works with deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    pts,
    indicator      = "2m_temperature",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s3_class(result, "sf")
  expect_false(any(is.na(result$.result)))
  expect_equal(unique(result$.result_unit), "K")
  expect_equal(round(result$.result, 4), round(result$.study - result$.baseline, 4))
})


test_that("link_monthly.sf works with explicit months", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    pts,
    indicator      = "2m_temperature",
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


test_that("link_monthly.sf works with buffer", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    pts,
    indicator = "2m_temperature",
    buffer    = 5000,
    cache     = TRUE,
    path      = cache
  )

  expect_s3_class(result, "sf")
  expect_equal(unique(result$.buffer), 5000)
})


# -------------------------------------------------------------------------
# link_monthly — SpatRaster
# -------------------------------------------------------------------------

test_that("link_monthly.SpatRaster works with simple extraction", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    grid,
    indicator = "2m_temperature",
    cache     = TRUE,
    path      = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(".study" %in% names(result))
  expect_equal(get_metag(result, "time_unit"), "months")
})


test_that("link_monthly.SpatRaster works with deviation baseline", {
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  grid  <- terra::rast(pts)
  terra::time(grid) <- as_date(pts$date[1])
  terra::crs(grid)  <- sf::st_crs(pts)$wkt
  cache <- test_cache()
  local_test_index(cache)

  result <- link_monthly(
    grid,
    indicator      = "2m_temperature",
    baseline       = c(1980, 1981),
    baseline_fun   = "mean",
    stat_wrangling = "deviation",
    cache          = TRUE,
    path           = cache
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(all(c(".study", ".baseline", ".result") %in% names(result)))
})


# -------------------------------------------------------------------------
# Input validation
# -------------------------------------------------------------------------

test_that("link_daily errors on invalid catalogue", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", catalogue = "invalid"),
    "Invalid"
  )
})

test_that("link_daily errors on invalid indicator", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "invalid"),
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

test_that("link_monthly errors on months and time_span combined", {
  pts <- test_pts()
  expect_error(
    link_monthly(pts, indicator = "2m_temperature",
                 months = c(3, 4, 5), time_span = 3),
    "cannot both be specified"
  )
})

test_that("link_daily errors on invalid baseline", {
  pts <- test_pts()
  expect_error(
    link_daily(pts, indicator = "2m_temperature", baseline = c(1980)),
    "Invalid"
  )
})
