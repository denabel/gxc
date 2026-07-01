local_key(service = "ecmwfr")

test_that("stash finds cached observation file", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)
  span     <- as.Date("2014-08-01")

  request <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = "observation"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 1)

  expect_false(is.null(restored))
  expect_true(file.exists(restored))
})


test_that("stash finds cached baseline files", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)

  baseline_span <- as.Date(c("1980-08-01", "1981-08-01"))

  request <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(baseline_span, "%Y"),
    months    = format(baseline_span, "%m"),
    days      = format(baseline_span, "%d"),
    prefix    = "baseline"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 2)

  expect_false(is.null(restored))
  expect_length(restored, 2)
  expect_true(all(file.exists(restored)))
})


test_that("stash returns NULL for unknown request", {
  cache <- test_cache()
  local_test_index(cache)

  unknown <- gxc:::.build_era5_daily_request(
    indicator = "total_precipitation",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = c(52, 13, 51, 14),
    years     = "2014", months = "08", days = "01",
    prefix    = "observation"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(unknown, expected_length = 1)

  expect_null(restored)
})


test_that("stash returns NULL when expected_length does not match", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)
  span     <- as.Date("2014-08-01")

  request <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = "observation"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 99)

  expect_null(restored)
})


test_that("stash ignores target field in hash", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)
  span     <- as.Date("2014-08-01")

  # Build request with different timestamp in target
  r1 <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = "observation"
  )
  r2        <- r1
  r2$target <- paste0("2m_temperature_observation_999999_999999_20140801")

  stash <- new_stash(file.path(cache, "era5"), service = "ecmwfr")

  # Both requests should produce the same hash and find the same file
  restored1 <- stash$restore(r1, expected_length = 1)
  restored2 <- stash$restore(r2, expected_length = 1)

  expect_false(is.null(restored1))
  expect_false(is.null(restored2))
  expect_equal(restored1, restored2)
})


test_that("stash resolves relative paths against cache directory", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()

  # reset_test_index stores only basenames
  reset_test_index(file.path(cache, "era5"), service = "ecmwfr")

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)
  span     <- as.Date("2014-08-01")

  request <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = "observation"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 1)

  # Should resolve filename against cache directory
  expect_false(is.null(restored))
  expect_true(file.exists(restored))

  # Restore absolute paths for other tests
  local_test_index(cache)
})


test_that("stash does not create duplicate entries", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  prepared <- sf::st_transform(pts, 4326)
  prepared <- sf::st_buffer(prepared, 0)
  extent   <- gxc:::.get_extent(prepared)
  span     <- as.Date("2014-08-01")

  request <- gxc:::.build_era5_daily_request(
    indicator = "2m_temperature",
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = "observation"
  )

  stash      <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  index_before <- stash$get()

  # Store same request again
  restored <- stash$restore(request, expected_length = 1)
  stash$store(restored, request)

  index_after <- stash$get()
  expect_equal(length(index_before), length(index_after))
})
