skip_if_no_fixtures()

local_key(service = "ecmwfr")

# Builds a daily ERA5 request keyed as .submit_batch() stores it
.stash_request <- function(extent, span, prefix = "observation",
                           indicator = "2m_temperature") {
  request <- gxc:::.build_era5_daily_request(
    indicator = indicator,
    catalogue = "derived-era5-land-daily-statistics",
    extent    = extent,
    years     = format(span, "%Y"),
    months    = format(span, "%m"),
    days      = format(span, "%d"),
    prefix    = prefix
  )
  request$.split <- "month"
  request
}


test_that("stash finds cached observation file", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  request <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 1)

  expect_false(is.null(restored))
  expect_true(file.exists(restored))
})


test_that("stash finds cached baseline files", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  baseline_span <- as.Date(c("1980-08-01", "1981-08-01"))
  request <- .stash_request(.test_extent(pts), baseline_span, prefix = "baseline")

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 2)

  expect_false(is.null(restored))
  expect_length(restored, 2)
  expect_true(all(file.exists(restored)))
})


test_that("stash returns NULL for unknown request", {
  cache <- test_cache()
  local_test_index(cache)

  unknown <- .stash_request(
    extent    = c(52, 13, 51, 14),
    span      = as.Date("2014-08-01"),
    indicator = "total_precipitation"
  )

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(unknown, expected_length = 1)

  expect_null(restored)
})


test_that("stash returns NULL when expected_length does not match", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  request <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 99)

  expect_null(restored)
})


test_that("stash ignores target field in hash", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  r1        <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))
  r2        <- r1
  r2$target <- "2m_temperature_observation_999999_999999_201408"

  stash <- new_stash(file.path(cache, "era5"), service = "ecmwfr")

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

  request <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 1)

  expect_false(is.null(restored))
  expect_true(file.exists(restored))

  # Restore absolute paths for other tests
  local_test_index(cache)
})


test_that("stash does not create duplicate entries", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  request <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))

  stash        <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  index_before <- stash$get()

  restored <- stash$restore(request, expected_length = 1)
  stash$store(restored, request)

  index_after <- stash$get()
  expect_equal(length(index_before), length(index_after))
})


test_that("daily request without month split key is not found", {
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  request        <- .stash_request(.test_extent(pts), as.Date("2014-08-01"))
  request$.split <- NULL

  stash    <- new_stash(file.path(cache, "era5"), service = "ecmwfr")
  restored <- stash$restore(request, expected_length = 1)

  expect_null(restored)
})
