test_that("months resolve to the most recent complete season before the observation", {
  summer <- c(6, 7, 8)
  expect_equal(.resolve_months(as.Date("2024-02-10"), summer),
               as.Date(c("2023-06-01", "2023-07-01", "2023-08-01")))
  expect_equal(.resolve_months(as.Date("2014-08-01"), summer),
               as.Date(c("2013-06-01", "2013-07-01", "2013-08-01")))
  expect_equal(.resolve_months(as.Date("2014-08-31"), summer),
               as.Date(c("2013-06-01", "2013-07-01", "2013-08-01")))
  expect_equal(.resolve_months(as.Date("2014-09-01"), summer),
               as.Date(c("2014-06-01", "2014-07-01", "2014-08-01")))
})

test_that("seasons across the turn of the year resolve correctly", {
  winter <- c(12, 1, 2)
  expect_equal(.resolve_months(as.Date("2023-03-10"), winter),
               as.Date(c("2022-12-01", "2023-01-01", "2023-02-01")))
  expect_equal(.resolve_months(as.Date("2023-02-10"), winter),
               as.Date(c("2021-12-01", "2022-01-01", "2022-02-01")))
  expect_equal(.resolve_months(as.Date("2023-12-15"), winter),
               as.Date(c("2022-12-01", "2023-01-01", "2023-02-01")))
})

test_that("count_above and count_below count study days for numeric input", {
  baseline <- 1:10                      # mean 5.5
  focal    <- c(5, 6, 7)

  above <- .compute_stat_wrangling(baseline, focal, "count_above")
  below <- .compute_stat_wrangling(baseline, focal, "count_below")

  expect_equal(above$reference_stat, 5.5)
  expect_equal(above$result, 2)
  expect_equal(below$result, 1)
})

test_that("count_above and count_below count study days for SpatRaster input", {
  make <- function(values) {
    r <- terra::rast(nrows = 2, ncols = 2, nlyrs = length(values))
    for (i in seq_along(values)) terra::values(r[[i]]) <- values[i]
    r
  }
  baseline   <- make(1:10)              # mean 5.5 in every cell
  focal_days <- make(c(5, 6, 7))
  study      <- terra::app(focal_days, mean)

  above <- .compute_stat_wrangling(baseline, study, "count_above",
                                   focal_days = focal_days)
  below <- .compute_stat_wrangling(baseline, study, "count_below",
                                   focal_days = focal_days)

  expect_true(all(terra::values(above$reference_stat) == 5.5))
  expect_true(all(terra::values(above$result) == 2))
  expect_true(all(terra::values(below$result) == 1))
})
