local_key(service = "ecmwfr")

# Fake store metadata with the real ERA5-Land grid, so planning and cell
# selection can be tested without network access
.fake_arco_meta <- function(chunks) {
  list(
    chunks     = chunks,
    dtype      = "<f4",
    compressor = NULL,
    lat        = round(seq(-90, 90, by = 0.1), 1),
    lon        = round(seq(-179.9, 180, by = 0.1), 1),
    time       = -175296 + seq_len(700000) - 1
  )
}

local_fake_arco_meta <- function(indicator = "2m_temperature", .envir = parent.frame()) {
  store <- .arco_stores[[indicator]]
  old   <- .arco_state$meta
  .arco_state$meta[[store$geo]]  <- .fake_arco_meta(c(33792, 4, 8))
  .arco_state$meta[[store$time]] <- .fake_arco_meta(c(1, 1024, 1024))
  withr::defer(.arco_state$meta <- old, envir = .envir)
}

germany <- c(53.8, 6.5, 47.8, 13.9)


test_that("time zone offsets and day hours are computed correctly", {
  expect_equal(.tz_offset_hours("utc+00:00"), 0)
  expect_equal(.tz_offset_hours("utc+02:00"), 2)
  expect_equal(.tz_offset_hours("utc-05:00"), -5)

  expect_equal(.arco_day_hours(as.Date("1970-01-02"), 0), 24:47)
  expect_equal(.arco_day_hours(as.Date("1970-01-02"), 1), 23:46)
  expect_length(.arco_day_hours(as.Date(c("2020-01-01", "2020-01-02")), 0), 48)
})

test_that("daily statistics map to aggregation functions", {
  expect_equal(.arco_daily_fun("daily_mean"), "mean")
  expect_equal(.arco_daily_fun("daily_maximum"), "max")
  expect_equal(.arco_daily_fun("daily_minimum"), "min")
  expect_equal(.arco_daily_fun("daily_sum"), "sum")
})

test_that("uncompressed chunks are decoded by data type", {
  x <- c(1.5, -2.25, NaN)
  expect_equal(.arco_decode(writeBin(x, raw(), size = 4, endian = "little"), "<f4", 3, NULL), x)
  expect_equal(.arco_decode(writeBin(x, raw(), size = 8, endian = "little"), "<f8", 3, NULL), x)

  # Int64 as little-endian (low word, high word) pairs
  i64 <- writeBin(c(-175296L, -1L, 12L, 0L), raw(), size = 4, endian = "little")
  expect_equal(.arco_decode(i64, "<i8", 2, NULL), c(-175296, 12))

  expect_error(.arco_decode(raw(4), "<u2", 2, NULL), "Unsupported")
})

test_that("cell selection includes the extent boundaries", {
  meta  <- .fake_arco_meta(c(1, 1024, 1024))
  cells <- .arco_cells(meta, c(51.1, 13.4, 50.9, 13.6))

  expect_equal(meta$lat[cells$rows + 1], c(50.9, 51.0, 51.1))
  expect_equal(meta$lon[cells$cols + 1], c(13.4, 13.5, 13.6))
})

test_that("chunk counts reflect layout and period", {
  geo  <- .fake_arco_meta(c(33792, 4, 8))
  time <- .fake_arco_meta(c(1, 1024, 1024))
  hrs  <- .arco_day_hours(as.Date("2025-02-01"), 0)

  # Germany: 16 x 10 geo tiles in one time chunk; one time tile per hour
  expect_equal(.arco_n_chunks(geo, germany, hrs), 160)
  expect_equal(.arco_n_chunks(time, germany, hrs), 24)
})

test_that("planning picks time chunks for short and geo chunks for long periods", {
  local_fake_arco_meta()

  one_day <- .arco_plan("2m_temperature", germany, as.Date("2025-02-01"), 0)
  expect_length(one_day, 1)
  expect_equal(one_day[[1]]$layout, "time")

  three_months <- seq(as.Date("2025-01-01"), as.Date("2025-03-31"), by = "day")
  long <- .arco_plan("2m_temperature", germany, three_months, 0)
  expect_length(long, 1)
  expect_equal(long[[1]]$layout, "geo")
  expect_equal(long[[1]]$days, three_months)
})

test_that("days not yet in the store abort with a clear message", {
  local_fake_arco_meta()

  expect_error(
    .arco_read_daily("2m_temperature", germany, as.Date("2035-01-01"),
                     "daily_mean", 0, "geo"),
    "not available"
  )
})

test_that("ARCO matches the CDS daily statistics catalogue", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  arco <- link_daily(pts, indicator = "2m_temperature",
                     catalogue = "arco-era5-land-hourly",
                     cache = TRUE, path = cache)
  cds  <- link_daily(pts, indicator = "2m_temperature",
                     cache = TRUE, path = cache)

  expect_equal(arco$.study, cds$.study, tolerance = 1e-6)
  expect_equal(unique(arco$.unit), "K")
})

test_that("ARCO baseline deviation is computed from cached files", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()

  result <- link_daily(pts, indicator = "2m_temperature",
                       catalogue = "arco-era5-land-hourly",
                       baseline = c(1980, 1981), baseline_fun = "mean",
                       stat_wrangling = "deviation",
                       cache = TRUE, path = cache)

  expect_false(any(is.na(result$.result)))
  expect_equal(result$.result, result$.study - result$.baseline)
})

test_that("ARCO precipitation daily sums are labelled per day", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()

  result <- link_daily(pts, indicator = "total_precipitation",
                       catalogue = "arco-era5-land-hourly",
                       statistic = "daily_sum",
                       cache = TRUE, path = cache)

  expect_true(all(result$.study >= 0))
  expect_equal(unique(result$.unit), "m/day")
})

test_that("every ARCO indicator has a store and a unit", {
  inds <- allowed_indicators_by_catalogue[["arco-era5-land-hourly"]]
  expect_setequal(names(.arco_stores), inds)
  expect_true(all(inds %in% names(.indicator_units)))
})
