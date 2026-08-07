test_that("link_daily with downsample_factor produces a small, non-zero difference vs full resolution (buffer)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_full <- link_daily(
    pts, indicator = "2m_temperature",
    buffer = 5000,
    baseline = c(1980, 1981), baseline_fun = "mean", stat_wrangling = "deviation",
    cache = TRUE, path = cache
  )

  result_downsampled <- link_daily(
    pts, indicator = "2m_temperature",
    buffer = 5000,
    baseline = c(1980, 1981), baseline_fun = "mean", stat_wrangling = "deviation",
    downsample_factor = 5, downsample_min_buffer = 0,
    cache = TRUE, path = cache
  )

  diff <- abs(result_full$.result - result_downsampled$.result)

  # Should differ only slightly (own measurement: <0.3% relative for
  # 5-100km buffers), not be identical (that would suggest downsampling
  # silently isn't applied) and not wildly different (that would suggest
  # a layer-matching bug).
  expect_true(all(diff < abs(result_full$.result) * 0.05 + 1e-6))
})

test_that("link_daily downsampling is NOT applied to point extraction (buffer = 0)", {
  skip_on_cran()
  fail_on_request()
  pts   <- test_pts(seq = FALSE)
  cache <- test_cache()
  local_test_index(cache)

  result_full <- link_daily(
    pts, indicator = "2m_temperature",
    baseline = c(1980, 1981), baseline_fun = "mean", stat_wrangling = "deviation",
    cache = TRUE, path = cache
  )

  result_downsampled <- link_daily(
    pts, indicator = "2m_temperature",
    baseline = c(1980, 1981), baseline_fun = "mean", stat_wrangling = "deviation",
    downsample_factor = 5, downsample_min_buffer = 0,
    cache = TRUE, path = cache
  )

  # buffer = 0 -> POINT geometry -> .extract_values() always uses full
  # resolution regardless of downsample_min_buffer -- results must be
  # IDENTICAL, not just close.
  expect_equal(result_full$.result, result_downsampled$.result)
})

test_that(".sub_layers() correctly re-subsets the coarse attribute", {
  skip_on_cran()
  fail_on_request()
  cache <- test_cache()

  baseline_files <- list.files(
    file.path(cache, "era5"), pattern = "baseline.*\\.nc$", full.names = TRUE
  )
  skip_if(length(baseline_files) < 3, "Not enough baseline fixture files for this test")

  raster_fine   <- gxc:::.safe_rast(baseline_files)
  raster_coarse <- terra::aggregate(raster_fine, fact = 2, fun = "mean", na.rm = TRUE)
  attr(raster_fine, "coarse") <- raster_coarse

  idx <- seq_len(min(2, terra::nlyr(raster_fine)))
  sub <- gxc:::.sub_layers(raster_fine, idx)

  expect_equal(terra::nlyr(attr(sub, "coarse")), length(idx))

  direct_coarse <- terra::aggregate(raster_fine[[idx]], fact = 2, fun = "mean", na.rm = TRUE)
  expect_equal(
    terra::values(attr(sub, "coarse")),
    terra::values(direct_coarse)
  )
})
