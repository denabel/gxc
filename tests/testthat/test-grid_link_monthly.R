test_that("grid_link_monthly returns a SpatRaster with appended focal_value when baseline is FALSE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping grid_link_monthly test")
  }

  api_key <- Sys.getenv("WF_API_KEY")
  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  library(terra)

  sample_grid <- rast(ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")

  terra::time(sample_grid) <- seq(as.Date("2014-01-01"), by = "1 month", length.out = nlyr(sample_grid))

  result <- grid_link_monthly(
    indicator = "2m_temperature",
    data = sample_grid,
    time_span = 0,
    time_lag = 0,
    baseline = FALSE,
    path = tempdir(),
    catalogue = "reanalysis-era5-land-monthly-means",
    by_hour = FALSE,
    method = "bilinear",
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
  )

  expect_s4_class(result, "SpatRaster")
  expect_true("focal_value" %in% names(result))
})
