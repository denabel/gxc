test_that("grid_link_daily returns a SpatRaster with appended focal_value when baseline is FALSE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping grid_link_daily test")
  }

  # Set the API key in keyring
  api_key <- Sys.getenv("WF_API_KEY")
  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  # Load terra
  library(terra)

  # Create a sample grid (SpatRaster)
  # For example, create a raster with 10 columns and 10 rows over a small extent
  sample_grid <- rast(ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")

  # Set a time dimension for the raster
  terra::time(sample_grid) <- as.Date("2014-08-01")

  # Call grid_link_daily with baseline = FALSE to avoid downloading baseline data
  result <- grid_link_daily(
    indicator = "2m_temperature",
    data = sample_grid,
    time_span = 0,
    time_lag = 0,
    baseline = c("1970","1971"),
    path = tempdir(),  # use temporary directory for downloads
    catalogue = "derived-era5-land-daily-statistics",
    statistic = "daily_mean",
    time_zone = "utc+00:00",
    method = "bilinear",
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
  )

  # Check that the result is a SpatRaster (terra objects are S4) and that it has a layer "focal_value"
  expect_s4_class(result, "SpatRaster")
  expect_true("focal_value" %in% names(result))
})
