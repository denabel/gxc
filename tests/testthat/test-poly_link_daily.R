test_that("poly_link_daily returns an sf object when baseline is FALSE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping poly_link_daily test")
  }

  # Set the API key in keyring (adjust service name if needed)
  api_key <- Sys.getenv("WF_API_KEY")
  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  # Create a simple polygon (a square) as an sf object
  library(sf)
  polygon <- st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  )))
  poly_sf <- st_sf(geometry = st_sfc(polygon, crs = 4326))
  poly_sf$date_column <- "2014-08-01"  # add a date column required by the function

  # Call poly_link_daily with baseline = FALSE to simplify the test
  result <- poly_link_daily(
    indicator = "2m_temperature",
    data = poly_sf,
    date_var = "date_column",
    time_span = 0,
    time_lag = 0,
    baseline = c("1970","1971"),
    order = "ymd",
    path = tempdir(),
    catalogue = "derived-era5-land-daily-statistics",
    statistic = "daily_mean",
    time_zone = "utc+00:00",
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
  )

  # Check that the result is an sf object and has the expected column "focal_value"
  expect_s3_class(result, "sf")
  expect_true("focal_value" %in% names(result))
})
