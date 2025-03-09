test_that("point_link_daily returns an sf object when baseline is TRUE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping point_link_daily test")
  }

  api_key <- Sys.getenv("WF_API_KEY")

  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  pts <- data.frame(
    lon = c(13.4, 11.6, 9.9),
    lat = c(52.5, 51.3, 50.1),
    date_column = c("2014-08-01", "2014-08-01", "2014-08-01")
  )
  pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

  result <- point_link_daily(
    indicator = "2m_temperature",
    data = pts_sf,
    date_var = "date_column",
    time_span = 0,
    time_lag = 0,
    buffer = 0,
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
