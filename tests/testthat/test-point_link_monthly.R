test_that("point_link_monthly returns an sf object with appended focal_value when baseline is FALSE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping point_link_monthly test")
  }

  api_key <- Sys.getenv("WF_API_KEY")
  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  pts <- data.frame(
    lon = c(13.4, 11.6, 9.9),
    lat = c(52.5, 51.3, 50.1),
    date_column = c("08-2014", "08-2014", "08-2014")
  )
  pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

  result <- point_link_monthly(
    indicator = "2m_temperature",
    data = pts_sf,
    date_var = "date_column",
    time_span = 0,
    time_lag = 0,
    buffer = 0,
    baseline = FALSE,
    order = "my",
    path = tempdir(),
    catalogue = "reanalysis-era5-land-monthly-means",
    by_hour = FALSE,
    keep_raw = FALSE,
    parallel = FALSE,
    chunk_size = 50
  )

  expect_s3_class(result, "sf")
  expect_true("focal_value" %in% names(result))
})
