test_that("poly_link_monthly returns an sf object with appended focal_value when baseline is FALSE", {
  skip_on_cran()

  if (Sys.getenv("WF_API_KEY") == "") {
    skip("API key not set, skipping poly_link_monthly test")
  }

  api_key <- Sys.getenv("WF_API_KEY")
  keyring::key_set_with_value(service = "wf_api_key", password = api_key)

  library(sf)

  polygon <- st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  )))
  poly_sf <- st_sf(geometry = st_sfc(polygon, crs = 4326))

  poly_sf$date_column <- "08-2014"

  result <- poly_link_monthly(
    indicator = "2m_temperature",
    data = poly_sf,
    date_var = "date_column",
    time_span = 0,
    time_lag = 0,
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
