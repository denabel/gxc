test_that("extent is snapped to the grid of the catalogue", {
  pts <- sf::st_as_sf(
    data.frame(lon = c(-9.14, 6.96), lat = c(38.72, 50.94)),
    coords = c("lon", "lat"), crs = 4326
  )

  expect_equal(.catalogue_grid("derived-era5-single-levels-daily-statistics"), 0.25)
  expect_equal(.catalogue_grid("reanalysis-era5-single-levels-monthly-means"), 0.25)
  expect_equal(.catalogue_grid("derived-era5-land-daily-statistics"), 0.1)

  # c(N, W, S, E); each bound lies on the 0.25 grid and outside the points
  ext <- .get_extent(pts, .catalogue_grid("derived-era5-single-levels-daily-statistics"))
  expect_equal(unname(ext), c(51, -9.25, 38.5, 7))
})

test_that("grid cells of edge observations are inside the extent", {
  pts <- sf::st_as_sf(
    data.frame(lon = c(-9.14, 6.96), lat = c(38.72, 50.94)),
    coords = c("lon", "lat"), crs = 4326
  )
  res <- 0.25
  ext <- unname(.get_extent(pts, res))

  # The grid point nearest to each observation must lie within the extent
  nearest_lon <- round(c(-9.14, 6.96) / res) * res
  nearest_lat <- round(c(38.72, 50.94) / res) * res
  expect_true(all(nearest_lon >= ext[2] & nearest_lon <= ext[4]))
  expect_true(all(nearest_lat >= ext[3] & nearest_lat <= ext[1]))
})
