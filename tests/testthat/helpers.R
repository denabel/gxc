test_geom <- function() {
  sf::st_sfc(sf::st_point(c(13, 50)), sf::st_point(c(14, 51)))
}


test_era5 <- function(seq = FALSE) {
  sf::st_sf(
    date = if (seq)
      seq(as.Date("2014-08-01"), as.Date("2014-08-02"), by = "1 day")
    else
      c("2014-08-01", "2014-08-01"),
    geometry = test_geom(),
    crs = 4326
  )
}


test_cache <- function(service = "ecmwfr") {
  test_path(file.path("fixtures", service))
}


fail_on_request <- function() {
  old <- do.call(options, as.list(c("__gxc_fail_on_request__" = TRUE)))
  do.call(options, list("__gxc_fail_on_forbidden__" = TRUE), envir = parent.frame())
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = parent.frame())
}


go_on_request <- function() {
  options("__gxc_fail_on_request__" = NULL)
}
