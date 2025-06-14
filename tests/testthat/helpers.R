test_geom <- function() {
  sf::st_sfc(sf::st_point(c(13, 50)), sf::st_point(c(14, 51)))
}


test_era5 <- function(seq = FALSE) {
  sf::st_sf(
    date = if (seq)
      seq(as_date("2014-08-01"), as_date("2014-08-02"), by = "1 day")
    else
      as_date(c("2014-08-01", "2014-08-01")),
    geometry = test_geom(),
    crs = 4326
  )
}


test_cache <- function(service = "ecmwfr") {
  test_path(file.path("fixtures", service))
}


local_test_index <- function(cache, .envir = parent.frame()) {
  stash <- new_stash(cache)
  old <- stash$get()
  new <- lapply(old, function(x) {
    if (!startsWith(x, "tests/testthat")) {
      x <- test_path(x)
    }
    x
  })
  stash$write(new)
  do.call(
    on.exit,
    list(bquote({
      stash <- .(stash)
      stash$write(.(old))
    })),
    envir = .envir
  )
}


reset_test_index <- function(cache) {
  stash <- new_stash(cache)
  old <- stash$get()
  new <- lapply(old, function(x) {
    gsub("tests/testthat/", "", x, fixed = TRUE)
  })
  stash$write(new)
}


fail_on_request <- function() {
  old <- do.call(options, as.list(c("__gxc_fail_on_request__" = TRUE)))
  do.call(options, list("__gxc_fail_on_forbidden__" = TRUE), envir = parent.frame())
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = parent.frame())
}


go_on_request <- function() {
  options("__gxc_fail_on_request__" = NULL)
}


temp_file <- function(...) {
  file <- normalizePath(tempfile(...))
  file.create(file)
  file
}


local_key <- function(service = "ecmwfr", user = NULL, key = "test", .envir = parent.frame()) {
  user <- user %||% service
  keys <- keyring::key_list(service = service)
  if (user %in% keys$username) {
    return(NULL)
  }
  keyring::key_set_with_value(service, user = user, password = key)
  do.call(
    on.exit,
    list(bquote(
      keyring::key_delete(
        service = .(service),
        user = .(user)
      )
    )),
    envir = .envir
  )
}
