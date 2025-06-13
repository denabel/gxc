stash <- new_stash(file.path(tempdir(), "test_stash"))
on.exit(stash$destroy())
request <- list(a = "test")
file <- temp_file()

test_that("stash can store files", {
  hash <- rlang::hash(c(request, list(service = "ecmwfr")))
  stash$store(file, request)

  index <- stash$get()
  expect_named(index, hash)
  expect_equal(index[[1]], file)
})


test_that("stash can restore files", {
  restored <- stash$restore(request)
  expect_equal(restored, file)
})


test_that("stash can pop exactly one file", {
  stash$store(temp_file(), list(b = "test"))
  stash$store(temp_file(), list(c = "test"))

  expect_length(stash$get(), 3)
  stash$pop(1)
  expect_length(stash$get(), 2)
})


test_that("stash can clean old files", {
  unlink(stash$get()[[1]])
  expect_length(stash$get(), 2)
  stash$clean()
  expect_length(stash$get(), 1)
})
