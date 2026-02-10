new_stash <- function(cache = NULL, service = "ecmwfr") {
  cache <- cache %||% .default_download_dir(cache = TRUE, service)
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)

  .get <- function() {
    index_path <- file.path(cache, "index.rds")

    if (file.exists(index_path)) {
      readRDS(index_path)
    } else {
      list()
    }
  }

  .destroy <- function() {
    unlink(cache, recursive = TRUE, force = TRUE)
  }

  .pop <- function(n = 1) {
    index <- .get()
    len <- length(index)
    to_pop <- index[seq(len - (n - 1), len)]
    for (file in to_pop) unlink(file)
    index <- index[!names(index) %in% names(to_pop)]
    .write(index)
  }

  .clean <- function() {
    index <- .get()
    corrupt <- !vapply(index, file.exists, FUN.VALUE = logical(1))
    index <- index[!corrupt]
    .write(index)
  }

  .restore <- function(request) {
    request$target <- NULL
    request$service <- service
    hash <- rlang::hash(request)
    index <- .get()
    cached_path <- index[[hash]]

    if (!is.null(cached_path) && !file.exists(cached_path)) {
      cli::cli_warn(c(
        "!" = "A matching file has been found in the cache but it is corrupt.",
        "i" = "Will clean the cache and redownload instead."
      ))

      index[[hash]] <- NULL
      return(NULL)
    }

    cached_path
  }

  .store <- function(path, request) {
    request$target <- NULL
    request$service <- service
    hash <- rlang::hash(request)
    entry <- list(normalizePath(path))
    names(entry) <- hash
    index <- .get()
    index <- c(index, entry)
    .write(index)
  }

  .write <- function(index) {
    index_path <- file.path(cache, "index.rds")
    saveRDS(index, index_path)
  }

  structure(
    class = "gxc_stash",
    list(
      path = cache,
      get = .get,
      write = .write,
      destroy = .destroy,
      pop = .pop,
      clean = .clean,
      store = .store,
      restore = .restore
    )
  )
}
