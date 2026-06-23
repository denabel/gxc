new_stash <- function(cache = NULL, service = "ecmwfr") {
  cache <- cache %||% .default_download_dir(cache = TRUE, service)
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)

  .get <- function() {
    index_path <- file.path(cache, "index.rds")
    if (file.exists(index_path)) readRDS(index_path) else list()
  }

  .write <- function(index) {
    saveRDS(index, file.path(cache, "index.rds"))
  }

  .destroy <- function() {
    unlink(cache, recursive = TRUE, force = TRUE)
  }

  .pop <- function(n = 1) {
    index  <- .get()
    len    <- length(index)
    to_pop <- index[seq(len - (n - 1), len)]
    for (file in to_pop) unlink(file)
    index  <- index[!names(index) %in% names(to_pop)]
    .write(index)
  }

  .clean <- function() {
    index   <- .get()
    corrupt <- !vapply(index, function(paths) all(file.exists(paths)), FUN.VALUE = logical(1))
    index   <- index[!corrupt]
    .write(index)
  }

  # Normalize params for hashing — remove fields that should not affect the hash
  .make_hash <- function(params) {
    params$target  <- NULL
    params$.prefix <- NULL
    params$service <- service
    rlang::hash(params)
  }

  # Resolve a path against the cache directory if it is not absolute
  .resolve_path <- function(p) {
    if (file.exists(p)) return(p)
    p_from_cache <- file.path(cache, p)
    if (file.exists(p_from_cache)) p_from_cache else p
  }

  .restore <- function(params, expected_length = NULL) {
    hash        <- .make_hash(params)
    index       <- .get()
    cached_path <- index[[hash]]

    if (is.null(cached_path)) return(NULL)

    # Resolve relative paths against cache directory
    cached_path <- sapply(cached_path, .resolve_path, USE.NAMES = FALSE)

    if (!is.null(expected_length) && length(cached_path) != expected_length) {
      cli::cli_warn(c(
        "!" = "Cache does not comprise all requested files.",
        "i" = "Will clean the cache and redownload instead just to be sure."
      ))
      index[[hash]] <- NULL
      .write(index)
      return(NULL)
    }

    if (!all(file.exists(cached_path))) {
      cli::cli_warn(c(
        "!" = "A matching file has been found in the cache but it is corrupt.",
        "i" = "Will clean the cache and redownload instead."
      ))
      index[[hash]] <- NULL
      .write(index)
      return(NULL)
    }

    cached_path
  }

  .store <- function(data_path, params) {
    hash  <- .make_hash(params)
    index <- .get()

    # Don't overwrite existing cache entry
    if (!is.null(index[[hash]])) return(invisible(NULL))

    entry        <- list(normalizePath(as.character(data_path)))
    names(entry) <- hash
    index        <- c(index, entry)
    .write(index)
  }

  structure(
    class = "gxc_stash",
    list(
      path    = cache,
      get     = .get,
      write   = .write,
      destroy = .destroy,
      pop     = .pop,
      clean   = .clean,
      store   = .store,
      restore = .restore
    )
  )
}
