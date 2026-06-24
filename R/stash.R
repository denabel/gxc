new_stash <- function(cache = NULL, service = "ecmwfr") {
  cache <- normalizePath(
    cache %||% .default_download_dir(cache = TRUE, service),
    mustWork = FALSE
  )
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
    corrupt <- !vapply(index, function(paths) all(file.exists(paths)),
                       FUN.VALUE = logical(1))
    index   <- index[!corrupt]
    .write(index)
  }

  # Normalize params for hashing — remove fields that should not affect
  # the hash so that requests with different timestamps or prefixes still
  # map to the same cache entry.
  .make_hash <- function(params) {
    params$target  <- NULL
    params$.prefix <- NULL
    params$service <- service
    rlang::hash(params)
  }

  # Resolve a path against the cache directory if it is not absolute or
  # if the absolute path no longer exists.
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

    # If cache is incomplete, keep the partial entry for resume and
    # return NULL silently — .submit_batch handles messaging
    if (!is.null(expected_length) && length(cached_path) != expected_length) {
      return(NULL)
    }

    if (!all(file.exists(cached_path))) {
      # Keep existing files in the index so the partial cache can be resumed
      existing  <- cached_path[file.exists(cached_path)]
      n_missing <- length(cached_path) - length(existing)
      cli::cli_warn(c(
        "!" = "{n_missing} cached file{?s} {?is/are} missing from disk.",
        i = "Will resume download for the {n_missing} missing file{?s}."
      ))
      index[[hash]] <- if (length(existing) > 0) existing else NULL
      .write(index)
      return(NULL)
    }

    cached_path
  }

  .store <- function(data_path, params) {
    hash  <- .make_hash(params)
    index <- .get()

    # Don't overwrite an existing complete cache entry
    if (!is.null(index[[hash]])) return(invisible(NULL))

    entry        <- list(normalizePath(as.character(data_path)))
    names(entry) <- hash
    index        <- c(index, entry)
    .write(index)
  }

  # Accumulates downloaded files for a request progressively — used when
  # files are downloaded one by one so that partial results are not lost
  # if the download is interrupted. Existing paths for the same hash are
  # preserved and the new paths are appended.
  .store_partial <- function(data_path, params) {
    hash     <- .make_hash(params)
    index    <- .get()
    existing <- index[[hash]]
    combined <- unique(c(
      existing,
      normalizePath(as.character(data_path))
    ))
    index[[hash]] <- combined
    .write(index)
  }

  structure(
    class = "gxc_stash",
    list(
      path          = cache,
      get           = .get,
      write         = .write,
      destroy       = .destroy,
      pop           = .pop,
      clean         = .clean,
      store         = .store,
      store_partial = .store_partial,
      restore       = .restore,
      make_hash     = .make_hash
    )
  )
}
