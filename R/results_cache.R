# results_cache.R
#
# Opt-in, disk-backed cache for per-date link_daily()/link_monthly()
# results. Deliberately separate from gxc's existing raw-file caching
# (.safe_rast()'s .raster_memo_cache/.raster_file_cache, and the
# download-level caching via `cache`/`path`) -- this caches the FINAL,
# already-extracted-and-aggregated result fragments, one per (spec,
# effective date), so a crash mid-run only loses whatever hasn't been
# cached yet, and a re-run with the same parameters skips computation
# entirely for already-cached dates.
#
# NOT enabled by default (cache_results = FALSE): cached fragments
# necessarily contain the exact geometry of the input observations
# (needed for the key and for the integrity self-check below), which for
# survey data can indirectly reveal respondent locations. Enabling this
# requires an explicit `results_cache_path` -- there is no default
# directory, forcing a deliberate choice of where such data is stored.

# .results_cache_key ----
# Builds a hash key from every parameter that affects the output for one
# (spec, effective date) combination, INCLUDING the exact geometry (as
# WKT) of the observations in this split. Including geometry in the key
# means two different sets of observations can never collide on the same
# cache entry, even if every other parameter is identical.
.results_cache_key <- function(splitted,
                               link_type,
                               indicator,
                               catalogue,
                               time_span,
                               months,
                               time_lag,
                               buffer,
                               baseline,
                               baseline_fun_names,
                               stat_wrangling_list,
                               study_fun_name,
                               downsample_factor,
                               downsample_min_buffer,
                               prefix,
                               this_date) {
  rlang::hash(list(
    link_type, indicator, catalogue, time_span, months, time_lag, buffer,
    baseline, baseline_fun_names, stat_wrangling_list, study_fun_name,
    downsample_factor, downsample_min_buffer, prefix,
    format(this_date),
    sf::st_as_text(sf::st_geometry(splitted))
  ))
}

# .read_results_cache ----
# Returns the cached combo_list for this key if present, or NULL on a
# cache miss. Performs an integrity self-check: the geometry stored
# ALONGSIDE the cached result (not just hashed into the key) must match
# the geometry currently being requested. This guards against a bug in
# how the key itself is constructed (rather than relying solely on the
# hash's own uniqueness) silently serving a wrong result for the wrong
# observations -- cheap to check, and this session has repeatedly found
# exactly this class of bug (row-order/misattribution errors) elsewhere.
.read_results_cache <- function(results_cache_path, key, splitted) {
  cache_file <- file.path(results_cache_path, paste0(key, ".rds"))
  if (!file.exists(cache_file)) return(NULL)

  cached <- readRDS(cache_file)

  stored_geom  <- sf::st_as_text(cached$geometry)
  current_geom <- sf::st_as_text(sf::st_geometry(splitted))

  if (!identical(stored_geom, current_geom)) {
    cli::cli_abort(c(
      "Results cache integrity error: stored geometry does not match the ",
      "requested geometry.",
      "i" = "Cache file: {.path {cache_file}}",
      "i" = "This should not happen if the cache key is built correctly --",
      "please report this as a bug rather than deleting the cache silently."
    ))
  }

  cached$result
}

# .write_results_cache ----
# Writes `payload` (link_daily()'s per-date combo_list, or link_monthly()'s
# per-date single sf object -- generic either way) to the cache, together
# with the exact geometry used, for the integrity check in
# .read_results_cache() above. Atomic (temp file + rename): a process
# killed mid-write leaves at most a stray .tmp file, never a
# truncated-but-file.exists()-passing cache entry.
.write_results_cache <- function(results_cache_path, key, splitted, payload) {
  dir.create(results_cache_path, showWarnings = FALSE, recursive = TRUE)

  cache_file <- file.path(results_cache_path, paste0(key, ".rds"))
  tmp_file   <- paste0(cache_file, ".tmp")

  saveRDS(
    list(geometry = sf::st_geometry(splitted), result = payload),
    tmp_file
  )
  file.rename(tmp_file, cache_file)
}
