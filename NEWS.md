# gxc — Changelog

## Development version

### New features

#### Baseline statistics (`stat_wrangling`)

`link_daily()` and `link_monthly()` now support a `stat_wrangling` argument that
controls how the focal period is compared to the baseline. Four options are
available:

- `"deviation"` (default): difference between focal and baseline reference value
- `"sd_deviation"`: z-score, i.e. deviation divided by the baseline standard deviation
- `"count_above"`: number of days/months in the focal period that exceed the
  baseline reference value (requires `time_span > 0` or `months`)
- `"count_below"`: analogous, counting days/months below the baseline reference value

```r
link_daily(
  pts,
  indicator      = "2m_temperature",
  time_span      = 7,
  baseline       = c("1980", "2010"),
  stat_wrangling = "count_above"
)
```

#### Baseline and study period aggregation functions

Two new arguments control how values are aggregated before comparison:

- `baseline_fun`: how the baseline layers are collapsed to a reference value.
  Accepts `"mean"` (default), `"median"`, `"min"`, `"max"`, `"sd"`, or
  percentiles `"p05"`, `"p10"`, `"p20"`, `"p80"`, `"p90"`, `"p95"`. Custom
  functions are also accepted but receive the label `"custom"` in the output
  metadata.
- `study_fun`: how the focal layers are collapsed to a single summary value
  (same options). Only relevant when `time_span > 0` or `months` is specified.

Note: some earlier development versions of `link_daily()` accepted a
`baseline_fun` argument as a raw function (e.g.
`function(x) mean(x, na.rm = TRUE)`). Raw functions are still accepted for
backwards compatibility but the string-based interface is now preferred.

```r
link_daily(
  pts,
  indicator      = "2m_temperature",
  time_span      = 30,
  baseline       = c("1980", "2010"),
  baseline_fun   = "p90",
  study_fun      = "mean",
  stat_wrangling = "count_above"
)
```

#### Multiple `baseline_fun`/`stat_wrangling` combinations

`baseline_fun` and `stat_wrangling` can each be given as a **list** to request
several combinations in a single call, e.g. comparing against both the median
and the 90th percentile of the baseline in one go:

```r
link_daily(
  pts,
  indicator      = "air_temperature_mean",
  catalogue      = "dwd-hyras-daily",
  baseline       = c("1961", "1990"),
  baseline_fun   = list("median", "p90"),
  stat_wrangling = list("count_above", "count_above")
)
```

Combinations are paired **positionally** (element 1 with element 1, element 2
with element 2 — not a cross product). If one of the two arguments is length 1
and the other is longer, the length-1 one is recycled against the longer one.
An error is raised if the lengths differ and neither is 1.

The expensive part of the computation — downloading and extracting baseline
values — is shared across all requested combinations; only the final
aggregation (`baseline_fun`/`stat_wrangling`) is repeated per combination, on
already-extracted values. This avoids re-running the (often dominant)
extraction step once per combination.

The return value changes shape when more than one combination is requested:
instead of a single `sf`/`SpatRaster` object, a **named list** of results is
returned, one per combination, named `"{baseline_fun_name}_{stat_wrangling}"`.
A single combination (the default, and the case when both arguments are given
as plain strings/functions) continues to return a single object, unchanged
from previous versions.

```r
result <- link_daily(
  pts, indicator = "air_temperature_mean", catalogue = "dwd-hyras-daily",
  baseline = c("1961", "1990"),
  baseline_fun = list("median", "p90"), stat_wrangling = "count_above"
)
result$median_count_above
result$p90_count_above
```

This is currently only supported for `sf` input, not `SpatRaster` input —
`SpatRaster` input still accepts only a single `baseline_fun`/`stat_wrangling`
value each.

#### Explicit month window (`months`)

Both `link_daily()` and `link_monthly()` now support a `months` argument for
specifying an explicit set of months as the study period instead of a rolling
`time_span`. For example, `months = c(3, 4, 5)` selects March–May. If the
input date falls within one of the specified months, the window is
automatically shifted one year back to avoid using incomplete data. Months
given out of calendar order (e.g. `c(12, 1, 2)` for a winter window) correctly
span a year boundary — no special tagging is needed, the year rollover is
detected automatically from a decrease in month number within the sequence.

`months` and `time_span` cannot be used simultaneously — an informative error
is thrown if both are provided.

```r
link_daily(
  pts,
  indicator      = "air_temperature_mean",
  catalogue      = "dwd-hyras-daily",
  months         = c(3, 4, 5),
  baseline       = c("1980", "2010"),
  baseline_fun   = "mean",
  stat_wrangling = "count_above"
)

link_monthly(
  pts,
  indicator      = "2m_temperature",
  months         = c(3, 4, 5),
  baseline       = c("1980", "2010"),
  baseline_fun   = "mean",
  stat_wrangling = "deviation"
)
```

For `link_daily()`, `months` expands to the full **daily** sequence spanning
the resolved months (e.g. every day from March 1 through May 31), since
`link_daily()` matches individual days rather than months. For
`link_monthly()`, `months` resolves to one value per month, matching against
monthly-resolution rasters directly — unchanged from previous behaviour.

Note: `months` combined with `stat_wrangling = "count_above"`/`"count_below"`
on `link_monthly()` compares against a monthly-resolution baseline, so the
resulting count is out of the number of *months* in the window (e.g. out of 3
for `c(3, 4, 5)`), not out of the number of days. `link_daily()` with `months`
counts at the daily level instead (out of the number of days in the window).

#### Output column naming (`prefix`)

A new `prefix` argument allows naming the output columns when calling
`link_daily()` or `link_monthly()` multiple times on the same dataset. Without
a prefix, columns are named `.study`, `.baseline`, `.result` etc. With
`prefix = "temp_7d"`, they become `.study_temp_7d`, `.baseline_temp_7d` etc.

This makes it straightforward to combine multiple indicator calls:

```r
pts |>
  link_daily(indicator = "2m_temperature", time_span = 7,  prefix = "temp_7d") |>
  link_daily(indicator = "2m_temperature", time_span = 30, prefix = "temp_30d")
```

#### DWD catalogue support

`link_daily()` and `link_monthly()` now support data from the German Weather
Service (DWD) in addition to ERA5. Two new catalogues are available:

- `"dwd-hyras-daily"`: daily HYRAS gridded data for Germany at 1 km resolution,
  downloaded directly from the DWD open data server. Year files are cached
  locally and sliced into individual daily `.tif` files on first use.
- `"dwd-monthly"`: monthly gridded data for Germany at 1 km resolution,
  downloaded as compressed ASCII grid files (`.asc.gz`) and converted to `.tif`
  on first use.

No API key is required for DWD data.

```r
# Daily air temperature
link_daily(
  pts,
  indicator = "air_temperature_mean",
  catalogue = "dwd-hyras-daily"
)

# Monthly drought index with baseline
link_monthly(
  pts,
  indicator      = "drought_index",
  catalogue      = "dwd-monthly",
  months         = c(6, 7, 8),
  baseline       = c("1980", "2010"),
  stat_wrangling = "deviation"
)
```

The following indicators are currently supported:

**`dwd-hyras-daily`**: `air_temperature_mean`, `air_temperature_max`,
`air_temperature_min`, `precipitation`

**`dwd-monthly`**: `air_temperature_mean`, `air_temperature_max`,
`air_temperature_min`, `precipitation`, `drought_index`

#### Spatial downsampling (`downsample_factor`)

`link_daily()` and `link_monthly()` gain two new arguments for trading a small
amount of spatial precision for a substantial extraction speedup on buffered
(polygon) extractions:

- `downsample_factor`: integer (e.g. `5`) or `NULL` (default, off). If set, an
  aggregated (coarser-resolution) copy of the downloaded raster is built once
  per unique set of files (and cached across calls sharing the same files and
  factor) and used for polygon extraction where the buffer is large enough
  (see `downsample_min_buffer`).
- `downsample_min_buffer`: numeric, default `0`. Minimum buffer radius
  (metres) for the aggregated raster to be used; smaller buffers still use
  full resolution. The default `0` means every real buffer (`> 0`) uses the
  aggregated version — point extraction (`buffer = 0`) is never downsampled
  regardless of this setting, since the aggregated-vs-full-resolution
  difference for single-cell point lookups was measured to be roughly 10x
  larger than for any buffered extraction.

```r
link_daily(
  pts,
  indicator             = "air_temperature_mean",
  catalogue             = "dwd-hyras-daily",
  buffer                = 100000,
  baseline              = c("1961", "1990"),
  baseline_fun          = "median",
  stat_wrangling        = "count_above",
  downsample_factor     = 5,
  downsample_min_buffer = 0
)
```

Own benchmarking (5–100km buffers, `downsample_factor = 5`, 1km source
resolution) measured a 10–32x extraction speedup with well under 0.3%
relative difference from full-resolution results across the whole buffer
range. This is opt-in (`downsample_factor = NULL` by default) since it does
trade a small amount of numerical precision for speed, and the tradeoff is a
data-analysis decision the caller should make deliberately rather than have
applied automatically.

The output gains a `.downsample_factor` column recording the factor actually
applied to each result (`NA` if downsampling wasn't requested, or wasn't
applicable — e.g. point extraction always stays at full resolution
regardless of this setting), so it's always possible to tell from the output
alone whether a given result used the native or an aggregated resolution —
important for reproducibility.

#### Opt-in results cache (`cache_results`)

`link_daily()` and `link_monthly()` gain two new arguments for caching the
final, already-extracted-and-aggregated per-date result to disk:

- `cache_results`: logical, default `FALSE`. If `TRUE`, a crash mid-run only
  loses whatever hasn't been cached yet, and re-runs with identical
  parameters skip computation entirely for already-cached dates. Distinct
  from `cache`, which only caches raw downloaded files — this caches the
  finished output.
- `results_cache_path`: character string, required when `cache_results =
  TRUE`. No default is provided deliberately — cached entries necessarily
  store the exact geometry of the input observations (needed for cache-key
  uniqueness and an integrity self-check on read), which for survey data can
  indirectly reveal respondent locations, so the storage location should be
  a conscious choice, not a fallback default.

```r
link_daily(
  pts,
  indicator          = "air_temperature_mean",
  catalogue          = "dwd-hyras-daily",
  baseline           = c("1961", "1990"),
  cache_results      = TRUE,
  results_cache_path = "/secure/path/not/under/version/control/"
)
```

The cache key incorporates every parameter that affects the output — indicator,
catalogue, `time_span`/`months`, `time_lag`, `buffer`, baseline settings,
`downsample_factor`, `prefix`, the effective date, and the exact geometry of
the observations in that date's split — so two different sets of observations
can never collide on the same cache entry. Reading a cache hit additionally
re-checks that the stored geometry matches the requested geometry, aborting
loudly on a mismatch rather than silently returning a result for the wrong
observations.

#### Redesigned output columns

Output columns are now consistently structured across all calls, regardless of
whether a baseline is requested. Calls without a baseline still produce all
columns, with `NA` in the baseline-specific ones — making `rbind()` across
different specifications straightforward.

The primary result columns come first, followed by metadata:

| Column | Content |
|---|---|
| `.study` | Focal period summary value |
| `.baseline` | Baseline reference value (`NA` if no baseline) |
| `.result` | Result of `stat_wrangling` (`NA` if no baseline) |
| `.indicator` | Indicator name (e.g. `"2m_temperature"`, `"air_temperature_mean"`) |
| `.unit` | Physical unit of the indicator (e.g. `"K"`, `"degC"`, `"mm"`) |
| `.resolution` | Spatial resolution of the source dataset (e.g. `"0.1° x 0.1°"`, `"1 km x 1 km"`) |
| `.time_unit` | Temporal unit of the study period (`"days"` or `"months"`) |
| `.result_unit` | Unit of `.result` (e.g. `"K"`, `"sd"`, `"days"`) |
| `.study_fun` | Aggregation function used for focal period |
| `.baseline_fun` | Aggregation function used for baseline (`NA` if no baseline) |
| `.baseline_years` | Baseline year range (`NA` if no baseline) |
| `.time_span` | Actual study window length used (in `time_unit`) |
| `.months` | `months` argument value (`NA` if not used) |
| `.time_lag` | `time_lag` argument value |
| `.buffer` | `buffer` argument value |
| `.downsample_factor` | Actually-applied downsampling factor (`NA` if `downsample_factor` wasn't set, or wasn't applicable — e.g. point extraction, or buffer below `downsample_min_buffer`) |
| `.source` | Dynamic citation string including the dataset name and access date |

When `prefix` is specified, all column names gain the prefix suffix (e.g.
`.study_temp_7d`). When multiple `baseline_fun`/`stat_wrangling` combinations
are requested, disambiguation across combinations happens via the names of
the returned list, not via additional column suffixing — each list element
keeps the plain column names above (optionally with the user-supplied
`prefix`).

For `link_daily.SpatRaster()` and `link_monthly.SpatRaster()`, metadata is
stored via `terra::metags()` instead of columns, since raster layers cannot hold
character data.

#### Progress reporting

The split-loop in `link_daily()` and `link_monthly()` uses a live progress bar
instead of a `cli_rule()` per date, keeping output concise regardless of how
many unique dates the dataset contains. The progress bar shows the current date
and overall progress, and a summary line is printed on completion:

```
⠸ Processing dates [34/62] · 2014-09-14 ██████████░░░░░░ 55%
✔ Done. 1000 observations across 62 dates linked in 8.4s.
```

The header block is printed immediately after input validation — before
`st_buffer()` and `split()` — so the user sees output right away instead of
waiting several seconds for spatial preprocessing to complete silently.

#### Batched API requests

Requests are split into individual API calls — one per day for `link_daily()`
and one per month for `link_monthly()` — which is the approach recommended by
the Copernicus CDS. These individual requests are submitted as a single batch
before extraction begins, so the API's parallel download capacity (`workers = 6`)
is used across all dates simultaneously.

All observation and baseline requests are submitted upfront before the extraction
loop starts. A global spatial extent is computed once over the full dataset and
reused across all splits, eliminating redundant downloads and significantly
reducing total download time for datasets with many unique dates.

#### Raster caching within a session

`.safe_rast()` now memoizes its result for the duration of the R session,
keyed by the exact set of file paths and their modification times. Repeated
calls that need the same underlying files — e.g. many `link_daily()` calls
sharing the same indicator, baseline years, and months, but differing in
`buffer` or `stat_wrangling` — reuse the cached raster instead of re-reading
file headers from disk every time. For a typical 30-year daily baseline
(~2,800 files), this turns a repeated ~20s raster-assembly cost into a
one-time cost per unique file set.

The cache automatically invalidates if any underlying file's modification
time changes (e.g. a corrupted file gets re-downloaded mid-session), so it
never silently serves stale data.

Additionally, individual files are now also cached at the single-file level
(not just as part of a full file set), so a *different* file set that
partially overlaps a previously-loaded one — e.g. two baseline periods
sharing some years, like 1961–1990 and 1981–2010 — reuses whichever
individual files it has in common instead of re-reading them, even though the
two file sets as a whole are different cache entries.

---

### Bug fixes

- Fixed ERA5 monthly date matching for indicators that store values on the last
  day of the preceding month (e.g. `total_precipitation`, `instantaneous_10m_wind_gust`,
  `snowfall`). A nearest-neighbour fallback within 31 days is now used when no
  exact date match is found.
- Fixed `is_monthly` detection in `extract.R` — previously relied on `day == 1`,
  which excluded indicators stored on the last day of the month. Detection now
  uses the median gap between dates (≥ 20 days) instead.
- Fixed `.safe_rast()` failing to set a correct spatial extent for ERA5
  Single-Levels NetCDF files, where terra reports `cells are not equally spaced`
  due to the Gaussian grid. The extent is now reconstructed from the `longitude`
  and `latitude` coordinate variables when the default 0–1 unit square is
  detected.
- Fixed a `cli` pluralization error (`Cannot pluralize without a quantity`) in
  `stash$restore()` when warning about missing cached files.
- Fixed year-boundary issues when `time_span` or `months` spans across a
  calendar year. Years, months, and days are now passed as paired vectors rather
  than independently, preventing incorrect cross-product combinations.
- Fixed `raster_timestamp()` failing when the number of date combinations
  produced by `make_dates()` did not match the number of raster layers.
- Fixed `link_daily.SpatRaster()` sending coordinates in the wrong CRS to the
  ERA5 API. The extent is now always computed in WGS84.
- Fixed monthly requests being incorrectly routed through the daily
  `wf_request_batch` pipeline. Monthly requests now use a dedicated
  `.submit_era5_monthly_batch()` function with per-month splitting.
- Fixed slow extraction for datasets where all observations share the same
  date and `time_span > 0`. `terra::app` and `terra::extract` are now called
  once for all points in a split rather than once per point.
- Fixed extraction becoming extremely slow — and, for large buffers,
  exhausting memory (`std::bad_alloc`) — because `.toi_extract_impl()` and
  `.toi_extract()` passed the full input `sf` object, including heavy nested
  list columns such as `time_span_seq`, into `terra::extract()`/
  `exactextractr::exact_extract()`. These only ever need the geometry;
  carrying the full attribute table along was measured to slow extraction
  down by roughly two orders of magnitude for a typical daily-linkage
  dataset. A new internal `.drop_heavy_columns()` strips list-type columns
  before extraction.
- Fixed polygon (`buffer > 0`) extraction using `terra::extract()`, which
  recomputes the polygon–raster cell overlap separately for every layer.
  This does not scale to baseline extractions with hundreds or thousands of
  layers, and can exhaust memory for large buffer radii (a large buffer can
  overlap tens of thousands of 1 km grid cells). Polygon geometries are now
  routed through `exactextractr::exact_extract()`, which computes each
  polygon's cell-coverage weights once and reuses them across all layers
  (~4.6x faster in benchmarking on a real buffered extraction, in addition
  to no longer crashing on large buffers). Point geometries (`buffer = 0`)
  continue to use `terra::extract()`, which is already efficient for them.
- Fixed `buffer = 0` unexpectedly triggering polygon-based rather than
  point-based extraction. `sf::st_buffer(x, 0)` converts point geometries
  into (zero-area) polygon geometries rather than leaving them unchanged.
  `link_daily.sf()` and `link_monthly.sf()` now skip the buffering step
  entirely when `buffer = 0`.
- Fixed baseline extraction using `terra::extract(..., fun = NULL)` to
  retrieve raw per-cell values. This is only meaningful for point geometries
  (exactly one cell per point); for polygons it returns a variable number of
  raw cell values per feature, which downstream code incorrectly assumed was
  always exactly one value per feature per layer. Baseline extraction now
  always aggregates to one (area-weighted, for polygons) value per feature
  per layer via `.extract_values()`.
- Fixed `.safe_rast()` loading every file individually in a loop
  (`lapply(paths, terra::rast)`), even in the common case where all files
  share identical geometry. `.safe_rast()` now attempts a single vectorized
  `terra::rast(paths)` call first, and only falls back to the slower
  per-file-plus-resample path if that fails or produces an unexpected number
  of layers (i.e. geometry genuinely differs somewhere).
- Fixed `exactextractr::exact_extract()` erroring with `names of input
  rasters must be unique` when raster layers selected for baseline
  extraction (e.g. the same day-of-year across many baseline years) share
  identical layer names. `.extract_values()` now assigns unique placeholder
  layer names before extraction; no downstream code relies on layer names,
  only on column position, so this is always safe.
- Fixed `exactextractr::exact_extract()` returning a plain numeric vector
  instead of a data.frame when the raster passed to it has exactly one
  layer (e.g. after a focal window is collapsed to a single layer for
  `stat_wrangling = "deviation"`/`"sd_deviation"` with `buffer > 0`).
  Downstream code always assumed a data.frame, causing a cryptic
  `subscript out of bounds` error. `.extract_values()` now always coerces
  its result to a data.frame regardless of layer count.
- Fixed `.toi_extract_impl()`'s focal (non-baseline) extraction branch
  checking `stat_wrangling %in% c("count_above", "count_below")` as if it
  were always a single string, even when multiple `baseline_fun`/
  `stat_wrangling` combinations were requested (a list). This produced a
  logical vector of length > 1 passed to `if()`, erroring with `argument
  has length > 1`, whenever a multi-combination call mixed
  `count_above`/`count_below` on the focal side. The check now inspects
  `any(unlist(stat_wrangling) %in% ...)`, matching the equivalent fix
  already applied to the baseline branch.
- Fixed `.add_baseline()`/`link_daily.sf()` appending the combination
  label to output column names (e.g. `.result_mean_deviation`) whenever
  more than one `baseline_fun`/`stat_wrangling` combination was requested,
  in addition to disambiguating via the returned list's names. This left
  the actually-expected plain column (e.g. `.result`) at its `NA`
  placeholder value inside each list element, while the real computed
  values sat unused in the mislabelled column. Column naming inside each
  combination's result now always reflects only the user-supplied
  `prefix`; disambiguation across combinations happens solely through the
  list names, as documented.
- Fixed `.time_span` metadata showing the raw (irrelevant) `time_span`
  argument value instead of the actual daily window length when `months`
  was used with `link_daily()`.
- Fixed `.safe_rast()`'s two-level cache skipping its geometry-alignment
  check whenever every needed file was already present in the
  single-file cache (`.raster_file_cache`). Layers retrieved from that
  cache can originate from an entirely different earlier `.safe_rast()`
  call and are not guaranteed to share a common grid with this call's
  other files, even though each was individually fine in its own
  original context; skipping the check let genuinely mismatched cached
  layers reach `do.call(c, ...)` directly, erroring with `[rast] extents
  do not match` instead of resampling. The alignment check now always
  runs against every layer being combined, regardless of whether any
  file needed loading this time.
- Fixed a `==` typo in `enrich()`'s `ags` branch (`ags == args$ags`
  instead of `ags = args$ags`), which passed a stray logical comparison
  as a positional argument to `ffm::bkg_admin()` instead of naming the
  `ags` parameter.
- Added the previously missing `make_empty_geometry()` helper used by
  `left_merge()` to pad unmatched rows in a spatial column with a valid
  empty geometry of the correct type, instead of erroring with an
  undefined function.
- Fixed non-ASCII characters (em dashes in comments) in `requests.R` and
  added the `Imports` (`countrycode`, `geodata`, `geonames`, `giscoR`,
  `glue`, `lubridate`, `purrr`) and `@importFrom` declarations (`stats::
  median`/`sd`, `utils::capture.output`/`download.file`) that `R CMD
  check` flagged as missing.

---

### Breaking changes

The output column names have changed. Code that referenced `.linked`,
`.deviation`, or `.baseline` directly will need to be updated:

| Old | New |
|---|---|
| `.linked` | `.study` |
| `.baseline` | `.baseline` (unchanged, but now always present) |
| `.deviation` | `.result` |

The `catalogue` argument now accepts DWD catalogues in addition to ERA5. The
default remains `"derived-era5-land-daily-statistics"` for `link_daily()` and
`"reanalysis-era5-land-monthly-means"` for `link_monthly()`.

`exactextractr` is now a required dependency (used for polygon/buffer
extraction).

`link_daily()` and `link_monthly()` may now return a **named list** of
results instead of a single `sf`/`SpatRaster` object, when more than one
`baseline_fun`/`stat_wrangling` combination is requested (see "Multiple
`baseline_fun`/`stat_wrangling` combinations" above). Existing single-value
calls are unaffected and continue to return a single object as before.

---

### Internal changes

- New internal functions `.build_era5_daily_request()`,
  `.build_era5_monthly_request()`, `.submit_era5_batch()`, and
  `.submit_era5_monthly_batch()` separate request construction from submission,
  enabling the batched upfront download strategy.
- New `.submit_batch()` consolidates shared batch submission logic for daily
  and monthly ERA5 requests.
- New `.request_dwd_daily()` and `.request_dwd_monthly()` handle DWD data
  download, decompression, CRS assignment, and per-day/per-month caching.
- New `.decompress_gz()` decompresses `.gz` files using base R without
  external dependencies.
- `.safe_rast()` now uses an internal `load_one()` helper that detects and
  corrects malformed extents in ERA5 Single-Levels NetCDF files by reading
  `longitude`/`latitude` variables directly via `ncdf4`.
- New `.resolve_baseline_fun()` resolves string or function arguments for
  `baseline_fun` and `study_fun`.
- New `.resolve_months()` handles year-boundary logic for explicit month windows.
- New `.catalogue_source()` derives the data source (`"era5"` or `"dwd"`) from
  the catalogue name, enabling dispatch to the correct requester.
- New `.catalogue_citation()` generates dynamic citation strings with ERA5 DOIs
  and DWD dataset names, stored in the `.source` output column.
- New `.indicator_units` lookup table maps indicator names to physical units,
  stored in the `.unit` output column.
- `compute_stat_wrangling()` is now an S3 generic with `.numeric` and
  `.SpatRaster` methods.
- `.toi_extract_impl()` now vectorises extraction over all points in a split
  when they share the same `time_span_seq`, with a row-wise fallback for
  datasets with varying date windows.
- New internal `.drop_heavy_columns()` (in `extract.R`) strips list-type
  attribute columns (e.g. `time_span_seq`) from an `sf` object before
  extraction, keeping it a plain `sf` object throughout rather than
  converting to a bare geometry/`SpatVector`.
- New internal `.extract_values()` (in `extract.R`) consolidates all
  point/polygon extraction into a single helper: `terra::extract()` for
  point geometries, `exactextractr::exact_extract()` for polygon geometries,
  always returning a `nrow(features) x nlyr(raster)` data.frame (the same
  type `terra::extract()`/`exact_extract()` already return natively).
  Replaces direct `terra::extract()` calls throughout `.toi_extract_impl()`
  and `.toi_extract()`. Also dispatches to a downsampled raster (see
  `downsample_factor`) for polygon geometries where applicable.
- New internal `.sub_layers()` (in `extract.R`) subsets a raster to specific
  layers while correctly re-subsetting an attached `"coarse"` (downsampled)
  attribute to the same layers — plain attribute carry-through on
  `raster[[idx]]` does NOT re-subset the attribute's own layers, which would
  otherwise silently extract from the wrong set of downsampled layers.
- `.safe_rast()` gains an in-memory, session-scoped cache
  (`.raster_memo_cache`), keyed by a hash of the sorted file paths and their
  modification times, plus a second, single-file-level cache
  (`.raster_file_cache`) for partial reuse across different-but-overlapping
  file sets.
- `.load_climate_raster()` gains a `downsample_factor` argument; the
  aggregated raster is memoized separately (`.raster_coarse_cache`), keyed
  by file set and factor, and attached to the returned raster as a
  `"coarse"` attribute for `.extract_values()` to pick up.
- `.add_baseline()`'s signature changed from singular `baseline_fun`/
  `baseline_fun_name`/`stat_wrangling` arguments to `baseline_fun_list`/
  `baseline_fun_names`/`stat_wrangling_list`, to support sharing extraction
  across multiple combinations (see "Multiple `baseline_fun`/
  `stat_wrangling` combinations" above). Returns a single object when
  `length(baseline_fun_list) == 1` (unchanged behaviour), or a named list
  otherwise. All call sites (`link_daily.sf()`, `link_daily.SpatRaster()`,
  `link_monthly.sf()`, `link_monthly.SpatRaster()`) updated accordingly.
- New internal `results_cache.R` module (`.results_cache_key()`,
  `.read_results_cache()`, `.write_results_cache()`) implements the opt-in
  per-date results cache (see "Opt-in results cache" above), using base R
  `saveRDS()`/`readRDS()` rather than an additional package dependency.
  Keys include a `link_type` discriminator (`"daily"`/`"monthly"`) so
  `link_daily()` and `link_monthly()` can never collide on the same cache
  entry even when sharing `results_cache_path` with otherwise-identical
  parameters.
- is_monthly detection uses median gap between dates (≥ 20 days) rather than
  checking `day == 1`, making it robust to indicators stored on the last day of
  the month.
- `.transform_time()` gains a `months` argument for explicit month windows,
  and a `daily_expansion` argument (default `FALSE`): when `TRUE` and
  `months` is set, `time_span_seq` contains the full daily sequence spanning
  the resolved months rather than one anchor date per month, needed for
  `link_daily()`'s day-level matching. `link_monthly()` is unaffected
  (`daily_expansion` defaults to `FALSE` there).
- Daily ERA5 requests include `data_format = "netcdf"` and
  `download_format = "unarchived"` to ensure consistent NetCDF output across
  all ERA5 catalogues.
- Internal data objects (`allowed_catalogues_*`, `allowed_indicators_*`,
  `allowed_hours`, etc.) moved to `aaa_data.R` to ensure they are loaded before
  all other files.
