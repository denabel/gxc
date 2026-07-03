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

#### Explicit month window (`months`) — `link_monthly()` only

`link_monthly()` gains a `months` argument for specifying an explicit set of
months as the study period instead of a rolling `time_span`. For example,
`months = c(3, 4, 5)` selects March–May. If the input date falls within one of
the specified months, the window is automatically shifted one year back to avoid
using incomplete data.

`months` and `time_span` cannot be used simultaneously — an informative error is
thrown if both are provided.

```r
link_monthly(
  pts,
  indicator      = "2m_temperature",
  months         = c(3, 4, 5),
  baseline       = c("1980", "2010"),
  baseline_fun   = "mean",
  stat_wrangling = "deviation"
)
```

Note: `months` combined with `stat_wrangling = "count_above"`/`"count_below"`
compares against a monthly-resolution baseline, so the resulting count is out
of the number of *months* in the window (e.g. out of 3 for `c(3, 4, 5)`), not
out of the number of days. For a day-level count over a calendar-month season,
use `link_daily()` with an explicit `time_span`/`time_lag` covering the same
date range instead.

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
| `.time_span` | `time_span` argument value |
| `.months` | `months` argument value — `link_monthly()` only (`NA` if not used) |
| `.time_lag` | `time_lag` argument value |
| `.buffer` | `buffer` argument value |
| `.source` | Dynamic citation string including the dataset name and access date |

When `prefix` is specified, all column names gain the prefix suffix (e.g.
`.study_temp_7d`).

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
  Replaces direct
  `terra::extract()` calls throughout `.toi_extract_impl()` and
  `.toi_extract()`.
- `.safe_rast()` gains an in-memory, session-scoped cache
  (`.raster_memo_cache`), keyed by a hash of the sorted file paths and their
  modification times.
- is_monthly detection uses median gap between dates (≥ 20 days) rather than
  checking `day == 1`, making it robust to indicators stored on the last day of
  the month.
- `.transform_time()` gains a `months` argument for explicit month windows.
- Daily ERA5 requests include `data_format = "netcdf"` and
  `download_format = "unarchived"` to ensure consistent NetCDF output across
  all ERA5 catalogues.
- Internal data objects (`allowed_catalogues_*`, `allowed_indicators_*`,
  `allowed_hours`, etc.) moved to `aaa_data.R` to ensure they are loaded before
  all other files.
