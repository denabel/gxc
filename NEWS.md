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
  This is a completely new argument — previously no explicit control over
  baseline aggregation existed. Accepts `"mean"` (default), `"median"`,
  `"min"`, `"max"`, `"sd"`, or percentiles `"p05"`, `"p10"`, `"p20"`,
  `"p80"`, `"p90"`, `"p95"`. Custom functions are also accepted but receive
  the label `"custom"` in the output metadata.
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
  indicator    = "2m_temperature",
  months       = c(3, 4, 5),
  baseline     = c("1980", "2010"),
  baseline_fun = "mean",
  stat_wrangling = "deviation"
)
```

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
| `.time_unit` | Temporal unit of the study period (`"days"` or `"months"`) |
| `.result_unit` | Unit of `.result` (e.g. `"K"`, `"sd"`, `"days"`) |
| `.study_fun` | Aggregation function used for focal period |
| `.baseline_fun` | Aggregation function used for baseline (`NA` if no baseline) |
| `.baseline_years` | Baseline year range (`NA` if no baseline) |
| `.time_span` | `time_span` argument value |
| `.months` | `months` argument value — `link_monthly()` only (`NA` if not used) |
| `.time_lag` | `time_lag` argument value |
| `.buffer` | `buffer` argument value |

When `prefix` is specified, all column names gain the prefix suffix (e.g.
`.study_temp_7d`).

For `link_daily.SpatRaster()` and `link_monthly.SpatRaster()`, metadata is
stored via `terra::metags()` instead of columns, since raster layers cannot hold
character data.

#### Batched API requests

Previously, all requested days or months were submitted as a single API request
containing the full date range. This caused two problems: date ranges spanning
year boundaries were not correctly represented, and large combined requests are
discouraged by the Copernicus API.

Requests are now split into individual API calls — one per day for
`link_daily()` and one per month for `link_monthly()` — which is the approach
recommended by the Copernicus CDS. These individual requests are submitted as a
single batch before extraction begins, so the API's parallel download capacity
(`workers = 6`) is used across all dates simultaneously.

Additionally, all observation and baseline requests are now submitted upfront
before the extraction loop starts, rather than one request per date group. A
global spatial extent is computed once over the full dataset and reused across
all splits, eliminating redundant downloads and significantly reducing total
download time for datasets with many unique dates.

---

### Bug fixes

- Fixed year-boundary issues when `time_span` or `months` spans across a
  calendar year (e.g. a December date with a window reaching into November of
  the previous year). Years, months, and days are now passed as paired vectors
  rather than independently, preventing incorrect cross-product combinations.
- Fixed `raster_timestamp()` failing when the number of date combinations
  produced by `make_dates()` did not match the number of raster layers.
- Fixed `link_daily.SpatRaster()` sending coordinates in the wrong CRS to the
  ERA5 API. The extent is now always computed in WGS84.
- Fixed monthly requests being incorrectly routed through the daily
  `wf_request_batch` pipeline. Monthly requests now use a dedicated
  `.submit_era5_monthly_batch()` function with per-month splitting.
- Fixed slow extraction for datasets where all observations share the same
  date and `time_span > 0`. `terra::app` and `terra::extract` are now called
  once for all points in a split rather than once per point, reducing
  extraction time by an order of magnitude.

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
- New `.safe_rast()` safely loads multiple raster files into a single
  `SpatRaster`, resampling to a common extent if files have mismatched extents.
- New `.resolve_baseline_fun()` resolves string or function arguments for
  `baseline_fun` and `study_fun`.
- New `.resolve_months()` handles year-boundary logic for explicit month windows.
- New `.catalogue_source()` derives the data source (`"era5"` or `"dwd"`) from
  the catalogue name, enabling dispatch to the correct requester.
- New `.indicator_units` lookup table maps indicator names to physical units.
- `compute_stat_wrangling()` is now an S3 generic with `.numeric` and
  `.SpatRaster` methods.
- `.toi_extract_impl()` now vectorises extraction over all points in a split
  when they share the same `time_span_seq`, with a row-wise fallback for
  datasets with varying date windows.
- `.toi_extract_grid()` and `.toi_extract_impl()` handle both daily and monthly
  rasters via an `is_monthly` flag, normalising dates for correct layer matching.
- `.transform_time()` gains a `months` argument for explicit month windows.
- Internal data objects (`allowed_catalogues_*`, `allowed_indicators_*`,
  `allowed_hours`, etc.) moved to `aaa_data.R` to ensure they are loaded before
  all other files, fixing documentation generation errors.
