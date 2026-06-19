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

#### Redesigned output columns

Output columns are now consistently structured across all calls, regardless of
whether a baseline is requested. Calls without a baseline still produce all
columns, with `NA` in the baseline-specific ones — making `rbind()` across
different specifications straightforward.

| Column | Content |
|---|---|
| `.study` | Focal period summary value |
| `.baseline` | Baseline reference value (`NA` if no baseline) |
| `.result` | Result of `stat_wrangling` (`NA` if no baseline) |
| `.indicator` | Indicator name |
| `.unit` | Unit of `.result` (e.g. `"K"`, `"sd"`, `"days_above"`) |
| `.study_fun` | Aggregation function used for focal period |
| `.baseline_fun` | Aggregation function used for baseline (`NA` if no baseline) |
| `.baseline_years` | Baseline year range (`NA` if no baseline) |
| `.time_span` | `time_span` argument value |
| `.months` | `months` argument value (`NA` if not used) |
| `.time_lag` | `time_lag` argument value |
| `.buffer` | `buffer` argument value |

For `link_daily.SpatRaster()` and `link_monthly.SpatRaster()`, metadata is
stored via `terra::metags()` instead of columns, since raster layers cannot hold
character data.

#### Batched API requests

Observation and baseline requests are now submitted as a single batch before
extraction begins, rather than one request per date group. This means the API's
parallel download capacity (`workers = 6`) is used across all dates
simultaneously, significantly reducing total download time for datasets with
many unique dates.

Additionally, a global spatial extent is computed once over the full dataset
rather than per date group. The downloaded raster is then reused across all
splits, eliminating redundant downloads.

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

---

### Breaking changes

The output column names have changed. Code that referenced `.linked`,
`.deviation`, or `.baseline` directly will need to be updated:

| Old | New |
|---|---|
| `.linked` | `.study` |
| `.baseline` | `.baseline` (unchanged, but now always present) |
| `.deviation` | `.result` |

---

### Internal changes

- New internal functions `.build_era5_daily_request()`,
  `.build_era5_monthly_request()`, `.submit_era5_batch()`, and
  `.submit_era5_monthly_batch()` separate request construction from submission,
  enabling the batched upfront download strategy.
- New `.resolve_baseline_fun()` resolves string or function arguments for
  `baseline_fun` and `study_fun`.
- New `.resolve_months()` handles year-boundary logic for explicit month windows.
- `compute_stat_wrangling()` is now an S3 generic with `.numeric` and
  `.SpatRaster` methods.
- `.toi_extract_grid()` and `.toi_extract_impl()` now accept a `stat_wrangling`
  argument and return uncollapsed layer stacks for count-based operations.
- `.transform_time()` gains a `months` argument for explicit month windows.
