
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check.yaml](https://github.com/denabel/gxc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denabel/gxc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/denabel/gxc/graph/badge.svg)](https://app.codecov.io/gh/denabel/gxc)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![Last-changedate](https://img.shields.io/badge/last%20change-2025--03--09-yellowgreen.svg)](/commits/master)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15041278.svg)](https://doi.org/10.5281/zenodo.15041278)
<!-- 
[![pkgcheck](https://github.com/denabel/gxc/workflows/pkgcheck/badge.svg)](https://github.com/denabel/gxc/actions?query=workflow%3Apkgcheck)
-->

# gxc: Easy access to Earth observation data 🌐

<img src="man/figures/logo.png" align="right" height="200"/>

For many researchers in the social sciences, **Earth observation (EO)**
data is a black box. This is mainly caused by a lack of knowledge about
techniques or data sources and unfamiliarity with complex data formats
such as high-resolution spatio-temporal raster data cubes. `gxc`
connects social science data to EO data sources, especially Copernicus,
enabling easy integration of spatial and temporal EO indicators. The
package is designed for social scientists but may also be useful for
earth system researchers.

Key features:

- Link social science data to Copernicus and other EO sources via five
  main attributes: indicator, intensity, time period, baseline, and
  spatial buffer
- Easily integrate complex spatio-temporal data formats into social
  science workflows
- Curated EO indicators: weather, climate, with more coming soon (e.g.,
  air quality, GHG, land cover)
- Supports an interactive interface through Shiny: *coming soon*
- FAIR and open science principles

For more infos and tutorials, check out our [online
compendium](https://denabel.github.io/gxc_pages/).

For our current list of curated indicators, see our [indicator
catalogue](https://denabel.github.io/gxc_pages/catalogue.html).

## Installation instructions

To install the package from GitHub:

``` r
# if (!require(pak)) install.packages("pak")
remotes::install_github("denabel/gxc")
library(gxc)
```

## API Access

The `gxc` linking functions integrate different data storages and APIs
to retrieve earth observation data. The main (and currently only)
service is the [ECMWF API](https://www.ecmwf.int/), which provides
access to indicators from the Copernicus services (e.g., on
[climate](https://cds.climate.copernicus.eu/),
[atmosphere](https://ads.atmosphere.copernicus.eu/), and [early
warning](https://ewds.climate.copernicus.eu/)). In the future, we aim to
integrate more data sources.

To access these data, you need an [ECMWF
account](https://www.ecmwf.int/user/login). All requests are performed
using the [ecmwfr](https://github.com/bluegreen-labs/ecmwfr) package.

To set your API key inside R, you can use the convenience function
`set_api_key()`, which opens an interactive prompt where you can enter
the key in a safe environment:

``` r
set_api_key("ecmwfr")
```

## How to use

### Example 1: Retrieving daily temperature for point data

In this first example, we show how to utilize the `link_daily`-function
to integrate temperature data from ERA5 reanalysis for a set of spatial
points. Let’s assume we have a series of georeferenced social media
posts on climate change and we would like to understand how these are
associated with temperature patterns at the person’s location.

### Package setup

We need some packages to load and prepare the world map
(`rnaturalearth`, `sf`, and `ggplot2`).

``` r
library(rnaturalearth)
library(sf)
library(ggplot2)
library(gxc)
```

### Create sample point data

Let’s assume we have a sample of social media posts across Germany
covering the time period from July to August 2019. We would like to
extend this dataset with temperature data from the specific day of the
content post. We create a sample of random points based on a shapefile
for Germany and add random day variables for the field period.

``` r
# Get Germany's boundary as an sf object
germany <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")

# Generate 1000 random points within Germany's boundary
n <- 1000
random_points <- st_sample(germany, size = n)
points_sf <- st_sf(geometry = random_points)

# Random date within day, month and year limits of field period
set.seed(123)
days <- sample(1:31, n, replace = TRUE)
months <- sample(c(7, 8), n, replace = TRUE)
dates <- sprintf("%s-%s-%s", 2019, months, days)
points_sf$date <- dates
```

### Do the linking

For our example, we would like to retrieve the daily maximum temperature
(`statistic = "daily_maximum`) for the specific tweet day
(`time_span = 0` and `time_lag = 0`) in a 10km area around the users’
location (`buffer = 10`).

``` r
result <- link_daily(points_sf, indicator = "2m_temperature", buffer = 10)
```

### Explore the extended dataset

We can see that the function has added additional columns on the linking
dates, and the actual values (in Kelvin), averaged across the buffer
zone.

``` r
result
#> Simple feature collection with 1000 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 6.07487 ymin: 47.39451 xmax: 14.76799 ymax: 54.8779
#> Geodetic CRS:  WGS 84
#> # A tibble: 1,000 × 3
#>    date      .linked            geometry
#>    <chr>       <dbl>         <POINT [°]>
#>  1 2019-8-31    299. (11.79916 52.39787)
#>  2 2019-7-15    288. (7.603277 52.78602)
#>  3 2019-8-19    291. (8.861578 52.36923)
#>  4 2019-8-14    289. (8.890016 47.78646)
#>  5 2019-7-3     293. (12.27968 48.72903)
#>  6 2019-7-10    290.  (11.8915 48.92563)
#>  7 2019-7-18    293.  (6.916019 51.2323)
#>  8 2019-7-22    295. (10.06352 49.19944)
#>  9 2019-7-11    292.  (13.7623 51.35259)
#> 10 2019-7-5     292. (12.74709 52.06254)
#> # ℹ 990 more rows
```

``` r
ggplot(result) +
  geom_sf(aes(color = .linked)) +
  scale_color_viridis_c() +
  theme_void() +
  labs(
    title = "Mean temperature (K) in July/August 2019",
    subtitle = "At respondent location on interview day",
    fill = "Temperature (K)"
  )
```

<img src="man/figures/README-visualize_example1-1.png" width="100%" />

### Example 2: Retrieving monthly averaged precipitation for countries

In this example, we show how to utilize the `link_monthly` function to
integrate precipitation data from the ERA5 reanalysis across countries
and for a specific point in time. We will enable parallel processing.

### Package setup

To enable parallelization, the
[`future`](https://future.futureverse.org/) package is needed. Using
`future::plan`, you can set up what kind of parallel processes to use.
By using a multi-session plan each parallel process uses a clean and
separate R session. A rule of thumb is to use one worker less than the
available number of workers on the system.

``` r
library(future)
plan(multisession, workers = availableCores() - 1)
```

### Load a world map

Let’s assume we require global precipitation data for October 2014. We
load the shapefile containing country-level polygons, subset it to the
most relevant variables, and add a time variable.

``` r
# Download world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[!world$admin %in% "Antarctica", ]
world <- world[c("admin", "iso_a3", "postal", "geometry")]

# Create fixed date-variable
world$date <- "2014-08-01"

# Plot world map
plot(world[1])
```

<img src="man/figures/README-map-1.png" width="100%" />

### Do the linking

We want to directly retrieve the averaged total precipitation data for
August 2014 (`time_span = 0` and `time_lag = 0`). We furthermore enable
parallel processing (`parallel = TRUE`) and rely on the default chunk
size (`chunk_size = 50`).

``` r
result <- link_monthly(world, indicator = "total_precipitation", parallel = TRUE)
```

### Explore the extended dataset

We can see that the function has added additional columns on the linking
dates, and the actual values, averaged across countries.

``` r
result
#> Simple feature collection with 241 features and 5 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -180 ymin: -58.49229 xmax: 180 ymax: 83.59961
#> Geodetic CRS:  WGS 84
#> # A tibble: 241 × 6
#>    admin                   iso_a3 postal date  .linked                  geometry
#>    <chr>                   <chr>  <chr>  <chr>   <dbl>        <MULTIPOLYGON [°]>
#>  1 Zimbabwe                ZWE    ZW     2014… 5.76e-5 (((31.28789 -22.40205, 3…
#>  2 Zambia                  ZMB    ZM     2014… 1.20e-5 (((30.39609 -15.64307, 3…
#>  3 Yemen                   YEM    YE     2014… 8.09e-4 (((53.08564 16.64839, 52…
#>  4 Vietnam                 VNM    VN     2014… 9.12e-3 (((104.064 10.39082, 104…
#>  5 Venezuela               VEN    VE     2014… 8.93e-3 (((-60.82119 9.138379, -…
#>  6 Vatican                 VAT    V      2014… 2.70e-4 (((12.43916 41.89839, 12…
#>  7 Vanuatu                 VUT    VU     2014… 1.29e-3 (((166.7458 -14.82686, 1…
#>  8 Uzbekistan              UZB    UZ     2014… 5.66e-5 (((70.94678 42.24868, 70…
#>  9 Uruguay                 URY    UY     2014… 1.62e-3 (((-53.37061 -33.74219, …
#> 10 Federated States of Mi… FSM    FSM    2014… 5.48e-3 (((162.9832 5.325732, 16…
#> # ℹ 231 more rows
```

``` r
ggplot(result) +
  geom_sf(aes(fill = .linked * 1000)) +
  scale_fill_viridis_c(transform = "log10", labels = \(x) sprintf("%g", x)) +
  theme_void() +
  labs(
    title = "Total precipitation in August 2014",
    subtitle = "Averaged across countries",
    fill = "Average total precipitation [in mm]"
  ) +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(2, "cm")
  )
```

<img src="man/figures/README-visualize_example2-1.png" width="100%" />

## Parallel processing

`gxc` follows the parallel computing paradigm of the `future` package.
By default, this is disabled and the data will be processed through a
standard sequential pipeline. However, users can enable parallel
processing in all major functions (`parallel = TRUE`). This can
significantly increase execution time of processes which use large
datasets. In our functions, parallel computing becomes especially
relevant when observations are linked with EO data based on varying
focal time periods. At the same time, setting up a parallel plan and
chunk-based processing generates an overhead which could lead to
performance decreases compared to sequential approaches. This is
especially true for smaller datasets with narrower spatial extent and
fewer observations. Check out our [performance
website](https://denabel.github.io/gxc_pages/performance.html) to find
out, whether it makes sense to enable parallel processing for your
dataset.

If `parallel=TRUE`, data processing is performed by pre-chunking input
data. The chunk sizes can be varied with `chunk_size=`. The default is
set to `50`.

## Contributing

We welcome all contributions! Please review our [contribution
guide](./CONTRIBUTING.md) and [code of conduct](./CODE_OF_CONDUCT.md)
before contributing.

## Getting in touch

If you encounter a bug, have usage questions, or want to share ideas to
make `gxc` better, feel free to file an
[issue](https://github.com/denabel/gxc/issues) or contact us directly:

Dennis Abel (<dennis.abel@gesis.org>)

Stefan Jünger (<stefan.juenger@gesis.org>).

## Citation

To cite `gxc` in publications use:

> Abel D, Jünger S (2025). gxc: Easy Access to Earth Observation Data. R
> package version 0.1.0, <https://github.com/denabel/gxc>.

Or in BibTeX:

    @manual{abel2025gxc,
      author = {Abel, Daniel and Jünger, Sebastian},
      title  = {{gxc: Easy Access to Earth Observation Data}},
      year   = {2025},
      note   = {R package version 0.1.0},
      url    = {https://github.com/denabel/gxc}
    }

## Disclaimer

Access to data from [Copernicus Climate Change
Service](https://cds.climate.copernicus.eu/), [Copernicus Atmosphere
Monitoring Service](https://ads.atmosphere.copernicus.eu/), and
[Copernicus Emergency Management
Service](https://ewds.climate.copernicus.eu/) requires a user-account
with the [European Center for Medium-Range Weather Forecasts
(ECMWF)](https://www.ecmwf.int/). Please ensure you follow their Terms
and Conditions.
