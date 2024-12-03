
# gxc

<img src="man/figures/gxclogo_v1_bright.png" align="right" height="200"/>

Welcome to the package website of “GESIS meets Copernicus” (gxc).

For many researchers in the social sciences, Earth observation (EO) data
represents a black box. Social science researchers face many obstacles
in applying and using these data, resulting from 1) a lack of technical
expertise, 2) a lack of knowledge of data sources and how to access
them, 3) unfamiliarity with complex data formats, such as
high-resolution, longitudinal raster datacubes, and 4) lack of expertise
in integrating the data into existing social science datasets. GxC aims
to close the gap by creating an automated interface to EO data and
complementary resources for social science research.

The project’s core is creating an open-source tool to link time- and
space-sensitive social science datasets with data from Earth observation
programs. Detailed documentation and beginner-friendly tutorials
complement the tool to showcase the capability of our project. The
social science community is the main target group of our tool. At the
same time, Earth system science researchers may similarly profit from
integrated social science data. This project supports inter- and
transdisciplinary research which is often made difficult because of
technical, disciplinary, and organizational barriers. The project
emphasizes research data management (RDM) workflows based on FAIR and
Open Data principles. All code is written in the open-source software R
and is made available on this website.

## Relevant sources of Earth observation data

Europe’s Earth Observation programme is called
[Copernicus](https://www.copernicus.eu/en). It is funded and managed by
the European Commission and partners like the [European Space
Agency](https://www.esa.int/) (ESA) and the [European Organisation for
the Exploitation of Meteorological
Satellites](https://www.eumetsat.int/) (EUMETSAT). It has been
operational since 2014 and provides free access to a wealth of satellite
data from ESA’s “Sentinel” fleet. Copernicus combines data from
satellites, ground-based as well as air- and sea-borne sensors to track
the Earth system and provide this information largely free for all
customers.

The ESA describes Copernicus as the world’s most ambitious Earth
observation program, which will be further expanded in the coming years.
On the [Copernicus homepage](https://www.copernicus.eu/en/access-data.),
the daily data collection is estimated at 12 terabytes. Given the
complexity of issues, Copernicus has separated its services for public
usage along several thematic areas:

- **Atmosphere**: [Copernicus Atmosphere Monitoring
  Service](https://atmosphere.copernicus.eu/) (CAMS)
- **Marine**: [Copernicus Marine Service](https://marine.copernicus.eu/)
  (CMEMS)
- **Land**: [Copernicus Land Monitoring
  Service](https://land.copernicus.eu/en) (CLMS)
- **Climate change**: [Copernicus Climate Change
  Service](https://climate.copernicus.eu/) (C3S)
- **Emergency**: [Copernicus Emergency Management
  Service](https://emergency.copernicus.eu/) (CEMS).

<img src="man/figures/copernicus_services.png" align="center" height="400"/>

Source: [Copernicus infrastructure and data
services](https://www.copernicus.eu/en/accessing-data-where-and-how/conventional-data-access-hubs)

This project focuses on the data provided by the Copernicus programme.
However, this is not the only relevant source of EO data which you can
consider for your projects. The US equivalent, for example, is based on
the [Landsat satellite programme](https://landsat.gsfc.nasa.gov/), which
is jointly operated by [NASA](https://www.nasa.gov/) and the [US
Geological Survey](https://www.usgs.gov/landsat-missions) (USGS).
[Google’s Earth Engine Cloud Computing
Platform](https://developers.google.com/earth-engine/datasets/catalog)
catalogs an extensive selection of additional data sets from various
sources.

## Package features

The unique feature of the tool should be the possibility of carrying out
both geographically and temporally high-resolution queries of data from
Copernicus and other Earth observation data sources, which at the same
time function efficiently on simple workstations albeit large amounts of
data. Our tested workflow development has identified five major levers:
indicator type, indicator intensity, focal time period, baseline time
period, and spatial buffer. Flexibility on these five attributes should
be maximized for users. The tool also offers the functionality to
automatically derive spatio-temporal links with other georeferenced data
(e.g., surveys, digital behavioral data).

Users should benefit from the core variables integrated into the
interface for social research. Preparatory work is currently being
carried out to select appropriate indicators from the corpus of
indicators offered by data providers and conceptualize the data
integration logic. Furthermore, in exchange with users and other
stakeholders, we compile data products from EOD that are particularly
relevant for social scientists. Examples include data on local air
quality and pollutants, extreme weather events, or land use changes. The
main data providers will be the Copernicus Monitoring Services on
Climate Change, Atmosphere, and Land.

<img src="man/figures/attribution_tree.png" align="center" height="400"/>

Major attributes for indicator specification. Source: Abel and Jünger
2024

GESIS strongly supports the FAIR data principles and Open Data. The
selection of R as the programming language for the tool supports
open-source infrastructure development and shareability, as well as
quality control via online repositories. Publishing the R scripts for
data management and analysis ensures the reproducibility of all research
steps.

## Example: Global temperature

For ERA5 and polygons

### Package setup

``` r
# Install and load required packages
required_packages <- c("devtools", "keyring", "rnaturalearth", "sf", "tidyverse")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)
```

    ## Lade nötiges Paket: usethis

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    ## [[1]]
    ## [1] "devtools"  "usethis"   "stats"     "graphics"  "grDevices" "utils"    
    ## [7] "datasets"  "methods"   "base"     
    ## 
    ## [[2]]
    ##  [1] "keyring"   "devtools"  "usethis"   "stats"     "graphics"  "grDevices"
    ##  [7] "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[3]]
    ##  [1] "rnaturalearth" "keyring"       "devtools"      "usethis"      
    ##  [5] "stats"         "graphics"      "grDevices"     "utils"        
    ##  [9] "datasets"      "methods"       "base"         
    ## 
    ## [[4]]
    ##  [1] "sf"            "rnaturalearth" "keyring"       "devtools"     
    ##  [5] "usethis"       "stats"         "graphics"      "grDevices"    
    ##  [9] "utils"         "datasets"      "methods"       "base"         
    ## 
    ## [[5]]
    ##  [1] "lubridate"     "forcats"       "stringr"       "dplyr"        
    ##  [5] "purrr"         "readr"         "tidyr"         "tibble"       
    ##  [9] "ggplot2"       "tidyverse"     "sf"            "rnaturalearth"
    ## [13] "keyring"       "devtools"      "usethis"       "stats"        
    ## [17] "graphics"      "grDevices"     "utils"         "datasets"     
    ## [21] "methods"       "base"

``` r
# Load gxc package (locally for now)
devtools::load_all()
```

    ## ℹ Loading gxc

### Load a world map

``` r
# Download world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
st_geometry(world)
```

    ## Geometry set for 242 features 
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -180 ymin: -89.99893 xmax: 180 ymax: 83.59961
    ## Geodetic CRS:  WGS 84
    ## First 5 geometries:

    ## MULTIPOLYGON (((31.28789 -22.40205, 31.19727 -2...

    ## MULTIPOLYGON (((30.39609 -15.64307, 30.25068 -1...

    ## MULTIPOLYGON (((53.08564 16.64839, 52.58145 16....

    ## MULTIPOLYGON (((104.064 10.39082, 104.083 10.34...

    ## MULTIPOLYGON (((-60.82119 9.138379, -60.94141 9...

``` r
# Subset to relevant variables
world <- world |> 
  select(admin, iso_a3, postal, geometry)

# Create fixed date-variable
world$date_raw <- "08-2014"

# Plot world map
plot(world[1])
```

![](README_files/figure-gfm/map-1.png)<!-- -->

### Store your API-key

# `{r key} # api_key <- Sys.getenv("WF_API_KEY") #  # keyring::key_set_with_value(service = "wf_api_key", password = api_key) #  #`

# 

# 

# \### Run poly_link-function

# 

# `{r poly_link} # dataset_out <- poly_link( #   indicator = "2m_temperature", #   data = world, #   date_var = "date_raw", #   time_span = 0, #   time_lag = 0, #   baseline = FALSE, #   min_year = "1989", #   max_year = "1990", #   order = "my", #   path = "./data/raw") #  #`

# 

# \### Explore the extended dataset

# 

# `{r dataset} # head(dataset_out) #  #`

# 

# `{r visualize} # ggplot(data = dataset_out) + #   geom_sf(aes(fill = focal_value)) + #   scale_fill_viridis_c() + #   theme_minimal() + #   labs( #     title = "Mean temperature (K) in August 2014", #     subtitle = "Averaged across countries", #     fill = "Temperature (K)" #   ) #  #`
