#' gxc: Easy Access to Earth Observation Data
#'
#' This package is part of the project "GESIS meets Copernicus".
#' It supports users of Earth observation data by offering easy
#' access and processing of data from Copernicus services and other
#' Earth observation sources.
#'
#' Version: 0.1.0
#'
#' @name gxc
#' @aliases gxc
#' @author
#'   Dennis Abel \email{dennis.abel@@gesis.org} and
#'   Stefan Jünger \email{stefan.juenger@@gesis.org}
#'
#' @details
#' This package provides six core functions to link your input data
#' with EO indicators. Depending on your data input format
#' (points, polygons, or raster) and the desired temporal resolution
#' of the required indicators (daily or monthly), you can choose from the
#' list of functions below:
#'
#' For Spatial Points (`point_link_`)
#'
#' - `point_link_daily`
#' - `point_link_monthly`
#'
#' For Polygons (`poly_link_`)
#'
#' - `poly_link_daily`
#' - `poly_link_monthly`
#'
#' For Gridded Data (`grid_link_`)
#'
#' - `grid_link_daily`
#' - `grid_link_monthly`
"_PACKAGE"
