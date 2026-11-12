#' gxc: Easy Access to Earth Observation Data
#'
#' Links social science data, such as geocoded survey responses, to Earth
#' observation indicators. Weather and climate indicators are retrieved from
#' the Copernicus Climate Data Store (ERA5, ERA5-Land) and the German Weather
#' Service (DWD) and extracted for each observation's location, date and
#' optional spatial buffer, optionally compared against a historical baseline.
#'
#' @section Main functions:
#' - [link_daily()]: link observations to daily indicators.
#' - [link_monthly()]: link observations to monthly indicators.
#' - [lookup()]: show which catalogues provide an indicator, with units and
#'   resolution.
#' - [gxc_indicators()]: list all available indicators.
#' - [set_api_key()]: store the API key for the Copernicus Climate Data
#'   Store.
#'
#' Both linking functions accept `sf` objects (points or polygons) and
#' `SpatRaster` objects.
#'
#' @importFrom stats median sd
#' @importFrom utils capture.output download.file
#' @keywords internal
"_PACKAGE"
