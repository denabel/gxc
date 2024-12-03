#' Link DWD Weather Data to Geospatial Data with Specific Observation Dates
#'
#' This function links geospatial data including specific observation dates with 
#' weather data from the German Weather Service (DWD).
#'
#' @param data A spatial dataset (sf object) containing geometries and 
#' attributes.
#' @param date_variable A character string specifying the date-related variable 
#' in the dataset to use for linking the weather data.
#' @param what A character vector specifying the weather variables to extract 
#' (default: air temperature mean, air temperature max, and precipitation).
#' @param days_back Optional integer specifying how many days back to consider 
#' for weather data.
#' @param months Optional vector specifying the months to consider.
#' @param years_back Optional integer specifying how many years back to consider 
#' for weather data.
#' @param reference_years Optional vector specifying the reference years to 
#' consider for calculating statistics.
#' @param buffers A numeric vector specifying buffer distances for spatial 
#' extraction.
#' @param reference_statistic A function to compute the reference statistic 
#' (default: mean with `na.rm = TRUE`).
#' @param crs The coordinate reference system for the output 
#' (default: EPSG 3035).
#' @param where_processed Optional directory path for storing processed data.
#' @param where_raw Optional directory path for storing raw data.
#' @param where_secret Optional directory path for secret data storage (created 
#' if it does not exist).
#' @param cache Logical; whether to cache results (default: TRUE).
#'
#' @return A data frame or tibble with the linked weather data.
#'
#' @details 
#' This function splits the input data by the specified date variable and links 
#' it to weather data from the DWD for each group.
#' It handles spatial data with different geometry types (e.g., POINT geometries 
#' with buffer adjustments) and supports a caching mechanism for efficient 
#' processing.
#'
#' @examples
#' \dontrun{
#' result <- 
#'   gxc_link_dwd(
#'     data = my_sf_data, 
#'     date_variable = "date_column", 
#'     what = "air_temperature_mean", 
#'     days_back = 5
#'   )
#' }
#'
#' @export
gxc_link_dwd <-
  function(
    data,
    date_variable, 
    what  = 
      c("air_temperature_mean", "air_temperature_max", "precipitation"),
    days_back = NULL,
    months = NULL,
    years_back = NULL,
    reference_years = NULL,
    buffers = NULL,
    reference_statistic = function(x) {mean(x, na.rm = TRUE)},
    crs = 3035,
    where_processed = NULL,
    where_raw = NULL,
    where_secret = NULL,
    cache = TRUE
  ) {
    
    # Checks if the 'where_secret' path is provided and creates the directory if 
    # it doesn't exist.
    if(!is.null(where_secret)) {
      dir.create(where_secret, showWarnings = FALSE)
    }
    
    # Determines the geometry type of the dataset (e.g., POINT, POLYGON, etc.)
    geometry_type <- sf::st_geometry_type(data, by_geometry = FALSE)
    
    # Buffers of size 0 cannot be computed in 'exactextractr::exact_extract()'.
    # If the geometry type is 'POINT', buffers of size 0 are replaced with 0.1 
    # to avoid calculation issues.
    if (geometry_type == "POINT") {
      buffers_fixed <- replace(buffers, buffers == 0 || is.null(buffers), .1)
    }
    
    # Splits the dataset into groups based on the 'date_variable'. 
    # Each group contains rows associated with a specific date.
    date_splitted_data <-
      data |>  
      dplyr::group_split(!!rlang::sym(date_variable))
    
    # For each group of data, apply the 'link_dwd_weather_single_date' function
    # This function links the spatial data to the DWD weather data
    linked_data <-
      date_splitted_data |> 
      purrr::map(~{
        link_dwd_weather_single_date(
          # Pass the split group data
          data = .x,  
          # The date variable used for splitting
          date_variable = date_variable,  
          # Weather variables to extract (e.g., temperature, precipitation)
          what = what,  
          # Number of days to look back for weather data
          days_back = days_back,  
          # Specific months to consider
          months = months,  
          # Years to look back
          years_back = years_back,  
          # Reference years for statistical calculations
          reference_years = reference_years,  
          # Buffer distances for spatial extraction
          buffers = buffers,  
          # Fixed buffer distances to handle 0-buffer cases
          buffers_fixed = buffers_fixed,  
          # Geometry type of the spatial data
          geometry_type = geometry_type,  
          # Function to calculate statistics (default: mean)
          reference_statistic = reference_statistic,  
          # Coordinate Reference System
          crs = crs,  
          # Path for storing processed data
          where_processed = where_processed,  
          # Path for storing raw data
          where_raw = where_raw,  
          # Path for storing sensitive data
          where_secret = where_secret,  
          # Whether to cache results
          cache = cache 
        )
      }) |>
      # Combine the results from all groups into a single dataset
      dplyr::bind_rows()  
    
    # Run garbage collection to free up memory after processing
    # gc()
    
    # Return the final linked dataset with weather data
    linked_data
  }
