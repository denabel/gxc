#' Specify Days Based on Input Date and Criteria
#'
#' This function generates a sequence of days based on the provided date, 
#' a specified number of days to go back, and/or specific months.
#' It allows for flexible selection of days either by a range of days back 
#' from the given date or by specifying certain months.
#'
#' @param date A date object representing the reference date from which to 
#' calculate the days.
#' @param days_back Optional integer specifying how many days back to include 
#' from the reference date.
#' @param months Optional character vector specifying the months to consider 
#' (e.g., "2023-01" for January 2023). If the month string starts with "P", it 
#' indicates a previous year.
#'
#' @return A vector of date objects representing the specified days.
#'
#' @details 
#' The function checks for `days_back` and `months` parameters:
#' - If both are NULL, it returns the original date.
#' - If `days_back` is specified, it generates a sequence of days going back 
#'   from the given date.
#' - If `months` is specified, it calculates the start and end dates for the 
#'   specified months and returns all days within that range.
#'
#' @examples
#' # Example 1: Specify days going back 5 days from a given date
#' specify_days(as.Date("2024-01-15"), days_back = 5, months = NULL)
#' 
#' # Example 2: Specify days for January and February of 2024
#' specify_days(as.Date("2024-01-15"), days_back = NULL, months = c(1, 2))
#' 
#' # Example 3: Specify days without additional criteria, returning the original 
#' date
#' specify_days(as.Date("2024-01-15"), days_back = NULL, months = NULL)
#'
#' @export
specify_days <- function(date, days_back, months) {
  # If both 'days_back' and 'months' are NULL, return the original date
  if (is.null(days_back) && is.null(months)) {
    days <- date
  }
  
  # If 'days_back' is provided and 'months' is NULL, generate a sequence of 
  # dates going back from the specified 'date' for the number of days specified 
  # by 'days_back'
  if (!is.null(days_back) && is.null(months)) {
    # Create a sequence from (date - days_back) to date (inclusive)
    days <- date - days_back:0
  }
  
  # If 'days_back' is NULL and 'months' is provided, calculate the range of days
  if (is.null(days_back) && !is.null(months)) {
    # Determine the starting year based on the first month in the 'months' 
    # vector
    start_year <- 
      ifelse(
        # If the first month starts with "P", set the year to the previous year
        grepl("P", months[1]),  
        lubridate::year(date) - 1, 
        # Otherwise, use the current year
        lubridate::year(date)  
      )
    
    # Determine the ending year based on the last month in the 'months' vector
    end_year <-
      ifelse(
        # If the last month starts with "P", set the year to the previous year
        grepl("P", months[length(months)]),  
        lubridate::year(date) - 1, 
        # Otherwise, use the current year
        lubridate::year(date)  
      )
    
    # Extract the starting month number from the first month string in 'months'
    start_month <- stringr::str_extract(months[1], "[0-9]+")
    
    # Extract the ending month number from the last month string in 'months'
    end_month <- stringr::str_extract(months[length(months)], "[0-9]+")
    
    # Create the start date using the starting year and month
    start_date <- 
      lubridate::ym(paste0(start_year, "-", start_month)) |> 
      lubridate::floor_date('month')  # Floor the date to the start of the month
    
    # Create the end date using the ending year and month
    end_date <-
      lubridate::ym(paste0(end_year, "-", end_month)) |> 
      # Ceiling the date to the end of the month
      lubridate::ceiling_date('month') - 1  
    
    # Generate a sequence of dates from start_date to end_date (inclusive)
    days <- seq(start_date, end_date, by = 1)
  }
  
  # Return the generated sequence of days
  days
}

# create_unique_string ----
create_unique_string <- 
  function(what, months, date = NULL, days_back, years_back, reference_years, 
           reference_statistic, buffers = NULL, crs = NULL, only_year = FALSE
  ) {
    
    strings <- list()
    
    strings$what_string <- glue::glue("what={what}")
    
    if (!is.null(date)) {
      strings$date_string <- glue::glue("date={date}")
    }
    
    if (isTRUE(only_year)) {
      strings$date_string <- glue::glue("date={lubridate::year(date)}")
    }
    
    strings$days_back_string <-
      glue::glue(
        "days_back={ifelse(is.null(days_back), 'NULL', ",
        "days_back)}"
      )
    
    strings$months_string <-
      glue::glue(
        "months={ifelse(is.null(months), 'NULL', ",
        "paste0(months[1], '_to_', ",
        "months[length(months)]))}"
      )
    
    strings$years_back_string <-
      glue::glue(
        "years_back={ifelse(is.null(years_back), 'NULL', ",
        "years_back)}"
      )
    
    strings$reference_years_string <-
      glue::glue(
        "reference_years={ifelse(is.null(reference_years), ",
        "'NULL', paste0(reference_years[1], '_to_', ",
        "reference_years[length(reference_years)]))}"
      )
    
    if(!is.null(buffers)) {
      strings$buffers_string <- glue::glue("buffer={buffers}")
    }
    
    strings$reference_statistic_string <-
      glue::glue(
        "reference_statistic=",
        "{stringr::str_replace_all(deparse(reference_statistic)[3], ' ', '')}"
      )
    
    if (!is.null(crs)) {
      strings$crs_string <- glue::glue("crs={crs}")
    }
    
    do.call(paste, c(strings, sep = "&"))
  }

# download_and_wrangle ----
download_and_wrangle <- 
  function(year, file_name, folder_name, where_raw, days, months) {
    
    # in case the first month was in the previous year, make sure to download
    # this layer as well
    months_from_previous_year <- grepl("P_", months)
    
    if (!is.null(months) && any(months_from_previous_year) && 
        !all(months_from_previous_year)) {
      year <- c(year - 1, year)
    }
    
    if (!is.null(months) && all(months_from_previous_year)) {
      year <- year - 1
    }
    
    base_filename <- glue::glue("{file_name}_{year}_v5-0_de.nc")
    
    generate_days <- function(the_file) {
      the_file_year <- 
        terra::sources(the_file) |> 
        basename() |> 
        stringr::str_extract("[0-9]{4}")
      
      seq(
        lubridate::ymd(glue::glue("{the_file_year}-01-01")),
        lubridate::ymd(glue::glue("{the_file_year}-12-31")),
        by = 1
      )
    }
    
    year_replace_table <- 
      tibble::tibble(
        actual_year = stringr::str_extract(days, "[0-9]{4}") |> unique(),
        target_year = year
      )
    
    days_table <- 
      tibble::tibble(
        actual_date = days,
        actual_year = stringr::str_extract(days, "[0-9]{4}"),
        actual_days = stringr::str_extract(days, "[-][0-9]{2}-[0-9]{2}")
      )
    
    days_merging_table <-
      merge(days_table, year_replace_table, by = "actual_year")
    
    days2 <-
      paste0(days_merging_table$target_year, days_merging_table$actual_days)
    
    base_filename_plus_location <- glue::glue("{where_raw}/{base_filename}")
    
    base_filename_plus_location |> 
      purrr::imap(~{
        if (!file.exists(.x)) {
          file_to_download <-
            glue::glue(
              "https://opendata.dwd.de/climate_environment/CDC/grids_germany/",
              "daily/hyras_de/{folder_name}/{base_filename[[.y]]}"
            )
          
          # actual download of the file!
          download.file(
            file_to_download,
            mode = "wb",
            .x,
            quiet = TRUE
          )
        }
      })
    
    # load data and filter according to days specification above
    raster_file <-
      base_filename_plus_location |> 
      purrr::map(~terra::rast(.x)) |>
      purrr::map(~.x |> magrittr::set_names(generate_days(.x))) |> 
      purrr::reduce(c)
    
    layer_index <- match(days2, names(raster_file))
    
    raster_file[[names(raster_file)[layer_index]]]
  }

# download_and_wrangle_reference_layers ----
download_and_wrangle_reference_layers <- 
  function (
    years, years_back, reference_years, file_name, folder_name, where_raw,
    reference_statistic, tmp_raster, days, months
  )  {
    if (!is.null(years_back) || !is.null(reference_years)) {
      reference_rasters <-
        years[-1] |>
        purrr::map(
          ~download_and_wrangle(
            year = .x, 
            file_name = file_name, 
            folder_name = folder_name, 
            where_raw = where_raw,
            days = days,
            months = months
          )
        )
      
      # combine and derive reference_statistic
      reference_rasters <-
        do.call(c, reference_rasters) |> 
        terra::app(reference_statistic)
      
      tmp_raster <-
        terra::ifel(tmp_raster > reference_rasters, 1, 0) |>
        terra::app(sum)
    } else {
      mean(tmp_raster, na.rm = TRUE)
    }
  }

# create_dwd_weather_layer ----
create_dwd_weather_layer <- 
  function(
    what = c("air_temperature_mean", "air_temperature_max", "precipitation"), 
    date, 
    days_back = NULL, 
    months = NULL,
    years_back = NULL,
    reference_years = NULL, 
    reference_statistic = function(x) {mean(x, na.rm = TRUE)},
    final_name = "value",
    crs = 3035,
    where_raw,
    where_processed,
    cache = TRUE
  ) {
    
    if (isTRUE(cache)) {
      dir.create(where_processed, showWarnings = FALSE)
    }
    
    if (!is.null(years_back) && !is.null(reference_years)) {
      stop("Define either the years_back or the reference_years argument!")
    }
    
    # build request ---
    # adjust folder and file names according to the DWD website
    if (what == "air_temperature_mean") {
      folder_name <- "air_temperature_mean"
      file_name   <- "tas_hyras_5"
    }
    if (what == "air_temperature_max") {
      folder_name <- "air_temperature_max"
      file_name   <- "tasmax_hyras_5"
    }
    if (what == "precipitation") {
      folder_name <- "precipitation"
      file_name   <- "pr_hyras_1"
    }
    
    # adjust date specifications ---
    focal_year <- lubridate::year(date)
    date       <- lubridate::date(date)
    
    # days according specific date or months in the past
    days <- specify_days(date = date, days_back = days_back, months = months)
    
    # define how many years (i.e., different raster layers) shall be downloaded
    if (is.null(years_back) && is.null(reference_years)) {
      years <- focal_year
    }
    if (!is.null(years_back) && is.null(reference_years)) {
      years <- 
        c(focal_year, sapply(1:years_back, function (i) focal_year - i))
    }
    if (is.null(years_back) && !is.null(reference_years)) {
      years <- c(focal_year, reference_years)
    }
    
    # unique filename for caching ---
    only_year <- ifelse(is.null(months), FALSE, TRUE)
    
    unique_filename <- 
      create_unique_string(
        what = what, months = months, date = date, days_back = days_back, 
        years_back = years_back, reference_years = reference_years, 
        reference_statistic = reference_statistic, crs = crs, 
        only_year = only_year
      ) |> 
      paste0(".tif")
    
    unique_filename_plus_location <- 
      glue::glue("{where_processed}/{unique_filename}")
    
    if (!file.exists(unique_filename_plus_location) || isFALSE(cache)) {
      
      # download and wrangle focal layer
      tmp_raster <-
        download_and_wrangle(
          year = years[1], 
          file_name = file_name, 
          folder_name = folder_name, 
          where_raw = where_raw,
          months = months,
          days = days
        )
      
      # download and wrangle reference layers if requested); otherwise build
      # mean from focal layer
      tmp_raster <- 
        download_and_wrangle_reference_layers(
          years = years, years_back = years_back, 
          reference_years = reference_years, file_name = file_name, 
          folder_name = folder_name, where_raw = where_raw,
          reference_statistic = reference_statistic, tmp_raster = tmp_raster, 
          days = days, months = months
        )
      
      # rename attribute of resulting layer
      names(tmp_raster) <- final_name
      
      # adjust crs
      tmp_raster <-
        tmp_raster |>
        terra::project(glue::glue("EPSG:{crs}"), method = "near")
      
      # store in caching folder
      terra::writeRaster(
        tmp_raster,
        unique_filename_plus_location,
        overwrite = TRUE
      )
    } else {
      tmp_raster <- terra::rast(unique_filename_plus_location)
    }
    
    # gc()
    
    tmp_raster
  }

# link_dwd_weather_single_date ----
link_dwd_weather_single_date <- function (
    data,
    date_variable,
    what,
    days_back,
    months,
    years_back,
    reference_years,
    buffers, 
    buffers_fixed = NULL,
    geometry_type,
    reference_statistic,
    crs,
    where_processed,
    where_raw,
    where_secret,
    cache
) {
  
  if (geometry_type == "POINT") {
    focal_row_geometries <-  
      buffers_fixed |> 
      lapply(function(i) {
        data |> 
          sf::st_buffer(i)
      }) |> 
      dplyr::bind_rows()
  } 
  
  if (grepl("POLY", geometry_type)) {
    focal_row_geometries <- data
  }
  
  # unique variable name for caching ---
  date <- data[[date_variable]][1]
  
  unique_filename <-
    create_unique_string(
      what = what, months = months, date = date, 
      days_back = days_back, years_back = years_back, 
      reference_years = reference_years, 
      reference_statistic = reference_statistic, buffers = buffers
    ) |>
    paste0(".qs")
  
  unique_variable_name <-
    create_unique_string(
      what = what, months = months, days_back = days_back,
      years_back = years_back, reference_years = reference_years,
      reference_statistic = reference_statistic, buffers = buffers
    )
  
  unique_variable_name_location <-
    glue::glue("{where_secret}/{unique_filename}")
  
  if(!file.exists(unique_variable_name_location)) {# || !is.null(where_secret)) {
    
    layer_to_link <-
      create_dwd_weather_layer(
        what = what,
        date = date,
        days_back = days_back,
        months = months,
        years_back = years_back,
        reference_years = reference_years,
        reference_statistic = reference_statistic,
        crs = crs,
        where_processed = where_processed,
        where_raw = where_raw,
        cache = cache
      )
    
    attributes <-
      # focal_row_geometries |>
      exactextractr::exact_extract(
        x = layer_to_link,
        y = focal_row_geometries,
        fun = "mean",
        progress = FALSE
      ) |> 
      #purrr::pmap_dfr(function(lfdn, intdate, geometry){
      #  single_geometry <- terra::vect(geometry)
      
      #  clip_1 <- terra::crop(layer_to_link, single_geometry)
      #  clip_2 <- terra::mask(clip_1, single_geometry)
      #
      #  terra::extract(clip_2, single_geometry, mean, ID = FALSE)
      #}) |>
      tibble::as_tibble() |>
      magrittr::set_names(unique_variable_name)
    
    attributes <- dplyr::bind_cols(data, attributes)
    
    qs::qsave(
      attributes,
      glue::glue("{where_secret}/{unique_filename}")
    )
    
    attributes
  } else {
    qs::qread(glue::glue("{where_secret}/{unique_filename}"))
  }
}
