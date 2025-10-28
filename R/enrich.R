enrich <- function(.data,
                   ids = "id",
                   type = NULL,
                   country = NULL,
                   level = NULL,
                   verbose = TRUE,
                   ...) {
  if (!ids %in% names(.data)) {
    cli::cli_abort("Column {.field {ids}} could not be found in `.data`.")
  }

  if (is.null(type) || identical(type, "gadm")) {
    conv <- convert_to_iso3(.data[[ids]])
  } else {
    conv <- list(ids = .data[[ids]])
  }

  if (is.null(type)) {
    type <- guess_id_type(.data[[ids]], iso3 = conv$changed)
  }

  args <- switch(
    type,
    gadm = parse_gadm(conv$ids),
    nuts = parse_gadm(conv$ids),
    lau = parse_lau(conv$ids),
    ags = parse_lau(conv$ids),
    cli::cli_abort("Admin type {type} is not supported (yet).")
  )

  if (!is.null(level)) {
    args$level <- level
  }

  switch(
    type,
    gadm = {
      for (lvl in unique(args$level)) {
        gid_col <- paste0("GID_", lvl)
        gadm_data <-           sf::st_as_sf(geodata::gadm(
          country %||% args$country,
          level = lvl,
          path = tempdir(),
          quiet = verbose,
          ...
        ))[c(gid_col, "geometry")]

        .data[args$level == lvl, ] <- left_merge(
          .data[args$level == lvl, , drop = FALSE],
          gadm_data,
          by.x = ids,
          by.y = gid_col
        )
      }
    },
    nuts = {
      for (lvl in unique(args$level)) {
        .data[args$level == lvl, ] <- left_merge(
          .data[args$level == lvl, , drop = FALSE],
          giscoR::gisco_get_nuts(
            country = country %||% args$country,
            nuts_level = lvl,
            nuts_id = args$nuts_id,
            ...
          ),
          by.x = ids,
          by.y = "NUTS_ID"
        )
      }
    },
    lau = {
      .data <- left_merge(
        .data,
        giscoR::gisco_get_lau(
          country = country %||% args$country,
          gisco_id = args$gisco_id,
          ...
        ),
        by.x = ids,
        by.y = "GISCO_ID"
      )
    },
    ags = {
      for (lvl in unique(args$level)) {
        .data[args$level == lvl, ] <- left_merge(
          .data[args$level == lvl, , drop = FALSE],
          ffm::bkg_admin(
            level = lvl,
            ags == args$ags,
            ...
          ),
          by.x = ids,
          by.y = "ags"
        )
      }
    }
  )
}


#' Takes a character vector and, if at least 90% of ids can be identified as
#' valid country codes, converts them to ISO-3 country codes
#' @param ids Character vector
#' @noRd
convert_to_iso3 <- function(ids) {
  cc_guess <- countrycode::guess_field(ids, min_similarity = 90)
  if (nrow(cc_guess) && !identical(cc_guess$code[1], "iso3c")) {
    type <- cc_guess$code[1]
    prob <- cc_guess$percent_of_unique_matched[1]
    info("Detected {type} codes with {prob}% certainty.")
    ids <- countrycode::countrycode(ids, origin = type, destination = "iso3c")
    list(ids = ids, changed = TRUE)
  } else {
    list(ids = ids, changed = FALSE)
  }
}


guess_id_type <- function(ids, iso3 = FALSE) {
  if (iso3 || all(is_gadm(ids))) {
    type <- "gadm"
  } else if (is_short_inspire(ids)) {
    type <- "short_inspire"
  } else if (is_long_inspire(ids)) {
    type <- "long_inspire"
  } else if (all(is_nuts(ids))) {
    type <- "nuts"
  } else if (all(is_lau(ids))) {
    type <- "lau"
  } else {
    cli::cli_abort(c(
      "Could not automatically detect admin type.",
      "i" = "Please use the `type` argument to manually specify the type of admin identifiers."
    ))
  }

  info("No ID type specified, using {.val {type}} IDs.")
  type
}


is_gadm <- function(ids) {
  grepl("^([A-Z]{3})(\\.[0-9]{1,3}_[0-9]{1})?$", ids)
}


is_lau <- function(ids) {
  grepl("([A-Z]{2}_)?[0-9]+", ids)
}


is_nuts <- function(ids) {
  grepl("^[A-Z]{2}[A-Z0-9]{1,3}$", ids)
}


parse_gadm <- function(ids) {
  gadm <- utils::strcapture(
    "^([A-Z]{3})(\\.[0-9]{1,3}_[0-9]{1})?$",
    ids,
    proto = list(country = "", level = "")
  )
  gadm$level <- ifelse(nzchar(gadm$level), 1, 0)
  gadm
}


parse_nuts <- function(ids) {
  nuts <- utils::strcapture(
    "^([A-Z]{2})([A-Z0-9])?([A-Z0-9])?([A-Z0-9])?$",
    ids,
    proto = list(country = "", level1 = "", level2 = "", level3 = "")
  )
  nuts$level1 <- nzchar(nuts$level1)
  nuts$level2 <- nzchar(nuts$level2)
  nuts$level3 <- nzchar(nuts$level3)
  data.frame(
    country = nuts$country,
    level = nuts$level1 + nuts$level2 + nuts$level3,
    nuts_id = ids
  )
}


parse_lau <- function(ids) {
  lau <- utils::strcapture(
    "(([A-Z]{2})_)?[0-9]+",
    ids,
    proto = list(pre = "", country = "")
  )
  data.frame(country = lau$country, gisco_id = ids)
}


is_short_inspire <- function(ids) {
  grepl("[0-9]+k?mN[0-9]+E[0-9]+", ids)
}


is_long_inspire <- function(ids) {
  grepl("CRS[0-9]+RES[0-9]+mN[0-9]+E[0-9]+", ids)
}
