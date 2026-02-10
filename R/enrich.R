enrich <- function(.data,
                   ids = "id",
                   type = NULL,
                   country = NULL,
                   level = NULL,
                   crs = 3035,
                   verbose = TRUE,
                   ...) {
  if (is.character(.data)) {
    .data <- data.frame(.data)
    names(.data) <- ids
  } else if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a dataframe.")
  }

  if (!ids %in% names(.data)) {
    cli::cli_abort("Column {.field {ids}} could not be found in `.data`.")
  }

  if (!is.null(type)) {
    if (length(type) != 1) {
      cli::cli_abort("Argument `type` must be of length 1.")
    }

    if (!type %in% supported_sources) {
      cli::cli_abort(c(
        "{type} is not (yet) a supported type of areal identifiers.",
        "i" = "Supported types include: {supported_sources}"
      ))
    }
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
    nuts = parse_nuts(conv$ids),
    lau = parse_lau(conv$ids),
    ags = parse_ags(conv$ids),
    postcode = {
      if (!"username" %in% ...names()) {
        cli::cli_abort(c(
          "If `type = \"postcode\"`, the `username` argument must be provided.",
          "i" = "You must be registered for the GeoNames ({.url https://www.geonames.org/export/web-services.html}) web service to query postcodes."
        ))
      }

      list()
    }
  )

  if (is.null(country) && "country" %in% names(args)) {
    country <- unique(args$country)
    info("Detected the following {cli::qty(level)}countr{?s/ies}: {country}")
  }

  if (is.null(level) && "level" %in% names(args)) {
    level <- unique(args$level)
    info("Detected the following {cli::qty(level)}level{?s}: {level}")
  }

  switch(
    type,
    gadm = {
      for (lvl in level) {
        gid_col <- paste0("GID_", lvl)
        reference <- sf::st_as_sf(geodata::gadm(
          country,
          level = lvl,
          path = tempdir(),
          quiet = !verbose,
          ...
        ))[c(gid_col, "geometry")]

        merged <- left_merge(
          .data[level == lvl, , drop = FALSE],
          reference,
          by.x = ids,
          by.y = gid_col
        )
        .data[level == lvl, names(merged)] <- merged
      }
    },
    nuts = {
      for (lvl in level) {
        reference <- giscoR::gisco_get_nuts(
          country = country,
          nuts_level = lvl,
          nuts_id = args$nuts_id,
          ...
        )

        merged <- left_merge(
          .data[level == lvl, , drop = FALSE],
          reference,
          by.x = ids,
          by.y = "NUTS_ID"
        )
        .data[level == lvl, names(merged)] <- merged
      }
    },
    lau = {
      reference <- giscoR::gisco_get_lau(
        country = country,
        gisco_id = args$gisco_id,
        ...
      )

      .data <- left_merge(
        .data,
        reference,
        by.x = ids,
        by.y = "GISCO_ID"
      )
    },
    ags = {
      for (lvl in level) {
        reference <- ffm::bkg_admin(
          level = lvl,
          ags == args$ags,
          ...
        )

        merged <- left_merge(
          .data[level == lvl, , drop = FALSE],
          reference,
          by.x = ids,
          by.y = "ags"
        )
        .data[level == lvl, names(merged)] <- merged
      }
    },
    postcode = {
      reference <- lapply(.data[[ids]], function(id) {
        args <- c(list(postalcode = id, maxRows = 1), ...)
        res <- suppressWarnings(do.call(geonames::GNpostalCodeSearch, args))
        if (is.null(res$lng) || is.null(res$lat)) {
          sf::st_point()
        } else {
          sf::st_point(as.numeric(c(res$lng, res$lat)))
        }
      })
      reference <- sf::st_sf(
        postcode = .data[[ids]],
        geometry = sf::st_as_sfc(reference),
        crs = 4326
      )

      .data <- left_merge(
        .data,
        reference,
        by.x = ids,
        by.y = "postcode"
      )
    }
  )

  out <- as_sf_tibble(
    .data,
    sf_column_name = "geometry",
    crs = sf::st_crs(reference)
  )
  sf::st_transform(out, crs)
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
  } else if (all(is_short_inspire(ids))) {
    type <- "short_inspire"
  } else if (all(is_long_inspire(ids))) {
    type <- "long_inspire"
  } else if (all(is_nuts(ids))) {
    type <- "nuts"
  } else if (all(is_lau(ids))) {
    type <- "lau"
  } else if (all(is_ags(ids))) {
    type <- "ags"
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
  grepl("^([A-Z]{3})(\\.[0-9]+){1,4}_[0-9]{1}?$", ids)
}


is_lau <- function(ids) {
  grepl("[A-Z]{2}_[0-9]+", ids)
}


is_nuts <- function(ids) {
  grepl("^[A-Z]{2}[A-Z0-9]{1,3}$", ids)
}


is_short_inspire <- function(ids) {
  grepl("[0-9]+k?mN[0-9]+E[0-9]+", ids)
}


is_long_inspire <- function(ids) {
  grepl("CRS[0-9]+RES[0-9]+mN[0-9]+E[0-9]+", ids)
}


is_ags <- function(ids) {
  grepl("^[0-1]([0-9]{1})?([0-9]{1})?([0-9]{2})?([0-9]{3})?", ids)
}


parse_gadm <- function(ids) {
  gadm <- utils::strcapture(
    "^([A-Z]{3})(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?_[0-9]{1}?$",
    ids,
    proto = list(country = "", level1 = "", level2 = "", level3 = "", level4 = "")
  )

  is_lvl <- startsWith(names(gadm), "level")
  gadm[is_lvl] <- lapply(gadm[is_lvl], nzchar)

  data.frame(
    country = gadm$country,
    level = do.call(psum, gadm[is_lvl])
  )
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
    "([A-Z]{2})_[0-9]+",
    ids,
    proto = list(country = "")
  )
  data.frame(country = lau$country, gisco_id = ids)
}


parse_ags <- function(ids) {
  ids_pad <- paste0(ids, strrep("0", 8 - nchar(ids)))
  ags_raw <- utils::strcapture(
    "([0-9]{2})([0-9]{1})([0-9]{2})([0-9]{3})",
    ids_pad,
    list(lan = 0, rbz = 0, krs = 0, gem = 0)
  )

  ags <- ags_raw
  is_city_state <- ags$lan %in% c(2, 11)
  for (i in seq_along(ags)) {
    ags[[i]] <- ags[[i]] > 0

    if (any(is_city_state)) {
      ags[[i]][is_city_state] <- TRUE
    }
  }

  # Include catch-all level "sta" that is selected when all other levels
  # are FALSE, i.e. 00000000 (Germany)
  ags <- cbind(sta = TRUE, ags)

  level <- unlist(.mapply(ags, MoreArgs = NULL, FUN = function(...) {
    names(which.max(rev(c(...))))
  }))

  # Assign the most common level to Berlin and Hamburg
  level[ags_raw$lan %in% c(2, 11)] <- names(which.max(table(level)))
  data.frame(country = "DEU", level = level, ags = ids)
}


supported_sources <- c(
  "gadm", "naturalearth", "inspire", "nuts", "ags", "postcode"
)
