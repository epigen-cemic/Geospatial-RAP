# =============================================================================
# Configuration helpers
# -----------------------------------------------------------------------------
# Shared helpers for reading app-level settings from config.json. These helpers
# are intentionally small so both standalone tools can use the same behaviour.
# =============================================================================

#' Get a scalar value from config.json
#'
#' @description
#' Returns a value from the parsed configuration list, falling back to a default
#' when the key is absent or empty.
#'
#' @param config List created by `jsonlite::fromJSON()`.
#' @param key Character scalar. Name of the configuration entry to read.
#' @param default Value returned when the entry is missing or empty.
#'
#' @return The configured value or `default`.
#' @keywords internal
rap_config_value <- function(config, key, default = NULL) {
  value <- config[[key]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value
}

#' List configured countries
#'
#' @description
#' Returns the names of configuration blocks that represent countries. App-level
#' keys such as `is_offline`, `active_country`, and `target_var` are excluded.
#'
#' @param config Parsed configuration list.
#'
#' @return Character vector of country keys available in the configuration.
#' @keywords internal
rap_available_countries <- function(config) {
  reserved <- c("is_offline", "active_country", "default_country", "default_level", "default_year", "target_var", "geospatial", "Global")
  candidates <- setdiff(names(config), reserved)
  candidates[vapply(config[candidates], is.list, logical(1))]
}

#' Resolve active countries from config.json
#'
#' @description
#' Normalises the `active_country` setting to a character vector and removes
#' entries that do not exist as country blocks in the configuration.
#'
#' @param config Parsed configuration list.
#' @param default_country Character scalar used when no valid country is set.
#'
#' @return Character vector with one or more valid country keys.
#' @keywords internal
rap_active_countries <- function(config, default_country = NULL) {
  if (is.null(default_country)) {
    default_country <- rap_config_value(config, "default_country", "Argentina")
  }

  configured <- rap_config_value(config, "active_country", default_country)
  configured <- as.character(unlist(configured, use.names = FALSE))
  configured <- configured[configured %in% rap_available_countries(config)]

  if (length(configured) == 0 && default_country %in% rap_available_countries(config)) {
    configured <- default_country
  }

  if (length(configured) == 0) {
    configured <- rap_available_countries(config)[1]
  }

  unique(configured)
}

#' Get the default active country
#'
#' @param config Parsed configuration list.
#' @param default_country Character scalar used when no valid country is set.
#'
#' @return Character scalar containing the first active country.
#' @keywords internal
rap_default_country <- function(config, default_country = NULL) {
  if (is.null(default_country)) {
    default_country <- rap_config_value(config, "default_country", "Argentina")
  }
  rap_active_countries(config, default_country)[1]
}

#' Should the UI show a country selector?
#'
#' @param config Parsed configuration list.
#'
#' @return Logical. `TRUE` when more than one active country is configured.
#' @keywords internal
rap_show_country_selector <- function(config) {
  length(rap_active_countries(config)) > 1
}

#' Build CUSUM geographic choices
#'
#' @description
#' Creates a named vector for the CUSUM geographic-level selector from the
#' selected country's `levels` entry.
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#'
#' @return Named character vector suitable for `selectInput()`.
#' @keywords internal
rap_cusum_geo_choices <- function(config, country) {
  levels <- config[[country]]$levels
  levels <- as.character(levels)
  names(levels) <- tools::toTitleCase(levels)
  levels
}

#' Build geospatial level choices
#'
#' @description
#' Creates a named vector for the geospatial selector using configured mapping
#' entries. Display labels prefer `ui_label`, then `layer_name`, then the key.
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param include_level1 Logical. If `FALSE`, only level2 and deeper are shown.
#'
#' @return Named character vector suitable for `selectInput()`.
#' @keywords internal
rap_geospatial_choices <- function(config, country, include_level1 = FALSE) {
  mappings <- config[[country]]$mapping
  if (is.null(mappings)) {
    return(character(0))
  }

  keys <- names(mappings)
  if (!include_level1) {
    keys <- keys[vapply(mappings[keys], function(x) {
      api_pop_mapping_column(x) %in% c("level2", "level3", "level4")
    }, logical(1))]
  }

  choices <- keys
  names(choices) <- vapply(keys, function(k) {
    cfg <- mappings[[k]]
    if (!is.null(cfg$ui_label)) return(cfg$ui_label)
    if (!is.null(cfg$layer_name)) return(cfg$layer_name)
    k
  }, character(1))

  choices
}

#' Get the configured Geopackage path for a country
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Character path to the configured Geopackage.
#' @keywords internal
rap_gpkg_path <- function(config, country, data_dir = "raw_data") {
  file_name <- config[[country]]$file_name
  if (is.null(file_name) || is.na(file_name) || !nzchar(file_name)) {
    return(NA_character_)
  }
  file.path(data_dir, file_name)
}

#' Check whether a country's Geopackage exists
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Logical indicating whether the configured Geopackage exists.
#' @keywords internal
rap_has_gpkg <- function(config, country, data_dir = "raw_data") {
  path <- rap_gpkg_path(config, country, data_dir)
  is.character(path) && length(path) == 1 && !is.na(path) && file.exists(path)
}

#' Build a user-facing missing-Geopackage message
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Character message explaining which file is missing.
#' @keywords internal
rap_missing_gpkg_message <- function(config, country, data_dir = "raw_data") {
  path <- rap_gpkg_path(config, country, data_dir)
  sprintf(
    "Geographic data are not available for '%s'. Expected Geopackage: %s. Please add the file or select another configured country.",
    country,
    path
  )
}


#' Get the configured default geospatial level
#'
#' @description
#' Returns the configured `default_level` when it exists for the selected country.
#' Otherwise, it falls back to the first available geospatial level choice.
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param include_level1 Logical. Passed to `rap_geospatial_choices()`.
#'
#' @return Character scalar containing a valid geospatial level key, or `NULL`.
#' @keywords internal
rap_default_geospatial_level <- function(config, country, include_level1 = FALSE) {
  choices <- rap_geospatial_choices(config, country, include_level1 = include_level1)
  if (length(choices) == 0) {
    return(NULL)
  }

  configured <- as.character(rap_config_value(config, "default_level", unname(choices)[1]))
  if (configured %in% unname(choices)) {
    return(configured)
  }

  unname(choices)[1]
}

#' Get the configured default year
#'
#' @param config Parsed configuration list.
#'
#' @return Integer year or `NULL` when no default year is configured.
#' @keywords internal
rap_default_year <- function(config) {
  value <- rap_config_value(config, "default_year", NULL)
  if (is.null(value) || is.na(value)) {
    return(NULL)
  }
  as.integer(value)
}

#' Get the configured geospatial rate multiplier
#'
#' @description
#' Returns the multiplier used to convert case counts and population into the
#' configured prevalence/rate variable. A value of 100000 means prevalence per
#' 100,000 population.
#'
#' @param config Parsed configuration list.
#' @param default Numeric default used when the setting is missing or invalid.
#'
#' @return Numeric scalar.
#' @keywords internal
rap_geospatial_rate_multiplier <- function(config, default = 100000) {
  value <- NULL
  if (!is.null(config$geospatial) && !is.null(config$geospatial$rate_multiplier)) {
    value <- config$geospatial$rate_multiplier
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1 || is.na(value) || value <= 0) {
    return(default)
  }
  value
}

#' Validate geospatial configuration and spatial files
#'
#' @description
#' Performs a startup pre-flight check for each active country. It reports missing
#' configuration fields, missing Geopackage files, missing layers, and missing
#' configured spatial identifier, parent, or name columns. The check is designed
#' to warn users clearly without preventing non-spatial parts of the app from
#' loading.
#'
#' @param config Parsed configuration list.
#' @param countries Character vector of active country keys.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return A named list. Each country contains `errors` and `warnings` character vectors.
#' @keywords internal
rap_geospatial_preflight <- function(config, countries = rap_active_countries(config), data_dir = "raw_data") {
  results <- stats::setNames(vector("list", length(countries)), countries)

  for (country in countries) {
    errors <- character(0)
    warnings <- character(0)
    cfg <- config[[country]]

    if (is.null(cfg) || !is.list(cfg)) {
      results[[country]] <- list(errors = sprintf("Country '%s' is not defined in config.json.", country), warnings = character(0))
      next
    }

    if (is.null(cfg$file_name) || !nzchar(as.character(cfg$file_name))) {
      errors <- c(errors, sprintf("Country '%s' is missing the required 'file_name' setting.", country))
      results[[country]] <- list(errors = errors, warnings = warnings)
      next
    }

    if (is.null(cfg$levels) || length(cfg$levels) == 0) {
      errors <- c(errors, sprintf("Country '%s' is missing configured geographic levels.", country))
    }

    if (is.null(cfg$mapping) || !is.list(cfg$mapping)) {
      errors <- c(errors, sprintf("Country '%s' is missing the 'mapping' section.", country))
      results[[country]] <- list(errors = errors, warnings = warnings)
      next
    }

    gpkg_path <- rap_gpkg_path(config, country, data_dir)
    if (is.na(gpkg_path) || !file.exists(gpkg_path)) {
      errors <- c(errors, rap_missing_gpkg_message(config, country, data_dir))
      results[[country]] <- list(errors = errors, warnings = warnings)
      next
    }

    available_layers <- tryCatch(sf::st_layers(gpkg_path)$name, error = function(e) {
      errors <<- c(errors, sprintf("Could not read layers from '%s': %s", gpkg_path, e$message))
      character(0)
    })

    for (level in names(cfg$mapping)) {
      level_cfg <- cfg$mapping[[level]]
      required_fields <- c("layer_name", "spatial_id")
      missing_fields <- required_fields[vapply(required_fields, function(x) {
        is.null(level_cfg[[x]]) || !nzchar(as.character(level_cfg[[x]]))
      }, logical(1))]

      if (length(missing_fields) > 0) {
        errors <- c(errors, sprintf(
          "Country '%s', level '%s' is missing required config field(s): %s.",
          country, level, paste(missing_fields, collapse = ", ")
        ))
        next
      }

      display_name_col <- NULL
      if (!is.null(level_cfg$name_col) && nzchar(as.character(level_cfg$name_col))) {
        display_name_col <- level_cfg$name_col
      } else if (!is.null(level_cfg$label_col) && nzchar(as.character(level_cfg$label_col))) {
        display_name_col <- level_cfg$label_col
      } else {
        warnings <- c(warnings, sprintf(
          "Country '%s', level '%s' has no configured 'name_col' or 'label_col'. Map labels may be unavailable.",
          country, level
        ))
      }

      if (!level_cfg$layer_name %in% available_layers) {
        errors <- c(errors, sprintf(
          "Country '%s' expects layer '%s' for level '%s', but that layer was not found in %s.",
          country, level_cfg$layer_name, level, gpkg_path
        ))
        next
      }

      layer_columns <- tryCatch({
        sql <- sprintf('SELECT * FROM "%s" LIMIT 1', level_cfg$layer_name)
        names(sf::st_read(gpkg_path, query = sql, quiet = TRUE))
      }, error = function(e) {
        errors <<- c(errors, sprintf(
          "Could not inspect layer '%s' for country '%s': %s",
          level_cfg$layer_name, country, e$message
        ))
        character(0)
      })

      if (length(layer_columns) == 0) {
        next
      }

      expected_columns <- c(level_cfg$spatial_id, display_name_col, level_cfg$parent_col)
      expected_columns <- expected_columns[!is.na(expected_columns) & nzchar(expected_columns)]
      missing_columns <- setdiff(expected_columns, layer_columns)

      if (length(missing_columns) > 0) {
        errors <- c(errors, sprintf(
          "Country '%s', layer '%s' is missing configured column(s): %s.",
          country, level_cfg$layer_name, paste(missing_columns, collapse = ", ")
        ))
      }
    }

    results[[country]] <- list(errors = unique(errors), warnings = unique(warnings))
  }

  results
}

#' Format pre-flight issues for display
#'
#' @param preflight_result List for a single country returned by `rap_geospatial_preflight()`.
#'
#' @return Character vector of user-facing messages.
#' @keywords internal
rap_format_preflight_messages <- function(preflight_result) {
  if (is.null(preflight_result)) {
    return(character(0))
  }
  c(preflight_result$errors, preflight_result$warnings)
}
