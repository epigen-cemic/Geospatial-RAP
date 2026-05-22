# =============================================================================
# Shared API-POP input helpers
# -----------------------------------------------------------------------------
# This file is intentionally shared by the CUSUM RAP and Geospatial RAP tools.
# Keep the logic identical across tools when changing CSV import/validation.
# =============================================================================

#' Read an uploaded API-POP CSV file
#'
#' @description
#' Reads a CSV file using either comma or semicolon delimiters based on the first line.
#'
#' @param path Character scalar. Path to the uploaded CSV file.
#'
#' @return A tibble containing the raw file contents.
#' @keywords internal
api_pop_read_file <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)

  if (grepl(";", first_line)) {
    readr::read_csv2(path, show_col_types = FALSE)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
}

#' Create a preparation-log entry
#'
#' @param ... Values pasted together into one log message.
#'
#' @return Character scalar.
#' @keywords internal
api_pop_log <- function(...) {
  paste0(...)
}

#' Get data-preparation log entries
#'
#' @param df Data frame with an optional `prep_log` attribute.
#'
#' @return Character vector of log entries.
#' @keywords internal
api_pop_get_log <- function(df) {
  log <- attr(df, "prep_log", exact = TRUE)
  if (is.null(log)) character(0) else log
}

#' Set data-preparation log entries
#'
#' @param df Data frame to annotate.
#' @param log Character vector of log entries.
#'
#' @return The input data frame with a `prep_log` attribute.
#' @keywords internal
api_pop_set_log <- function(df, log) {
  attr(df, "prep_log") <- unique(as.character(log))
  df
}

#' Append a data-preparation log entry
#'
#' @param df Data frame with an optional `prep_log` attribute.
#' @param ... Values pasted together into one log message.
#'
#' @return The input data frame with an updated `prep_log` attribute.
#' @keywords internal
api_pop_add_log <- function(df, ...) {
  api_pop_set_log(df, c(api_pop_get_log(df), api_pop_log(...)))
}

#' Resolve the API-POP column configured for a level
#'
#' @param mapping_entry List containing `csv_column` or `db_column`.
#'
#' @return Character scalar with the standardized lower-case column name, or `NULL`.
#' @keywords internal
api_pop_mapping_column <- function(mapping_entry) {
  if (!is.null(mapping_entry$csv_column)) return(tolower(mapping_entry$csv_column))
  if (!is.null(mapping_entry$db_column)) return(tolower(mapping_entry$db_column))
  NULL
}

#' Find a mapping entry by standardized column
#'
#' @param config Parsed configuration list.
#' @param active_country Character scalar. Country key to read.
#' @param generic_col Character scalar such as `level1`, `level2`, or `level3`.
#'
#' @return A mapping entry list, or `NULL` when no match is found.
#' @keywords internal
api_pop_get_mapping_by_column <- function(config, active_country, generic_col) {
  mappings <- config[[active_country]]$mapping
  if (is.null(mappings)) return(NULL)
  hits <- Filter(function(x) identical(api_pop_mapping_column(x), generic_col), mappings)
  if (length(hits) == 0) NULL else hits[[1]]
}

#' Resolve the configured geography-name column
#'
#' @description
#' Returns `name_col` when available, with `label_col` retained as a backward-compatible fallback.
#'
#' @param mapping_entry List containing the spatial-layer mapping.
#'
#' @return Character scalar naming the column used as the display label.
#' @keywords internal
api_pop_get_name_col <- function(mapping_entry) {
  if (!is.null(mapping_entry$name_col)) return(mapping_entry$name_col)
  if (!is.null(mapping_entry$label_col)) return(mapping_entry$label_col)
  if (!is.null(mapping_entry$layer_name)) return(mapping_entry$layer_name)
  NA_character_
}

#' Standardise API-POP column names and types
#'
#' @description
#' Normalises common column aliases, converts core fields to the correct types,
#' applies configured code padding where needed, and records a preparation log.
#'
#' @param df Data frame read from uploaded CSV files.
#' @param config Optional parsed configuration list.
#' @param active_country Optional country key used to read geography settings.
#' @param require_population Logical. Whether population is required.
#'
#' @return A standardised data frame with a `prep_log` attribute.
#' @keywords internal
api_pop_standardise_columns <- function(df,
                                        config = NULL,
                                        active_country = NULL,
                                        require_population = FALSE) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Uploaded file is empty or could not be read.")
  }

  original_names <- names(df)
  names(df) <- tolower(trimws(names(df)))
  log <- character(0)

  if (!identical(original_names, names(df))) {
    log <- c(log, "Column names were trimmed and converted to lower case.")
  }

  aliases <- list(
    country = c("country", "pais", "país"),
    level1 = c("level1", "level_1", "province", "provincia", "region", "región", "rgn", "rgn22cd"),
    level2 = c("level2", "level_2", "department", "departamento", "district", "lad", "lad22cd", "lad23cd"),
    level3 = c("level3", "level_3", "fraction", "fraccion", "fracción", "subdistrict", "msoa", "msoa21cd"),
    year = c("year", "anio", "año", "ano", "epiyear"),
    week = c("week", "semana", "epiweek", "epi_week", "se"),
    n_cases = c("n_cases", "cases", "case_count", "count", "n", "events", "suspected_cases", "consultations"),
    population = c("population", "n_population", "pop", "denominator", "population_total")
  )

  for (std in names(aliases)) {
    if (std %in% names(df)) next
    hits <- aliases[[std]][aliases[[std]] %in% names(df)]
    if (length(hits) > 0) {
      names(df)[names(df) == hits[1]] <- std
      log <- c(log, sprintf("Column '%s' was standardised to '%s'.", hits[1], std))
    }
  }

  # Also honour country-specific config db_column/csv_column names when present.
  if (!is.null(config) && !is.null(active_country) && !is.null(config[[active_country]]$mapping)) {
    mappings <- config[[active_country]]$mapping
    for (level_key in names(mappings)) {
      csv_col <- api_pop_mapping_column(mappings[[level_key]])
      if (!is.null(csv_col) && csv_col %in% names(df)) {
        generic_col <- csv_col
        if (grepl("^level[0-9]+$", csv_col)) generic_col <- csv_col
        if (!generic_col %in% names(df)) {
          names(df)[names(df) == csv_col] <- generic_col
          log <- c(log, sprintf("Configured geography column '%s' was standardised to '%s'.", csv_col, generic_col))
        }
      }
    }
  }

  required <- c("year", "week", "n_cases")
  if (require_population) required <- c(required, "population")

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required column(s): %s. Accepted case-count aliases include n_cases, cases, case_count, count and n. Accepted population aliases include population, pop and denominator.",
      paste(missing, collapse = ", ")
    ))
  }

  if ("year" %in% names(df)) {
    before_na <- sum(is.na(df$year))
    df$year <- suppressWarnings(as.integer(df$year))
    after_na <- sum(is.na(df$year))
    log <- c(log, "Year was converted to integer.")
    if (after_na > before_na) log <- c(log, "Some year values could not be converted to integer and became NA.")
  }

  if ("week" %in% names(df)) {
    before_na <- sum(is.na(df$week))
    df$week <- suppressWarnings(as.integer(df$week))
    after_na <- sum(is.na(df$week))
    log <- c(log, "Week was converted to integer.")
    if (after_na > before_na) log <- c(log, "Some week values could not be converted to integer and became NA.")
  }

  if ("n_cases" %in% names(df)) {
    before_na <- sum(is.na(df$n_cases))
    df$n_cases <- suppressWarnings(as.numeric(df$n_cases))
    after_na <- sum(is.na(df$n_cases))
    log <- c(log, "Case counts were converted to numeric.")
    if (after_na > before_na) log <- c(log, "Some case-count values could not be converted to numeric and became NA.")
  }

  if ("population" %in% names(df)) {
    before_na <- sum(is.na(df$population))
    df$population <- suppressWarnings(as.numeric(df$population))
    after_na <- sum(is.na(df$population))
    log <- c(log, "Population was converted to numeric.")
    if (after_na > before_na) log <- c(log, "Some population values could not be converted to numeric and became NA.")
  }

  # Trim geographic identifiers and apply configured zero-padding only when a
  # code_length exists. Alphanumeric codes such as England LAD/MSOA do not need padding.
  for (generic_col in c("country", "level1", "level2", "level3", "level4")) {
    if (generic_col %in% names(df)) {
      df[[generic_col]] <- stringr::str_trim(as.character(df[[generic_col]]))

      mapping_entry <- NULL
      if (!is.null(config) && !is.null(active_country)) {
        mapping_entry <- api_pop_get_mapping_by_column(config, active_country, generic_col)
      }

      if (!is.null(mapping_entry) && !is.null(mapping_entry$code_length)) {
        width <- as.integer(mapping_entry$code_length)
        df[[generic_col]] <- stringr::str_pad(df[[generic_col]], width = width, pad = "0")
        log <- c(log, sprintf("Column '%s' was padded to width %s using config.json.", generic_col, width))
      }
    }
  }

  bad_time <- is.na(df$year) | is.na(df$week)
  if (any(bad_time)) {
    log <- c(log, sprintf("%s row(s) have invalid year/week values and may be excluded during analysis.", sum(bad_time)))
  }

  api_pop_set_log(df, log)
}

#' Validate standardised API-POP data
#'
#' @param df Data frame returned by `api_pop_standardise_columns()`.
#' @param require_population Logical. Whether population is required.
#' @param target_col Optional character scalar naming a required geography column.
#'
#' @return `NULL` when valid; otherwise a user-facing error message.
#' @keywords internal
api_pop_validate_data <- function(df,
                                  require_population = FALSE,
                                  target_col = NULL) {
  required <- c("year", "week", "n_cases")
  if (!is.null(target_col)) required <- c(required, target_col)
  if (require_population) required <- c(required, "population")

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(sprintf("Missing required column(s): %s.", paste(missing, collapse = ", ")))
  }

  if (any(is.na(df$year)) || any(is.na(df$week))) {
    return("Year/week columns contain values that could not be converted to numbers.")
  }

  if (any(is.na(df$n_cases))) {
    return("Case/count column contains values that could not be converted to numbers.")
  }

  if (require_population && any(is.na(df$population))) {
    return("Population column contains values that could not be converted to numbers.")
  }

  NULL
}

#' Combine and standardise uploaded API-POP files
#'
#' @description
#' Reads one or more uploaded CSV files, binds them together, standardises columns,
#' and stores a preparation log as an attribute on the returned data frame.
#'
#' @param file_paths Character vector of uploaded file paths.
#' @param config Optional parsed configuration list.
#' @param active_country Optional country key used to read geography settings.
#' @param require_population Logical. Whether population is required.
#'
#' @return A standardised combined data frame with a `prep_log` attribute.
#' @keywords internal
api_pop_combine_files <- function(file_paths,
                                  config = NULL,
                                  active_country = NULL,
                                  require_population = FALSE) {
  read_one <- function(path) {
    api_pop_read_file(path)
  }

  raw_df <- purrr::map_dfr(file_paths, read_one, .id = "file_index")
  raw_df$file_index <- suppressWarnings(as.numeric(raw_df$file_index))

  log <- sprintf("Read %s uploaded file(s) and combined them into %s row(s).", length(file_paths), nrow(raw_df))

  out <- api_pop_standardise_columns(
    raw_df,
    config = config,
    active_country = active_country,
    require_population = require_population
  )

  api_pop_set_log(out, c(log, api_pop_get_log(out)))
}
