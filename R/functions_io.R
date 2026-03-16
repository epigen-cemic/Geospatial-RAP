# Read uploaded CSV (handles comma and semicolon separators) --------------------
read_api_pop_output <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  if (grepl(";", first_line)) {
    readr::read_csv2(path, show_col_types = FALSE)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
}

# Standardise input column names -----------------------------------------------
standardise_input_columns <- function(df) {
  names(df) <- tolower(names(df))

  candidates <- list(
    n_cases    = c("n_cases", "cases", "count", "n"),
    population = c("population", "n_population", "pop", "denominator")
  )

  for (std in names(candidates)) {
    hits <- candidates[[std]][candidates[[std]] %in% names(df)]
    if (length(hits) == 0) stop(sprintf("Required column for '%s' not found.", std))
    if (hits[1] != std) names(df)[names(df) == hits[1]] <- std
  }

  required <- c("country", "level2", "level3", "year", "week", "n_cases", "population")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  df %>%
    dplyr::mutate(
      level2     = as.character(level2),
      level3     = as.character(level3),
      year       = as.integer(year),
      week       = as.integer(week),
      n_cases    = as.numeric(n_cases),
      population = as.numeric(population)
    )
}

combine_uploaded_data <- function(file_paths) {
  raw_df <- purrr::map_dfr(file_paths, read_api_pop_output, .id = "file_index")
  standardise_input_columns(raw_df)
}

# Temporal + hierarchical aggregation ------------------------------------------
# Join strategy:
#   department level -> aggregate by level2 (name) — robust, no code arithmetic needed.
#   fraction level   -> aggregate by level3 (cod_indec string).
#
# NOTE on population: constant per area across weeks, so we take first non-NA value.
prepare_geospatial_dataset <- function(df,
                                       selected_level  = c("department", "fraction"),
                                       selected_year,
                                       week_from,
                                       week_to,
                                       dept_code_nchar = 5) {
  selected_level <- match.arg(selected_level)

  df <- standardise_input_columns(df) %>%
    dplyr::filter(year == selected_year,
                  week >= week_from,
                  week <= week_to)

  if (nrow(df) == 0) return(tibble::tibble())

  first_pop <- function(x) { v <- x[!is.na(x)]; if (length(v) == 0) NA_real_ else v[1] }

  if (selected_level == "department") {
    # Join key: level2 name string matches gpkg 'nam' column
    weekly_df <- df %>%
      dplyr::group_by(level2, year, week) %>%
      dplyr::summarise(
        n_cases    = sum(n_cases, na.rm = TRUE),
        population = first_pop(population),
        .groups    = "drop"
      ) %>%
      dplyr::rename(analysis_unit_id    = level2,
                    analysis_unit_label = level2)

    # Restore label column (was consumed by rename — keep both)
    weekly_df <- weekly_df %>%
      dplyr::mutate(analysis_unit_label = analysis_unit_id)

  } else {
    # Join key: level3 string matches gpkg 'cod_indec' column
    weekly_df <- df %>%
      dplyr::group_by(level3, year, week) %>%
      dplyr::summarise(
        n_cases    = sum(n_cases, na.rm = TRUE),
        population = first_pop(population),
        .groups    = "drop"
      ) %>%
      dplyr::rename(analysis_unit_id    = level3,
                    analysis_unit_label = level3)

    weekly_df <- weekly_df %>%
      dplyr::mutate(analysis_unit_label = analysis_unit_id)
  }

  weekly_df %>%
    dplyr::group_by(analysis_unit_id, analysis_unit_label) %>%
    dplyr::summarise(
      n_cases    = sum(n_cases, na.rm = TRUE),
      population = first_pop(population),
      n_weeks    = dplyr::n_distinct(week),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      prevalence_100k = dplyr::if_else(
        !is.na(population) & population > 0,
        (n_cases / population) * 100000,
        NA_real_
      )
    )
}
