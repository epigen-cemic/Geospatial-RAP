# Load spatial layers -----------------------------------------------------------
load_geospatial_layer <- function(country        = "Argentina",
                                  selected_level = c("department", "fraction"),
                                  config,
                                  data_dir       = here::here("raw_data")) {
  selected_level <- match.arg(selected_level)
  cfg        <- config[[country]]
  layer_info <- cfg$mapping[[selected_level]]
  gpkg_path  <- file.path(data_dir, cfg$file_name)

  if (!file.exists(gpkg_path)) {
    stop(sprintf("Geopackage not found at: %s", gpkg_path))
  }

  if (selected_level == "department") {
    # Select cde (numeric ID kept for reference) + nam (name, used as join key)
    sql <- sprintf("SELECT cde, nam, geom FROM %s", layer_info$layer_name)
  } else {
    sql <- sprintf("SELECT cod_indec, cde, cpr, geom FROM %s", layer_info$layer_name)
  }

  sf::st_read(gpkg_path, query = sql, quiet = TRUE) %>%
    sf::st_make_valid()
}

# Join analytical data to geometry ---------------------------------------------
# Department level : join by NAME  (analysis_unit_id = level2 name  <-> nam in gpkg)
# Fraction level   : join by CODE  (analysis_unit_id = level3 string <-> cod_indec in gpkg)
join_analysis_to_geometry <- function(sf_layer,
                                      analysis_df,
                                      selected_level = c("department", "fraction")) {
  selected_level <- match.arg(selected_level)

  if (selected_level == "department") {
    # Normalise both sides: trim whitespace, consistent case for robust matching
    sf_layer    <- sf_layer %>%
      dplyr::mutate(nam = trimws(as.character(nam)))
    analysis_df <- analysis_df %>%
      dplyr::mutate(analysis_unit_id = trimws(as.character(analysis_unit_id)))

    joined <- dplyr::left_join(sf_layer, analysis_df, by = c("nam" = "analysis_unit_id"))

    # Diagnostic: warn if many geometries have no matching data
    n_matched   <- sum(!is.na(joined$prevalence_100k))
    n_total     <- nrow(joined)
    if (n_matched == 0) {
      warning(sprintf(
        "join_analysis_to_geometry [department]: 0 of %d geometries matched. ",
        n_total,
        "Check that level2 names in the CSV match 'nam' values in the geopackage."
      ))
    }
    joined

  } else {
    sf_layer    <- sf_layer %>%
      dplyr::mutate(cod_indec = trimws(as.character(cod_indec)))
    analysis_df <- analysis_df %>%
      dplyr::mutate(analysis_unit_id = trimws(as.character(analysis_unit_id)))

    joined <- dplyr::left_join(sf_layer, analysis_df, by = c("cod_indec" = "analysis_unit_id"))

    n_matched <- sum(!is.na(joined$prevalence_100k))
    n_total   <- nrow(joined)
    if (n_matched == 0) {
      warning(sprintf(
        "join_analysis_to_geometry [fraction]: 0 of %d geometries matched. ",
        n_total,
        "Check that level3 codes in the CSV match 'cod_indec' values in the geopackage."
      ))
    }
    joined
  }
}

# Local Moran's I (LISA) -------------------------------------------------------
compute_lisa <- function(sf_df,
                         value_col = "prevalence_100k",
                         alpha     = 0.05) {

  vals <- sf_df[[value_col]]

  sf_df$local_moran_i <- NA_real_
  sf_df$local_p_value <- NA_real_
  sf_df$cluster_type  <- "No data"

  valid_idx <- which(!is.na(vals) & !sf::st_is_empty(sf_df))

  if (length(valid_idx) < 3) {
    sf_df$cluster_type[valid_idx] <- "Insufficient areas"
    return(sf_df)
  }

  sub_sf <- sf_df[valid_idx, ]
  x      <- sub_sf[[value_col]]

  if (length(unique(round(x, 10))) < 2) {
    sf_df$cluster_type[valid_idx] <- "No variation"
    return(sf_df)
  }

  nb <- spdep::poly2nb(sub_sf, queen = TRUE)

  if (all(spdep::card(nb) == 0)) {
    sf_df$cluster_type[valid_idx] <- "No neighbours"
    return(sf_df)
  }

  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  local <- spdep::localmoran(x, listw, zero.policy = TRUE)

  z_vals <- as.numeric(scale(x))
  lag_z  <- spdep::lag.listw(listw, z_vals, zero.policy = TRUE)
  p_vals <- local[, 5]

  cluster           <- rep("Not significant", length(x))
  sig               <- !is.na(p_vals) & p_vals <= alpha
  cluster[sig & z_vals >= 0 & lag_z >= 0] <- "High-High"
  cluster[sig & z_vals <= 0 & lag_z <= 0] <- "Low-Low"
  cluster[sig & z_vals >  0 & lag_z <  0] <- "High-Low"
  cluster[sig & z_vals <  0 & lag_z >  0] <- "Low-High"

  sf_df$local_moran_i[valid_idx] <- as.numeric(local[, 1])
  sf_df$local_p_value[valid_idx] <- as.numeric(p_vals)
  sf_df$cluster_type[valid_idx]  <- cluster

  sf_df
}

# Global Moran's I summary statistic -------------------------------------------
compute_global_moran <- function(sf_df, value_col = "prevalence_100k") {
  vals      <- sf_df[[value_col]]
  valid_idx <- which(!is.na(vals) & !sf::st_is_empty(sf_df))

  if (length(valid_idx) < 3) return(NULL)

  sub_sf <- sf_df[valid_idx, ]
  x      <- sub_sf[[value_col]]

  if (length(unique(round(x, 10))) < 2) return(NULL)

  nb     <- spdep::poly2nb(sub_sf, queen = TRUE)
  if (all(spdep::card(nb) == 0)) return(NULL)

  listw  <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  mt     <- spdep::moran.test(x, listw, zero.policy = TRUE)

  list(
    statistic = round(mt$statistic, 4),
    p_value   = round(mt$p.value,   4),
    method    = mt$method
  )
}

# Results table ----------------------------------------------------------------
prepare_results_table <- function(sf_df,
                                  selected_level = c("department", "fraction")) {
  selected_level <- match.arg(selected_level)

  # For department: id=cde (numeric INDEC code), label=nam (name)
  # For fraction:   id=cod_indec, label=cod_indec
  id_col    <- if (selected_level == "department") "cde"       else "cod_indec"
  label_col <- if (selected_level == "department") "nam"       else "cod_indec"

  sf_df %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      ID              = .data[[id_col]],
      Area            = .data[[label_col]],
      Cases           = n_cases,
      Population      = population,
      Weeks           = n_weeks,
      Prevalence_100k = round(prevalence_100k, 2),
      Local_Moran_I   = round(local_moran_i,   4),
      P_value         = round(local_p_value,    4),
      LISA_cluster    = cluster_type
    ) %>%
    dplyr::arrange(dplyr::desc(Prevalence_100k), ID)
}
