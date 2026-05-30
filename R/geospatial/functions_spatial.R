#' @title Load and Standardise Geospatial Layer
#'
#' @description
#' Reads a spatial layer from a Geopackage and returns a standardized schema
#' (id, label, geom). It also ensures the layer is transformed to WGS84
#' (EPSG:4326) for compatibility with Leaflet.
#'
#' @param selected_level Character. The level key (e.g., "department").
#' @param config List. The loaded config.json object.
#' @param active_country Character. The country key (e.g., "Argentina").
#' @param data_dir Character. Relative path to the data folder.
#'
#' @return An `sf` object with columns: id, label, and geom.
#' @export
load_geospatial_layer <- function(selected_level,
                                  config,
                                  active_country,
                                  data_dir = "raw_data") {

  cfg        <- config[[active_country]]
  layer_info <- cfg$mapping[[selected_level]]
  gpkg_path  <- file.path(data_dir, cfg$file_name)

  if (!file.exists(gpkg_path)) {
    stop(sprintf("Geopackage not found at: %s", gpkg_path))
  }

  label_col <- api_pop_get_name_col(layer_info)

  sql <- sprintf(
    "SELECT %s AS id, %s AS label, geom FROM %s",
    layer_info$spatial_id,
    label_col,
    layer_info$layer_name
  )

  sf::st_read(gpkg_path, query = sql, quiet = TRUE) %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
}


#' @title Load Background Layer (Optimized RDS)
#' @param level Character. The level name (e.g., "province", "department").
#' @param data_dir Character. Directory where optimized RDS files are stored.
#' @return An sf object.
#' @export
load_background_layer <- function(level, data_dir = "data") {
  path <- file.path(data_dir, paste0(level, "_optimized.rds"))

  if (!file.exists(path)) {
    stop(sprintf("Background RDS not found: %s", path))
  }

  readRDS(path)
}


#' @title Normalise Geography Join Keys
#'
#' @description
#' Internal helper used when matching uploaded geography values to spatial-layer
#' IDs or labels. It trims whitespace, lowercases values, and removes accents so
#' names such as "Ciudad Autonoma de Buenos Aires" can match spatial labels such
#' as "Ciudad Autónoma de Buenos Aires".
#'
#' @param x Vector of geography IDs or names.
#'
#' @return Character vector with normalised join keys.
#' @keywords internal
normalise_geography_join_key <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(x)
}

#' @title Join Analytical Data to Geometry
#' @param sf_layer sf object. The map polygons with standardized `id` and `label` columns.
#' @param analysis_df Data frame. The results with an `analysis_unit_id` column.
#' @param target_var Character scalar. Analysis variable used to check join success.
#' @return A joined sf object containing both geometry and analytical data.
#' @export
join_analysis_to_geometry <- function(sf_layer, analysis_df, target_var = "prevalence_100k") {

  sf_layer$id <- as.character(sf_layer$id)
  analysis_df$analysis_unit_id <- as.character(analysis_df$analysis_unit_id)

  joined_by_id <- dplyr::left_join(sf_layer, analysis_df, by = c("id" = "analysis_unit_id"))

  id_matches <- 0L
  if (target_var %in% names(joined_by_id)) {
    id_matches <- sum(!is.na(joined_by_id[[target_var]]))
  }

  if (id_matches > 0L || !"label" %in% names(sf_layer)) {
    if (target_var %in% names(joined_by_id) && all(is.na(joined_by_id[[target_var]]))) {
      warning("Spatial Join Failure: 0 geometries matched the analysis data. Check ID formatting.")
    }
    return(joined_by_id)
  }

  # Some API-POP exports use geography names (for example, "Comuna 10") while
  # the configured spatial layer uses official codes as IDs (for example,
  # "02070"). If ID matching fails, fall back to matching against the spatial
  # label/name column.
  sf_by_label <- sf_layer %>%
    dplyr::mutate(.join_key = normalise_geography_join_key(.data$label))

  analysis_by_label <- analysis_df %>%
    dplyr::mutate(.join_key = normalise_geography_join_key(.data$analysis_unit_id))

  joined_by_label <- dplyr::left_join(sf_by_label, analysis_by_label, by = ".join_key") %>%
    dplyr::select(-.join_key)

  label_matches <- 0L
  if (target_var %in% names(joined_by_label)) {
    label_matches <- sum(!is.na(joined_by_label[[target_var]]))
  }

  if (label_matches > 0L) {
    return(joined_by_label)
  }

  warning("Spatial Join Failure: 0 geometries matched the analysis data by ID or by geography label/name.")
  joined_by_id
}


#' @title Get Spatial Weights
#' @description Internal helper to generate the weights matrix used for all spatial tests.
#' @param sf_data sf object.
#' @return A listw spatial weights object.
#' @keywords internal
get_spatial_weights <- function(sf_data) {
  nb <- suppressWarnings(suppressMessages(
    spdep::poly2nb(sf_data)
  ))
  spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
}


#' @title Compute LISA Cluster Categories
#' @description Calculates Local Moran's I to identify statistically significant clusters.
#' @param sf_data sf object. The joined spatial data.
#' @param target_var Character. The column name to analyze (e.g., "prevalence_100k").
#' @param significance Numeric. The p-value threshold (default 0.05).
#' @return An sf object enriched with LISA categories and p-values, or the 
#'   original sf_data with NA columns if computation cannot proceed.
#' @export
compute_lisa <- function(sf_data, target_var, significance = 0.05) {

  x <- sf_data[[target_var]]

  if (nrow(sf_data) < 4) {
    warning(sprintf(
      "compute_lisa: only %d spatial units available (minimum 4 required). LISA not computed.",
      nrow(sf_data)
    ))
    return(
      sf_data %>%
        dplyr::mutate(
          lisa_category = factor(NA_character_,
                                 levels = c("High-High", "Low-Low", "High-Low",
                                            "Low-High", "Not Significant")),
          p_value       = NA_real_
        )
    )
  }

  if (all(is.na(x)) || isTRUE(var(x, na.rm = TRUE) == 0)) {
    warning("compute_lisa: target variable has zero variance or all values are NA. LISA not computed.")
    return(
      sf_data %>%
        dplyr::mutate(
          lisa_category = factor(NA_character_,
                                 levels = c("High-High", "Low-Low", "High-Low",
                                            "Low-High", "Not Significant")),
          p_value       = NA_real_
        )
    )
  }

  tryCatch({
    listw <- get_spatial_weights(sf_data)
    lisa  <- spdep::localmoran(x, listw, zero.policy = TRUE)

    scaled_x    <- as.vector(scale(x))
    spatial_lag <- spdep::lag.listw(listw, scaled_x, zero.policy = TRUE)

    lisa_cat <- rep("Not Significant", nrow(sf_data))
    p_vals   <- lisa[, 5]

    is_sig <- p_vals <= significance

    lisa_cat[is_sig & scaled_x > 0 & spatial_lag > 0] <- "High-High"
    lisa_cat[is_sig & scaled_x < 0 & spatial_lag < 0] <- "Low-Low"
    lisa_cat[is_sig & scaled_x > 0 & spatial_lag < 0] <- "High-Low"
    lisa_cat[is_sig & scaled_x < 0 & spatial_lag > 0] <- "Low-High"

    sf_data %>%
      dplyr::mutate(
        lisa_category = factor(lisa_cat, levels = c("High-High", "Low-Low",
                                                    "High-Low", "Low-High",
                                                    "Not Significant")),
        p_value       = p_vals
      )

  }, error = function(e) {
    warning(sprintf("compute_lisa failed: %s", conditionMessage(e)))
    sf_data %>%
      dplyr::mutate(
        lisa_category = factor(NA_character_,
                               levels = c("High-High", "Low-Low", "High-Low",
                                          "Low-High", "Not Significant")),
        p_value       = NA_real_
      )
  })
}


#' @title Compute Global Moran's I
#' @description Calculates the global spatial autocorrelation statistic.
#' Fails gracefully (returns NULL) if the data is too sparse, constant, or disconnected.
#' @param sf_data sf object.
#' @param target_var Character. Variable to analyze.
#' @return A list with statistic and p_value, or NULL if computation fails.
#' @export
compute_global_moran <- function(sf_data, target_var) {

  x <- sf_data[[target_var]]

  if (nrow(sf_data) < 4) return(NULL)
  if (all(is.na(x)) || isTRUE(var(x, na.rm = TRUE) == 0)) return(NULL)

  tryCatch({
    listw <- get_spatial_weights(sf_data)

    gm_test <- spdep::moran.test(
      x           = x,
      listw       = listw,
      zero.policy = TRUE,
      na.action   = na.omit
    )

    list(
      statistic = round(gm_test$estimate[1], 3),
      p_value   = round(gm_test$p.value, 4)
    )

  }, error = function(e) {
    NULL
  })
}
