#' @title Load and Standardise Geospatial Layer
#'
#' @description
#' Reads a spatial layer from a Geopackage and uses SQL aliasing to return
#' a standardized schema (id, label, geom). It also ensures the layer is 
#' transformed to WGS84 (EPSG:4326) for immediate compatibility with web maps.
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
  
  # OPTIMIZATION: SQL Aliasing
  # We force the database columns into 'id' and 'label' immediately.
  sql <- sprintf(
    "SELECT %s AS id, %s AS label, geom FROM %s",
    layer_info$spatial_id,
    layer_info$label_col,
    layer_info$layer_name
  )
  
  # OPTIMIZATION: CRS Standardisation
  # st_transform(4326) ensures the map always works in Leaflet.
  sf::st_read(gpkg_path, query = sql, quiet = TRUE) %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
}


#' @title Join Analytical Data to Geometry
#'
#' @description
#' Merges the calculated prevalence data with the spatial polygons using 
#' a standardized ID join. Includes a diagnostic check to ensure the join 
#' was successful.
#'
#' @param sf_layer sf object. The map polygons with a standardized 'id' column.
#' @param analysis_df Data frame. The results with an 'analysis_unit_id' column.
#'
#' @return A joined sf object containing both geometry and analytical data.
#' @export
join_analysis_to_geometry <- function(sf_layer, analysis_df) {
  
  # 1. Perform the join on the standardized ID columns
  joined <- dplyr::left_join(sf_layer, analysis_df, by = c("id" = "analysis_unit_id"))
  
  # 2. Diagnostic: Warn if 0 matches occurred
  # (Crucial for RAP to surface data mismatches immediately)
  if (all(is.na(joined$prevalence_100k))) {
    warning("Spatial Join Failure: 0 geometries matched the analysis data. Check ID formatting.")
  }
  
  return(joined)
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
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  return(listw)
}


#' @title Compute LISA Cluster Categories
#' @description Calculates Local Moran's I to identify statistically significant 
#' hotspots, coldspots, and spatial outliers.
#' @param sf_data sf object. The joined spatial data.
#' @param target_var Character. The column name to analyze (e.g., "prevalence_100k").
#' @param significance Numeric. The p-value threshold (default 0.05).
#' @return An sf object enriched with LISA categories and p-values.
#' @export
compute_lisa <- function(sf_data, target_var, significance = 0.05) {
  # 1. Get the weights from our new helper
  listw <- get_spatial_weights(sf_data)
  
  # 2. Do the LISA-specific math
  x    <- sf_data[[target_var]]
  lisa <- spdep::localmoran(x, listw, zero.policy = TRUE)
  
  # 3. Quadrant Logic
  # Scale the variable and its spatial lag to find the center (0)
  scaled_x   <- as.vector(scale(x))
  spatial_lag <- spdep::lag.listw(listw, scaled_x, zero.policy = TRUE)
  
  # 4. Assignment
  lisa_cat <- rep("Not Significant", nrow(sf_data))
  p_vals   <- lisa[, 5]
  
  is_sig <- p_vals <= significance
  
  lisa_cat[is_sig & scaled_x > 0 & spatial_lag > 0] <- "High-High"
  lisa_cat[is_sig & scaled_x < 0 & spatial_lag < 0] <- "Low-Low"
  lisa_cat[is_sig & scaled_x > 0 & spatial_lag < 0] <- "High-Low"
  lisa_cat[is_sig & scaled_x < 0 & spatial_lag > 0] <- "Low-High"
  
  # Return enriched data
  sf_data %>%
    dplyr::mutate(
      lisa_category = factor(lisa_cat, levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")),
      p_value       = p_vals
    )
}


#' @title Compute Global Moran's I
#' @description Calculates the global spatial autocorrelation statistic.
#' Fails gracefully (returns NULL) if the data is too sparse or polygons are disconnected.
#' @param sf_data sf object.
#' @param target_var Character. Variable to analyze.
#' @return A list with statistic and p_value, or NULL if computation fails.
#' @export
compute_global_moran <- function(sf_data, target_var) {
  tryCatch({
    
    # Get weights using the silenced helper function!
    listw <- get_spatial_weights(sf_data)
    
    # 3. Calculate Global Moran's I
    gm_test <- spdep::moran.test(
      x           = sf_data[[target_var]], 
      listw       = listw, 
      zero.policy = TRUE, 
      na.action   = na.omit
    )
    
    # 4. Return formatted results
    list(
      statistic = round(gm_test$estimate[1], 3),
      p_value   = round(gm_test$p.value, 4)
    )
    
  }, error = function(e) {
    # If the math fails (e.g., completely disconnected polygons), return NULL silently
    return(NULL) 
  })
}