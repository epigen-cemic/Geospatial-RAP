#' @title Prepare Results Table
#' @description Formats the spatial analysis results into a clean, non-spatial 
#' dataframe for display in the Shiny UI (DT table).
#' @param sf_df sf object. The output from join_analysis_to_geometry and compute_lisa.
#' @param target_var Character. Name of the analytical variable to show in the table.
#' @return A data frame (no geometry) with rounded results and standardized headers.
#' @export
prepare_results_table <- function(sf_df, target_var) {
  sf_df %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      ID           = id,
      Area         = label,
      Cases        = n_cases,
      Population   = population,
      Result       = round(.data[[target_var]], 2),
      P_value      = round(p_value, 4),
      LISA_cluster = lisa_category
    ) %>%
    dplyr::arrange(dplyr::desc(Result), ID)
}


#' @title Format Popup Labels
#' @description Vectorized HTML label.
#' @keywords internal
format_popup_label <- function(sf_df, target_var) {
  var_label <- gsub("_", " ", target_var)
  var_label <- paste0(toupper(substr(var_label, 1, 1)), substr(var_label, 2, nchar(var_label)))

  sprintf(
    "<strong>%s</strong> (ID: %s)<br/>
    %s: %s<br/>
    Cluster: %s<br/>
    P-value: %s",
    sf_df$label,
    sf_df$id,
    var_label,
    round(sf_df[[target_var]], 2),
    sf_df$lisa_category,
    round(sf_df$p_value, 4)
  ) %>% lapply(htmltools::HTML)
}


#' @title Resolve Background Geographic Level
#' @param selected_level Character string. The currently selected geographic level.
#' @param config List. The parsed configuration object loaded from `config.json`.
#' @param active_country Character string. The country key used to extract the country mapping.
#' @return A character string representing the background level key.
#' @export
get_background_level <- function(selected_level, config, active_country) {

  mappings <- config[[active_country]]$mapping

  ordered_keys <- names(mappings)[order(sapply(mappings, function(x) {
    as.numeric(gsub("level", "", x$csv_column))
  }))]

  pos <- match(selected_level, ordered_keys)
  bg_pos <- max(1, pos - 1)

  ordered_keys[bg_pos]
}


#' @title Base Map Generator for Geospatial Plots
#' @param sf_df sf object. Spatial dataframe containing geometries and analysis results.
#' @param target_var Character. Name of the primary analysis variable.
#' @param pal Function. A Leaflet color palette function.
#' @param fill_col Character. Column used to color the polygons.
#' @param is_offline Logical. If TRUE, suppresses external tile providers.
#' @return A Leaflet map object.
#' @export
add_base_geospatial_map <- function(sf_df, target_var, pal, fill_col,
                                    is_offline, selected_level,
                                    config, active_country) {

  map <- leaflet::leaflet()

  if (!is_offline) {
    map <- map %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldGrayCanvas,
        options = leaflet::providerTileOptions(opacity = 0.6)
      )
  } else {
    bg_level <- get_background_level(selected_level, config, active_country)
    bg_layer <- load_background_layer(bg_level)

    map <- map %>%
      leaflet::addPolygons(
        data        = bg_layer,
        fillColor   = "#d9d9d9",
        color       = "#bdbdbd",
        weight      = 0.8,
        fillOpacity = 1
      )
  }

  map <- map %>%
    leaflet::addPolygons(
      data         = sf_df,
      fillColor    = pal(sf_df[[fill_col]]),
      fillOpacity  = 0.8,
      color        = "#404040",
      weight       = 0.5,
      popup        = format_popup_label(sf_df, target_var),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#ffffff",
        bringToFront = TRUE
      )
    )

  if (is_offline && !is.null(sf_df) && nrow(sf_df) > 0) {
    center <- sf::st_coordinates(sf::st_point_on_surface(sf::st_union(sf_df)))
    map <- map %>%
      leaflet::setView(
        lng = center[1],
        lat = center[2],
        zoom = 6
      )
  }

  map
}


#' @title Render Disease Prevalence Map
#' @param sf_df sf object. Spatial data after joining with epidemiological results.
#' @param target_var Character. The variable name used for the color scale.
#' @param is_offline Logical. Whether to run in offline mode.
#' @return A leaflet map object displaying disease prevalence.
#' @export
render_prevalence_map <- function(sf_df, target_var, is_offline,
                                  selected_level, config, active_country) {

  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain  = sf_df[[target_var]],
    na.color = "#9e9e9e"
  )

  add_base_geospatial_map(
    sf_df          = sf_df,
    target_var     = target_var,
    pal            = pal,
    fill_col       = target_var,
    is_offline     = is_offline,
    selected_level = selected_level,
    config         = config,
    active_country = active_country
  ) %>%
    leaflet::addLegend(
      pal    = pal,
      values = sf_df[[target_var]],
      title  = target_var,
      opacity = 1
    )
}


#' @title Render LISA Cluster Map
#' @param sf_df sf object. Spatial data processed through the LISA calculation module.
#' @param target_var Character. The variable used for the original analysis.
#' @param is_offline Logical. Whether to run in offline mode.
#' @return A leaflet map object displaying LISA clusters.
#' @export
render_lisa_map <- function(sf_df, target_var, is_offline,
                            selected_level, config, active_country) {

  lisa_colors <- c(
    "High-High" = "#d73027", "Low-Low" = "#4575b4", "High-Low" = "#f46d43",
    "Low-High" = "#74add1", "Not Significant" = "#d9d9d9", "No data" = "#f0f0f0"
  )

  present_levels <- levels(droplevels(factor(sf_df$lisa_category, levels = names(lisa_colors))))
  pal <- leaflet::colorFactor(palette = lisa_colors[present_levels], domain = present_levels)

  add_base_geospatial_map(
    sf_df          = sf_df,
    target_var     = target_var,
    pal            = pal,
    fill_col       = "lisa_category",
    is_offline     = is_offline,
    selected_level = selected_level,
    config         = config,
    active_country = active_country
  ) %>%
    leaflet::addLegend(
      pal    = pal,
      values = sf_df$lisa_category,
      title  = "LISA Clusters"
    )
}


#' @title Render Results Datatable
#' @param sf_df sf object. The analyzed spatial data.
#' @param target_var Character. The name of the analysis variable.
#' @return A DT datatable object.
#' @export
render_results_table <- function(sf_df, target_var) {

  tbl_data <- prepare_results_table(sf_df, target_var)

  cluster_colors <- c(
    "High-High"       = "#fdd0cb",
    "Low-Low"         = "#c9d9f0",
    "High-Low"        = "#fde0c9",
    "Low-High"        = "#d5ecf8",
    "Not Significant" = "#f0f0f0"
  )

  DT::datatable(
    tbl_data,
    rownames = FALSE,
    filter   = "top",
    options  = list(
      pageLength = 15,
      scrollX = TRUE,
      dom = 'ltip'
    )
  ) %>%
    DT::formatStyle(
      "LISA_cluster",
      backgroundColor = DT::styleEqual(names(cluster_colors), unname(cluster_colors))
    )
}
