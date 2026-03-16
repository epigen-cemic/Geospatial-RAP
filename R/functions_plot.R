# Popup label builder ----------------------------------------------------------
format_popup_label <- function(id_value, label_value, prevalence, cases, pop,
                               cluster = NULL) {
  parts <- c(
    sprintf("<strong>ID:</strong> %s",              id_value),
    sprintf("<strong>Area:</strong> %s",            label_value),
    sprintf("<strong>Cases:</strong> %s",           ifelse(is.na(cases),      "—", formatC(cases, format = "d", big.mark = ","))),
    sprintf("<strong>Population:</strong> %s",      ifelse(is.na(pop),        "—", formatC(pop,   format = "d", big.mark = ","))),
    sprintf("<strong>Prevalence / 100,000:</strong> %s",
            ifelse(is.na(prevalence), "—", format(round(prevalence, 2), nsmall = 2)))
  )
  if (!is.null(cluster)) {
    parts <- c(parts, sprintf("<strong>LISA cluster:</strong> %s", cluster))
  }
  htmltools::HTML(paste(parts, collapse = "<br/>"))
}

# Choropleth prevalence map ----------------------------------------------------
render_prevalence_map <- function(sf_df,
                                  selected_level = c("department", "fraction")) {
  selected_level <- match.arg(selected_level)
  id_col    <- if (selected_level == "department") "cde"       else "cod_indec"
  label_col <- if (selected_level == "department") "nam"       else "cod_indec"

  valid_vals <- sf_df$prevalence_100k[!is.na(sf_df$prevalence_100k)]
  if (length(valid_vals) == 0) stop("No prevalence data available after joining to geometry. Check that level2 names in the CSV match department names (nam) in the geopackage.")

  pal <- leaflet::colorNumeric(
    palette  = "YlOrRd",
    domain   = sf_df$prevalence_100k,
    na.color = "#d9d9d9"
  )

  labels <- purrr::pmap(
    list(sf_df[[id_col]], sf_df[[label_col]],
         sf_df$prevalence_100k, sf_df$n_cases, sf_df$population),
    format_popup_label
  )

  leaflet::leaflet(sf_df) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      fillColor       = ~pal(prevalence_100k),
      fillOpacity     = 0.8,
      color           = "#555555",
      weight          = 0.7,
      popup           = labels,
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#000000",
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      pal      = pal,
      values   = ~prevalence_100k,
      title    = "Prevalence / 100,000",
      position = "bottomright",
      na.label = "No data"
    )
}

# LISA cluster map -------------------------------------------------------------
render_lisa_map <- function(sf_df,
                            selected_level = c("department", "fraction")) {
  selected_level <- match.arg(selected_level)
  id_col    <- if (selected_level == "department") "cde"       else "cod_indec"
  label_col <- if (selected_level == "department") "nam"       else "cod_indec"

  cluster_levels <- c(
    "High-High", "Low-Low", "High-Low", "Low-High",
    "Not significant", "No data", "No variation", "No neighbours",
    "Insufficient areas"
  )
  sf_df$cluster_type <- factor(sf_df$cluster_type, levels = cluster_levels)

  cluster_colours <- c(
    "High-High"          = "#d73027",
    "Low-Low"            = "#4575b4",
    "High-Low"           = "#f46d43",
    "Low-High"           = "#74add1",
    "Not significant"    = "#d9d9d9",
    "No data"            = "#f0f0f0",
    "No variation"       = "#bdbdbd",
    "No neighbours"      = "#969696",
    "Insufficient areas" = "#737373"
  )

  # Only include levels present in the data to keep the legend clean
  present_levels   <- levels(droplevels(sf_df$cluster_type))
  present_colours  <- cluster_colours[present_levels]

  pal <- leaflet::colorFactor(
    palette  = unname(present_colours),
    levels   = present_levels,
    na.color = "#f0f0f0"
  )

  labels <- purrr::pmap(
    list(sf_df[[id_col]], sf_df[[label_col]],
         sf_df$prevalence_100k, sf_df$n_cases, sf_df$population,
         as.character(sf_df$cluster_type)),
    format_popup_label
  )

  leaflet::leaflet(sf_df) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      fillColor        = ~pal(cluster_type),
      fillOpacity      = 0.85,
      color            = "#555555",
      weight           = 0.7,
      popup            = labels,
      highlightOptions = leaflet::highlightOptions(
        weight       = 2,
        color        = "#000000",
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      pal      = pal,
      values   = ~cluster_type,
      title    = "LISA cluster type",
      position = "bottomright"
    )
}
