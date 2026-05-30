#' Register Geospatial module logic: maps.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_maps <- function(state) {
  evalq({
    output$global_moran_banner <- renderUI({
      req(analyzed_sf())

      gm <- compute_global_moran(analyzed_sf(), target_var = target_var)
      if (is.null(gm)) return(NULL)

      sig_label <- if (gm$p_value < 0.001) "p < 0.001" else paste0("p = ", round(gm$p_value, display_digits()))
      colour    <- if (gm$p_value <= 0.05) "#e8f5e9" else "#f5f5f5"
      border    <- if (gm$p_value <= 0.05) "#2e7d32"  else "#9e9e9e"
      txt_col   <- if (gm$p_value <= 0.05) "#1b5e20"  else "#616161"

      div(
        class = "global-moran-banner",
        style = sprintf(
          "background:%s; border-left:5px solid %s; color:%s;",
          colour, border, txt_col
        ),
        div(
          class = "global-moran-summary",
          sprintf(
            "Global Moran's I = %s  (%s) - %s",
            round(gm$statistic, display_digits()), sig_label,
            if (gm$p_value <= 0.05) "statistically significant spatial autocorrelation"
            else "no significant spatial autocorrelation detected"
          )
        )
      )
    })

    output$prevalence_map <- leaflet::renderLeaflet({
      req(analyzed_sf())
      render_prevalence_map(
        analyzed_sf(),
        target_var     = target_var,
        is_offline     = is_offline,
        selected_level = input$geo_level,
        config         = config,
        active_country = current_country()
      )
    })

    output$lisa_map <- leaflet::renderLeaflet({
      req(analyzed_sf())
      render_lisa_map(
        analyzed_sf(),
        target_var     = target_var,
        is_offline     = is_offline,
        selected_level = input$geo_level,
        config         = config,
        active_country = current_country()
      )
    })
  }, envir = state)
  invisible(state)
}
