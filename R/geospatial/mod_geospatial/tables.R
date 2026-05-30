#' Register Geospatial module logic: tables.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_tables <- function(state) {
  evalq({
    output$results_table <- DT::renderDT({
      req(analyzed_sf())
      render_results_table(analyzed_sf(), target_var = target_var, digits = display_digits())
    })

    output$prepared_change_log <- renderUI({
      log_items <- prepared_geo_log()
      if (is.null(log_items) || length(log_items) == 0) return(NULL)

      div(
        class = "prepared-log",
        h4("Data preparation log"),
        tags$ul(lapply(log_items, tags$li))
      )
    })

    output$prepared_data_table <- DT::renderDT({
      req(prepared_geo_data())
      format_display_table(prepared_geo_data())
    })
  }, envir = state)
  invisible(state)
}
