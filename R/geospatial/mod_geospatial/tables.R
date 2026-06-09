#' Register Geospatial module logic: tables.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_tables <- function(state) {
  evalq({
    results_table_data <- reactive({
      req(analyzed_sf())

      df <- prepare_results_table(analyzed_sf(), target_var)
      selected_col <- input$results_filter_col

      if (is.null(selected_col) || !selected_col %in% names(df)) {
        return(df)
      }

      apply_table_filter(
        df,
        selected_col = selected_col,
        text_value = input$results_filter_text,
        min_value = suppressWarnings(as.numeric(input$results_filter_min)),
        max_value = suppressWarnings(as.numeric(input$results_filter_max))
      )
    })

    prepared_table_data <- reactive({
      req(prepared_geo_data())

      df <- prepared_geo_data()
      selected_col <- input$prepared_filter_col

      if (is.null(selected_col) || !selected_col %in% names(df)) {
        return(df)
      }

      apply_table_filter(
        df,
        selected_col = selected_col,
        text_value = input$prepared_filter_text,
        min_value = suppressWarnings(as.numeric(input$prepared_filter_min)),
        max_value = suppressWarnings(as.numeric(input$prepared_filter_max))
      )
    })

    output$results_table_filter_ui <- renderUI({
      req(analyzed_sf())
      build_table_filter_ui(session$ns, "results", prepare_results_table(analyzed_sf(), target_var))
    })

    output$results_filter_value_ui <- renderUI({
      req(analyzed_sf())
      df <- prepare_results_table(analyzed_sf(), target_var)
      build_table_filter_value_ui(session$ns, "results", df, input$results_filter_col)
    })

    output$prepared_data_table_filter_ui <- renderUI({
      req(prepared_geo_data())
      build_table_filter_ui(session$ns, "prepared", prepared_geo_data())
    })

    output$prepared_filter_value_ui <- renderUI({
      req(prepared_geo_data())
      build_table_filter_value_ui(session$ns, "prepared", prepared_geo_data(), input$prepared_filter_col)
    })

    output$results_table <- DT::renderDT({
      req(analyzed_sf())

      df <- results_table_data()

      cluster_colors <- c(
        "High-High"       = "#fdd0cb",
        "Low-Low"         = "#c9d9f0",
        "High-Low"        = "#fde0c9",
        "Low-High"        = "#d5ecf8",
        "Not Significant" = "#f0f0f0"
      )

      dt <- format_display_table(df)

      if ("LISA_cluster" %in% names(df)) {
        dt <- dt %>%
          DT::formatStyle(
            "LISA_cluster",
            backgroundColor = DT::styleEqual(names(cluster_colors), unname(cluster_colors))
          )
      }

      dt
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
      format_display_table(prepared_table_data())
    })
  }, envir = state)
  invisible(state)
}
