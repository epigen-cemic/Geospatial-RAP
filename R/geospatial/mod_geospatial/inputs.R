#' Geospatial input/sidebar UI
#'
#' @param ns Namespace function from the Shiny module.
#' @return A Shiny UI object.
geospatial_inputs_ui <- function(ns) {
  div(
    class = "sidebar-dark",
    fileInput(ns("file_upload"), "Upload API-Pop CSV",
              multiple = TRUE, accept = c(".csv")),
    uiOutput(ns("uploaded_files_ui")),

    uiOutput(ns("country_selector_ui")),
    uiOutput(ns("geodata_warning")),
    uiOutput(ns("geo_level_ui")),

    tags$label(class = "control-label", "Time Window"),
    selectInput(ns("time_mode"), label = NULL,
                choices = c("Single epidemiological week" = "single",
                            "Week range"                  = "range"),
                selected = "single", width = "100%"),

    uiOutput(ns("year_ui")),
    uiOutput(ns("week_ui")),
    br(),
    actionButton(ns("run_analysis"), "Run Analysis",
                 class = "dashboard-run-button"),
    br(), br(),
    tags$small(style = "color: #cccccc; display: block;",
               "Calculates observed prevalence per 100,000 and Local Moran's I (LISA)."),
    tags$small(style = "color: #cccccc; display: block; margin-top: 8px;",
               "Department is recommended for routine analysis. Censal Fraction provides higher resolution but may be noisier.")
  )
}

#' Register Geospatial module logic: inputs.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_inputs <- function(state) {
  evalq({
    # Country selection is driven by config$active_country. When the setting
    # contains a single country, no selector is shown. When it contains multiple
    # countries, users can choose which configured country to analyze.
    country_choices <- active_countries
    current_country <- reactive({
      if (length(country_choices) > 1) {
        req(input$active_country)
        input$active_country
      } else {
        active_country
      }
    })

    output$country_selector_ui <- renderUI({
      if (length(country_choices) <= 1) {
        return(NULL)
      }

      tagList(
        tags$label(class = "control-label", "Country"),
        selectInput(
          ns("active_country"),
          label = NULL,
          choices = country_choices,
          selected = active_country,
          width = "100%"
        )
      )
    })

    output$geodata_warning <- renderUI({
      req(current_country())

      messages <- rap_format_preflight_messages(preflight_results[[current_country()]])
      if (length(messages) == 0) {
        return(NULL)
      }

      div(
        class = "dashboard-error",
        tags$strong("Configuration warning"),
        tags$ul(lapply(messages, tags$li))
      )
    })

    output$geo_level_ui <- renderUI({
      req(current_country())

      choices <- rap_geospatial_choices(config, current_country(), include_level1 = FALSE)
      shiny::validate(shiny::need(length(choices) > 0, "No geographic levels are configured for this country."))

      tags$div(
        tags$label(class = "control-label", "Geographic Level"),
        selectInput(
          ns("geo_level"),
          label = NULL,
          choices = choices,
          selected = rap_default_geospatial_level(config, current_country(), include_level1 = FALSE),
          width = "100%"
        )
      )
    })

    raw_combined_data <- reactive({
      req(active_uploaded_file_paths())

      tryCatch({
        combine_uploaded_data(
          file_paths     = active_uploaded_file_paths(),
          config         = config,
          active_country = current_country()
        )

      }, error = function(e) {
        message("CRITICAL ERROR READING CSV: ", e$message)
        showNotification(paste("Error reading files:", e$message), type = "error", duration = 15)
        return(NULL)
      })
    })

    output$error_message <- renderUI({
      if (is.null(raw_combined_data())) return(NULL)

      target_col <- NULL
      if (!is.null(input$geo_level) && !is.null(config[[current_country()]]$mapping[[input$geo_level]])) {
        target_col <- api_pop_mapping_column(config[[current_country()]]$mapping[[input$geo_level]])
      }

      needed  <- unique(c("year", "week", "n_cases", "population", target_col))
      missing <- setdiff(needed, names(raw_combined_data()))

      if (length(missing) > 0) {
        div(
          class = "dashboard-error",
          paste(
            "Missing required column(s):",
            paste(missing, collapse = ", "),
            ". Accepted aliases include cases/count/n for n_cases and population/pop/denominator for population."
          )
        )
      } else {
        NULL
      }
    })

    output$year_ui <- renderUI({
      req(raw_combined_data())
      years <- sort(unique(raw_combined_data()$year))
      shiny::validate(shiny::need(length(years) > 0, "No years available in the uploaded data."))

      selected_year <- default_year
      if (is.null(selected_year) || !selected_year %in% years) {
        selected_year <- max(years)
      }

      selectInput(ns("selected_year"), "Year",
                  choices = years, selected = selected_year, width = "100%")
    })

    output$week_ui <- renderUI({
      req(raw_combined_data(), input$selected_year, input$time_mode)

      weeks <- raw_combined_data() %>%
        dplyr::filter(year == input$selected_year) %>%
        dplyr::pull(week) %>%
        unique() %>%
        sort()

      shiny::validate(shiny::need(length(weeks) > 0, "No weeks available for the selected year."))

      if (input$time_mode == "single") {
        selectInput(ns("selected_week"), "Epidemiological Week",
                    choices = weeks, selected = max(weeks), width = "100%")
      } else {
        sliderInput(ns("week_range"), "Week Range",
                    min = min(weeks), max = max(weeks),
                    value = c(min(weeks), max(weeks)), step = 1)
      }
    })

    selected_weeks <- reactive({
      req(input$time_mode)

      if (input$time_mode == "single") {
        req(input$selected_week)
        w <- as.integer(input$selected_week)
        c(w, w)
      } else {
        req(input$week_range)
        as.integer(input$week_range)
      }
    })

    analyzed_sf <- eventReactive(input$run_analysis, {
      req(raw_combined_data(), input$geo_level, input$selected_year, selected_weeks())

      wk <- selected_weeks()

      analysis_df <- tryCatch({
        prepare_geospatial_dataset(
          df             = raw_combined_data(),
          selected_level = input$geo_level,
          selected_year  = as.integer(input$selected_year),
          week_from      = wk[1],
          week_to        = wk[2],
          config         = config,
          active_country = current_country()
        )
      }, error = function(e) {
        showNotification(paste("Data prep error:", e$message), type = "error")
        return(NULL)
      })

      shiny::validate(shiny::need(!is.null(analysis_df) && nrow(analysis_df) > 0, "No data available for the selected inputs."))

      prepared_geo_data(analysis_df)
      prepared_geo_log(api_pop_get_log(analysis_df))

      preflight_messages <- rap_format_preflight_messages(preflight_results[[current_country()]])
      shiny::validate(
        shiny::need(
          length(preflight_messages) == 0,
          paste(preflight_messages, collapse = "\n")
        )
      )

      sf_layer <- tryCatch({
        load_geospatial_layer(
          selected_level = input$geo_level,
          config         = config,
          active_country = current_country()
        )
      }, error = function(e) {
        showNotification(paste("Geopackage error:", e$message), type = "error")
        return(NULL)
      })

      shiny::validate(shiny::need(!is.null(sf_layer), "Could not load spatial layer."))

      expected_len <- config[[current_country()]]$mapping[[input$geo_level]]$code_length

      sf_layer$id <- as.character(sf_layer$id)
      if (!is.null(expected_len) && !is.na(expected_len)) {
        sf_layer$id <- stringr::str_pad(
          sf_layer$id,
          width = expected_len,
          pad = "0"
        )
      }

      analysis_df$analysis_unit_id <- as.character(analysis_df$analysis_unit_id)

      joined_sf <- join_analysis_to_geometry(sf_layer, analysis_df, target_var = target_var)
      joined_sf <- joined_sf %>% dplyr::filter(!is.na(.data[[target_var]]))

      matched_rows <- nrow(joined_sf)
      shiny::validate(
        shiny::need(
          matched_rows > 0,
          sprintf("Spatial Join Failure: The CSV geography values (e.g., %s) do not match the map IDs or labels (e.g., ID %s / label %s).",
                  head(analysis_df$analysis_unit_id, 1), head(sf_layer$id, 1), head(sf_layer$label, 1))
        )
      )

      compute_lisa(joined_sf, target_var = target_var)
    })
  }, envir = state)
  invisible(state)
}
