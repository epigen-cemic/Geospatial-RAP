#' Geospatial analysis module UI
#'
#' @description
#' Builds the standalone Geospatial RAP interface, including upload controls,
#' geographic and time filters, map outputs, results tables, prepared data, and
#' Help content.
#'
#' @param id Character scalar. Shiny module namespace identifier.
#'
#' @return A Shiny UI object.
#' @export
geospatialUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "config.css")),

    div(class = "nav-bar",
        div(class = "nav-content",
            div(class = "header-logo-left")
        )
    ),

    div(class = "hero-section",
        div(class = "hero-content",
            h1(class = "app-title", "Geospatial Analysis")
        )
    ),

    div(class = "content-wrapper",
        div(class = "well-custom",
            fluidRow(
              column(
                3,
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
                               class = "btn-default dashboard-run-btn"),
                  br(), br(),
                  tags$small(style = "color: #cccccc; display: block;",
                             "Calculates observed prevalence per 100,000 and Local Moran's I (LISA)."),
                  tags$small(style = "color: #cccccc; display: block; margin-top: 8px;",
                             "Department is recommended for routine analysis. Censal Fraction provides higher resolution but may be noisier.")
                )
              ),

              column(
                9,
                uiOutput(ns("error_message")),
                uiOutput(ns("global_moran_banner")),
                div(
                  class = "main-tabs",
                  tabsetPanel(
                    tabPanel("Prevalence Map",
                             br(),
                             tags$p(class = "output-description",
                                    "Map of observed prevalence per 100,000 for the selected time window."),
                             leaflet::leafletOutput(ns("prevalence_map"), height = "650px")),
                    tabPanel("LISA Cluster Map",
                             br(),
                             tags$p(class = "output-description",
                                    "Local spatial clusters based on Local Moran's I. Clusters should be reviewed epidemiologically."),
                             leaflet::leafletOutput(ns("lisa_map"), height = "650px")),
                    tabPanel("Results Table",
                             br(),
                             downloadButton(ns("download_results"),
                                            "Download Results CSV",
                                            class = "btn-default"),
                             br(), br(),
                             DT::DTOutput(ns("results_table"))),
                    tabPanel("Prepared Data",
                             br(),
                             tags$p(class = "output-description",
                                    "This table shows the filtered and aggregated dataset used for the spatial analysis."),
                             uiOutput(ns("prepared_change_log")),
                             downloadButton(ns("download_prepared_data"),
                                            "Download Prepared Data CSV",
                                            class = "btn-default"),
                             br(), br(),
                             DT::DTOutput(ns("prepared_data_table"))),
                    tabPanel("Help",
                             br(),
                             geospatial_help_tab())
                  )
                )
              )
            )
        )
    )
  )
}


#' Geospatial analysis module server
#'
#' @description
#' Runs the standalone Geospatial RAP server logic, including API-POP import,
#' validation, country-specific configuration, spatial joins, prevalence mapping,
#' LISA calculation, and export handlers.
#'
#' @param id Character scalar. Shiny module namespace identifier matching `geospatialUI()`.
#'
#' @return No return value. Called for Shiny side effects.
#' @export
geospatialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prepared_geo_data <- reactiveVal(NULL)
    prepared_geo_log <- reactiveVal(character(0))

    uploaded_file_state <- reactiveVal(NULL)

    format_uploaded_file_size <- function(bytes) {
      if (is.null(bytes) || is.na(bytes)) return("")
      if (bytes < 1024) return(paste0(bytes, " B"))
      if (bytes < 1024^2) return(paste0(round(bytes / 1024, 1), " KB"))
      paste0(round(bytes / 1024^2, 1), " MB")
    }

    active_uploaded_files <- reactive({
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)
      files <- files[isTRUE(files$active) | files$active, , drop = FALSE]
      req(nrow(files) > 0)
      files
    })

    active_uploaded_file_paths <- reactive({
      files <- active_uploaded_files()
      setNames(files$datapath, seq_along(files$datapath))
    })


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


    observeEvent(input$file_upload, {
      req(input$file_upload)

      uploaded <- input$file_upload
      new_files <- data.frame(
        id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(nrow(uploaded)), "_", make.names(uploaded$name)),
        name = uploaded$name,
        datapath = uploaded$datapath,
        size = uploaded$size,
        uploaded_at = as.character(Sys.time()),
        active = TRUE,
        stringsAsFactors = FALSE
      )

      existing <- uploaded_file_state()
      if (is.null(existing)) {
        uploaded_file_state(new_files)
      } else {
        uploaded_file_state(rbind(existing, new_files))
      }
    }, ignoreInit = TRUE)

    output$uploaded_files_ui <- renderUI({
      files <- uploaded_file_state()
      uploaded_count <- if (is.null(files)) 0 else nrow(files)
      active_count <- if (is.null(files)) 0 else sum(files$active, na.rm = TRUE)

      tagList(
        div(
          class = "uploaded-files-summary",
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label", "Uploaded files"),
            span(class = "uploaded-files-count", uploaded_count)
          ),
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label muted", "Active files"),
            span(class = "uploaded-files-count active", active_count)
          )
        ),
        if (!is.null(files) && nrow(files) > 0) {
          actionButton(ns("manage_uploaded_files"), "Manage files", class = "dashboard-small-button")
        }
      )
    })

    observeEvent(input$manage_uploaded_files, {
      files <- uploaded_file_state()

      if (is.null(files) || nrow(files) == 0) {
        showModal(modalDialog(
          title = "Uploaded files",
          p("No files have been uploaded."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }

      table_rows <- lapply(seq_len(nrow(files)), function(i) {
        tags$tr(
          tags$td(
            checkboxInput(
              ns(paste0("file_active_", files$id[i])),
              label = NULL,
              value = isTRUE(files$active[i]),
              width = "20px"
            )
          ),
          tags$td(files$name[i]),
          tags$td(format_uploaded_file_size(files$size[i]))
        )
      })

      showModal(modalDialog(
        title = "Uploaded files",
        size = "l",
        easyClose = TRUE,
        tags$p("Uncheck files that should not be included in the analysis. Use 'Remove inactive files' to remove unchecked files from this session."),
        tags$table(
          class = "uploaded-files-table",
          tags$thead(tags$tr(tags$th("Active"), tags$th("File name"), tags$th("Size"))),
          tags$tbody(table_rows)
        ),
        footer = tagList(
          actionButton(ns("apply_file_selection"), "Apply selection", class = "dashboard-primary-button"),
          actionButton(ns("remove_inactive_files"), "Remove inactive files", class = "dashboard-danger-button"),
          actionButton(ns("clear_all_files_modal"), "Clear all files", class = "dashboard-danger-button"),
          modalButton("Close")
        )
      ))
    })

    observeEvent(input$apply_file_selection, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )

      uploaded_file_state(files)
      removeModal()
      showNotification(paste(sum(files$active), "active file(s) selected."), type = "message")
    })

    observeEvent(input$remove_inactive_files, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )
      files <- files[files$active, , drop = FALSE]

      if (nrow(files) == 0) {
        uploaded_file_state(NULL)
      } else {
        uploaded_file_state(files)
      }

      removeModal()
      showNotification("Inactive files were removed.", type = "message")
    })

    observeEvent(input$clear_all_files_modal, {
      uploaded_file_state(NULL)
      prepared_geo_data(NULL)
      prepared_geo_log(character(0))
      removeModal()
      showNotification("All uploaded files were removed.", type = "message")
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
          paste(preflight_messages, collapse = "
")
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
          sprintf("Spatial Join Failure: The CSV IDs (e.g., %s) do not match the Map IDs (e.g., %s).",
                  head(analysis_df$analysis_unit_id, 1), head(sf_layer$id, 1))
        )
      )

      compute_lisa(joined_sf, target_var = target_var)
    })

    output$global_moran_banner <- renderUI({
      req(analyzed_sf())

      gm <- compute_global_moran(analyzed_sf(), target_var = target_var)
      if (is.null(gm)) return(NULL)

      sig_label <- if (gm$p_value < 0.001) "p < 0.001" else paste0("p = ", gm$p_value)
      colour    <- if (gm$p_value <= 0.05) "#e8f5e9" else "#f5f5f5"
      border    <- if (gm$p_value <= 0.05) "#2e7d32"  else "#9e9e9e"
      txt_col   <- if (gm$p_value <= 0.05) "#1b5e20"  else "#616161"

      div(
        style = sprintf(
          "background:%s; border-left:5px solid %s; color:%s; padding:10px 14px; margin-bottom:14px; font-size:13px;",
          colour, border, txt_col
        ),
        sprintf(
          "Global Moran's I = %s  (%s) — %s",
          gm$statistic, sig_label,
          if (gm$p_value <= 0.05) "statistically significant spatial autocorrelation"
          else "no significant spatial autocorrelation detected"
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

    output$results_table <- DT::renderDT({
      req(analyzed_sf())
      render_results_table(analyzed_sf(), target_var = target_var)
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
      DT::datatable(
        prepared_geo_data(),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$download_prepared_data <- downloadHandler(
      filename = function() {
        paste0("geospatial_prepared_data_", input$geo_level, "_", input$selected_year, ".csv")
      },
      content = function(file) {
        readr::write_csv(prepared_geo_data(), file)
      }
    )

    output$download_results <- downloadHandler(
      filename = function() {
        paste0("geospatial_results_", input$geo_level, "_", input$selected_year, ".csv")
      },
      content = function(file) {
        raw_tbl <- prepare_results_table(analyzed_sf(), target_var = target_var)
        readr::write_csv(raw_tbl, file)
      }
    )
  })
}
