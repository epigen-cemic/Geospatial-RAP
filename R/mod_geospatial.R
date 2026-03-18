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
              # ---- Sidebar ----
              column(3,
                div(class = "sidebar-dark",
                    fileInput(ns("file_upload"), "Upload API-Pop CSV",
                              multiple = TRUE, accept = c(".csv")),

                    tags$label(class = "control-label", "Geographic Level"),
                    selectInput(ns("geo_level"), label = NULL,
                                choices = geo_choices, selected = "department",
                                width = "100%"),

                    tags$label(class = "control-label", "Time Window"),
                    selectInput(ns("time_mode"), label = NULL,
                                choices = c("Single epidemiological week" = "single",
                                            "Week range"                  = "range"),
                                selected = "single", width = "100%"),

                    uiOutput(ns("year_ui")),
                    uiOutput(ns("week_ui")),
                    br(),
                    actionButton(ns("run_analysis"), "Run Spatial Analysis",
                                 class = "btn-default",
                                 style = "width: 100%; color: black;"),
                    br(), br(),
                    tags$small(style = "color: #cccccc; display: block;",
                               "Calculates prevalence per 100,000 and Local Moran's I (LISA)."),
                    tags$small(style = "color: #cccccc; display: block; margin-top: 8px;",
                               "Department is recommended for routine analysis. Censal Fraction provides higher resolution but may be noisier.")
                )
              ),

              # ---- Main panel ----
              column(9,
                     uiOutput(ns("error_message")),
                     uiOutput(ns("global_moran_banner")),
                     
                     # THE FIX: Wrap the tabsetPanel in the main-tabs div!
                     div(class = "main-tabs",
                         tabsetPanel(
                           tabPanel("Prevalence Map",
                                    br(),
                                    leaflet::leafletOutput(ns("prevalence_map"), height = "650px")
                           ),
                           tabPanel("LISA Cluster Map",
                                    br(),
                                    leaflet::leafletOutput(ns("lisa_map"), height = "650px")
                           ),
                           tabPanel("Results Table",
                                    br(),
                                    DT::DTOutput(ns("results_table")),
                                    br(),
                                    downloadButton(ns("download_results"),
                                                   "Download Results CSV",
                                                   class = "btn-default")
                           )
                         )
                     )
              )
            )
        )
    )
  )
}


geospatialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Data ingestion ---------------------------------------------------------
    raw_combined_data <- reactive({
      req(input$file_upload)
      tryCatch({
        # 1. Read the raw files
        file_paths <- setNames(input$file_upload$datapath, seq_along(input$file_upload$datapath))
        raw_df <- purrr::map_dfr(file_paths, read.csv, .id = "file_index")
        # This will lowercase, rename aliases, validate columns, AND pad the zeros.
        clean_df <- standardise_input_columns(
          df             = raw_df, 
          config         = config, 
          active_country = active_country
        )

        return(clean_df)
        
      }, error = function(e) {
        message("CRITICAL ERROR READING CSV: ", e$message)
        showNotification(paste("Error reading files:", e$message), type = "error", duration = 15)
        return(NULL)
      })
    })
    
    # -- Dynamic year selector --------------------------------------------------
    output$year_ui <- renderUI({
      req(raw_combined_data())
      years <- sort(unique(raw_combined_data()$year))
      selectInput(ns("selected_year"), "Year",
                  choices = years, selected = max(years), width = "100%")
    })

    # -- Dynamic week selector --------------------------------------------------
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

    # -- Resolve selected week window ------------------------------------------
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

    # -- Core analysis (triggered by button only) ------------------------------
    analyzed_sf <- eventReactive(input$run_analysis, {
      
      # Check if inputs exist
      req(raw_combined_data(), input$geo_level, input$selected_year, selected_weeks())
      
      wk <- selected_weeks()
      
      # 1. Prepare tabular data
      analysis_df <- tryCatch({
        prepare_geospatial_dataset(
          df              = raw_combined_data(),
          selected_level  = input$geo_level,
          selected_year   = as.integer(input$selected_year),
          week_from       = wk[1],
          week_to         = wk[2],
          config          = config,         
          active_country  = active_country  
        )
      }, error = function(e) {
        showNotification(paste("Data prep error:", e$message), type = "error")
        return(NULL)
      })
      
      shiny::validate(shiny::need(!is.null(analysis_df) && nrow(analysis_df) > 0, "No data available."))
      
      # 2. Load spatial geometry
      sf_layer <- tryCatch({
        load_geospatial_layer(
          selected_level = input$geo_level,
          config         = config,          
          active_country = active_country   
        )
      }, error = function(e) {
        showNotification(paste("Geopackage error:", e$message), type = "error")
        return(NULL)
      })
      
      shiny::validate(shiny::need(!is.null(sf_layer), "Could not load spatial layer."))
      
      # 3. Standardize IDs with leading zeros for spatial join
      expected_len <- config[[active_country]]$mapping[[input$geo_level]]$code_length
      
      sf_layer$id <- stringr::str_pad(
        as.character(sf_layer$id), 
        width = expected_len, 
        pad = "0"
      )
      
      analysis_df$analysis_unit_id <- as.character(analysis_df$analysis_unit_id)
      
      # 4. Perform the join
      joined_sf <- join_analysis_to_geometry(sf_layer, analysis_df)
      
      # 5. Isolate regions with data to prevent spatial math errors
      joined_sf <- joined_sf %>% dplyr::filter(!is.na(.data[[target_var]]))
      
      # Validate that the join was successful
      matched_rows <- nrow(joined_sf)
      shiny::validate(
        shiny::need(
          matched_rows > 0, 
          sprintf("Spatial Join Failure: The CSV IDs (e.g., %s) do not match the Map IDs (e.g., %s).", 
                  head(analysis_df$analysis_unit_id, 1), head(sf_layer$id, 1))
        )
      )
      
      # 6. Execute spatial cluster analysis
      final_sf <- compute_lisa(joined_sf, target_var = target_var)
      
      return(final_sf)
    })
    # -- Input validation banner ------------------------------------------------
    output$error_message <- renderUI({
      req(raw_combined_data())
      needed  <- c("country", "level2", "level3", "year", "week", "n_cases", "population")
      missing <- setdiff(needed, names(raw_combined_data()))
      if (length(missing) > 0) {
        div(style = "background:#ffe5e5; color:#a30000; padding:12px; border-left:5px solid #a30000; margin-bottom:15px;",
            paste("Missing required columns:", paste(missing, collapse = ", ")))
      }
    })

    # -- Global Moran's I summary banner ----------------------------------------
    output$global_moran_banner <- renderUI({
      req(analyzed_sf())
      
      # FIX: Changed value_col to target_var
      gm <- compute_global_moran(analyzed_sf(), target_var = target_var)
      
      if (is.null(gm)) return(NULL)
      
      sig_label <- if (gm$p_value < 0.001) "p < 0.001" else paste0("p = ", gm$p_value)
      colour    <- if (gm$p_value <= 0.05) "#e8f5e9" else "#f5f5f5"
      border    <- if (gm$p_value <= 0.05) "#2e7d32"  else "#9e9e9e"
      txt_col   <- if (gm$p_value <= 0.05) "#1b5e20"  else "#616161"
      
      div(style = sprintf(
        "background:%s; border-left:5px solid %s; color:%s; padding:10px 14px; margin-bottom:14px; font-size:13px;",
        colour, border, txt_col),
        sprintf("Global Moran's I = %s  (%s) — %s",
                gm$statistic, sig_label,
                if (gm$p_value <= 0.05) "statistically significant spatial autocorrelation"
                else "no significant spatial autocorrelation detected")
      )
    })

    # -- Map outputs -----------------------------------------------------------
    output$prevalence_map <- leaflet::renderLeaflet({
      req(analyzed_sf())
      # Passes global target_var and is_offline switch
      render_prevalence_map(analyzed_sf(), target_var = target_var, is_offline = is_offline)
    })
    
    output$lisa_map <- leaflet::renderLeaflet({
      req(analyzed_sf())
      render_lisa_map(analyzed_sf(), target_var = target_var, is_offline = is_offline)
    })
    
    # -- Results table ---------------------------------------------------------
    output$results_table <- DT::renderDT({
      req(analyzed_sf())
      # One function now handles both data cleaning and LISA color styling
      render_results_table(analyzed_sf(), target_var = target_var)
    })
    
    # For the download handler, you still need the raw data (without HTML/DT styling)
    output$download_results <- downloadHandler(
      filename = function() {
        wk <- selected_weeks()
        paste0("geospatial_results_", input$geo_level, "_", input$selected_year, ".csv")
      },
      content = function(file) {
        # Use the cleaning function directly for the CSV export
        raw_tbl <- prepare_results_table(analyzed_sf(), target_var = target_var)
        readr::write_csv(raw_tbl, file)
      }
    )
  })
}
