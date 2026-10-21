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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "config.css"),
      tags$script(HTML(sprintf("
        (function () {
          var isLoadingOutput = false;
          var overlaySelector = '#%s';
          var runAnalysisInputId = '%s';
          var mapOutputIds = ['%s', '%s'];

          function isGeospatialMap(event) {
            return mapOutputIds.indexOf(event.name) !== -1;
          }

          function showGeospatialLoading() {
            isLoadingOutput = true;
            $(overlaySelector)
              .addClass('is-visible')
              .attr('aria-hidden', 'false');
          }

          function hideGeospatialLoading() {
            isLoadingOutput = false;
            $(overlaySelector)
              .removeClass('is-visible')
              .attr('aria-hidden', 'true');
          }

          $(document)
            .off('shiny:inputchanged.geospatialLoading')
            .on('shiny:inputchanged.geospatialLoading', function (event) {
              if (event.name === runAnalysisInputId) {
                showGeospatialLoading();
              }
            })
            .off('shiny:outputinvalidated.geospatialLoading')
            .on('shiny:outputinvalidated.geospatialLoading', function (event) {
              if (isGeospatialMap(event)) {
                showGeospatialLoading();
              }
            })
            .off('shiny:idle.geospatialLoading')
            .on('shiny:idle.geospatialLoading', function () {
              if (isLoadingOutput) {
                hideGeospatialLoading();
              }
            });
        }());
      ",
        ns("loading_overlay"),
        ns("run_analysis"),
        ns("prevalence_map"),
        ns("lisa_map")
      )))
    ),

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
                geospatial_inputs_ui(ns)
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
                                            class = "dashboard-primary-button"),
                             br(), br(),
                             uiOutput(ns("results_table_filter_ui")),
                             DT::DTOutput(ns("results_table"))),
                    tabPanel("Prepared Data",
                             br(),
                             tags$p(class = "output-description",
                                    "This table shows the filtered and aggregated dataset used for the spatial analysis."),
                             uiOutput(ns("prepared_change_log")),
                             downloadButton(ns("download_prepared_data"),
                                            "Download Prepared Data CSV",
                                            class = "dashboard-primary-button"),
                             br(), br(),
                             uiOutput(ns("prepared_data_table_filter_ui")),
                             DT::DTOutput(ns("prepared_data_table"))),
                    tabPanel("Help",
                             br(),
                             geospatial_help_tab(ns))
                  )
                )
              )
            )
        )
    ),

    tags$div(
      id = ns("loading_overlay"),
      class = "geospatial-loading-overlay",
      role = "status",
      `aria-live` = "polite",
      `aria-hidden` = "true",
      tags$div(
        class = "geospatial-loading-dialog",
        tags$div(
          class = "geospatial-loading-spinner",
          `aria-hidden` = "true"
        ),
        tags$div(class = "geospatial-loading-title", "Loading..."),
        tags$div(
          class = "geospatial-loading-message",
          "Processing data and updating maps."
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
    state <- environment()

    prepared_geo_data <- reactiveVal(NULL)
    prepared_geo_log <- reactiveVal(character(0))
    uploaded_file_state <- reactiveVal(NULL)

    geospatial_server_helpers(state)
    geospatial_server_uploads(state)
    geospatial_server_inputs(state)
    geospatial_server_maps(state)
    geospatial_server_tables(state)
    geospatial_server_downloads(state)
  })
}
