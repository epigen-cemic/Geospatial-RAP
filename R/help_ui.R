# =============================================================================
# Geospatial Help UI
# =============================================================================

#' Geospatial help tab UI
#'
#' @description
#' Builds the Help tab for the standalone Geospatial RAP tool. It describes the tool purpose, required input columns, accepted aliases, documentation links, display settings, and prepared-data log.
#'
#' @param ns Function. Namespace function from the Shiny module.
#'
#' @return A Shiny `tagList` containing Help tab content.
#' @keywords internal
geospatial_help_tab <- function(ns = identity) {
  tagList(
    div(
      class = "help-card",
      h3("Geospatial RAP Help"),
      p(
        "The Geospatial RAP creates prevalence maps and Local Moran's I (LISA) cluster maps. ",
        "Population is required because prevalence is calculated as cases per 100,000 population."
      ),
      tags$ul(
        tags$li(tags$a(href = "docs/User Manual.pdf", target = "_blank", "Open user manual")),
        tags$li(tags$a(href = "docs/User Manual ES.pdf", target = "_blank", "Open spanish user manual"))
      )
    ),

    div(
      class = "help-card",
      h3("Display settings"),
      p("Use these settings to adjust interface readability and table display without changing downloaded CSV values."),
      tags$label(class = "control-label", "Display Font Size"),
      selectInput(
        ns("font_size"),
        label = NULL,
        choices = c(
          "Small (current)" = "small",
          "Medium" = "medium",
          "Large" = "large",
          "Extra large" = "xlarge"
        ),
        selected = "small",
        width = "280px"
      ),
      tags$label(class = "control-label", "Decimal places shown in result tables"),
      selectInput(
        ns("display_digits"),
        label = NULL,
        choices = setNames(as.character(0:8), as.character(0:8)),
        selected = "4",
        width = "180px"
      ),
      p(class = "help-note", "This only changes how numbers are displayed in the app. Downloaded CSV files keep the full numeric values.")
    ),

    div(
      class = "help-card",
      h3("Required data structure"),
      p("The uploaded file should contain one row per geographic unit and epidemiological week."),
      tags$table(
        class = "requirements-table",
        tags$thead(
          tags$tr(
            tags$th("Column"),
            tags$th("Required"),
            tags$th("Description"),
            tags$th("Accepted aliases / examples")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("year"), tags$td("Yes"), tags$td("Epidemiological year."), tags$td("year, anio, año, epiyear; e.g. 2026")),
          tags$tr(tags$td("week"), tags$td("Yes"), tags$td("Epidemiological week."), tags$td("week, semana, epiweek, se; e.g. 8")),
          tags$tr(tags$td("country"), tags$td("Recommended"), tags$td("Country or national unit."), tags$td("Argentina or England")),
          tags$tr(tags$td("level1"), tags$td("Recommended"), tags$td("First administrative level."), tags$td("province, region, Level1")),
          tags$tr(tags$td("level2"), tags$td("Required for LAD/department maps"), tags$td("Second administrative level."), tags$td("department, district, LAD, Level2")),
          tags$tr(tags$td("level3"), tags$td("Required for MSOA/fraction maps"), tags$td("Third administrative level."), tags$td("fraction, MSOA, Level3")),
          tags$tr(tags$td("n_cases"), tags$td("Yes"), tags$td("Observed cases or event count."), tags$td("n_cases, cases, case_count, count, n")),
          tags$tr(tags$td("population"), tags$td("Yes"), tags$td("Population denominator used to calculate prevalence per 100,000."), tags$td("population, pop, denominator"))
        )
      )
    ),

    div(
      class = "help-card",
      h3("Global Moran's I note"),
      p(
        "The banner above the maps summarizes Global Moran's I for the selected map data. ",
        "It is calculated using prevalence per 100,000 people for the selected country, level, year, and week range."
      ),
      tags$ul(
        tags$li(tags$strong("Moran's I: "), "spatial autocorrelation statistic. Positive values suggest nearby areas have similar prevalence values; values near 0 suggest little spatial pattern; negative values suggest neighboring areas tend to be different."),
        tags$li(tags$strong("p-value: "), "statistical test for whether the observed spatial pattern is unlikely to be due to chance. Values at or below 0.05 are usually interpreted as statistically significant."),
        tags$li(tags$strong("Significant spatial autocorrelation: "), "the selected map has evidence of a non-random spatial pattern. This does not identify individual clusters by itself; use the LISA Cluster Map for local cluster interpretation.")
      )
    ),

    div(
      class = "help-card",
      h3("Prepared data and change log"),
      p(
        "The Prepared Data tab shows the dataset after filtering by year/week, aggregating cases, ",
        "deduplicating population denominators, calculating prevalence, and matching geographic IDs ",
        "to the spatial layer. The change log describes these processing steps."
      )
    )
  )
}
