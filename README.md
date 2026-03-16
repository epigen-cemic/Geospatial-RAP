# Geospatial-RAP v1.1

Reproducible Analytical Pipeline for geospatial epidemiological surveillance (Work Package 1.2).  
Uses the same API-Pop aggregated CSV input format as the CUSUM-RAP.

## What the app does

1. Reads one or more API-Pop CSV files (comma or semicolon-separated).  
2. Aggregates cases and population by the chosen geographic level and time window.  
3. Calculates **consultation-based prevalence per 100,000 inhabitants**.  
4. Runs **Global Moran's I** (spatial autocorrelation summary) and **Local Moran's I (LISA)** cluster detection.  
5. Produces three outputs:
   - **Choropleth map** of prevalence  
   - **LISA cluster map** (High-High, Low-Low, High-Low, Low-High, Not significant)  
   - **Results table** (downloadable as CSV)

## Repository structure

```
Geospatial-RAP-1.1/
├── app.R                    # Shiny entry point
├── global.R                 # Packages, config, function sources
├── config.json              # Geographic level definitions
├── R/
│   ├── functions_io.R       # CSV reading, column standardisation, aggregation
│   ├── functions_spatial.R  # Layer loading, join, Global + Local Moran's I
│   ├── functions_plot.R     # Leaflet choropleth and LISA maps
│   ├── mod_geospatial.R     # Shiny UI + server module
│   └── scripts/main.R       # Offline helper (testing only)
├── raw_data/
│   └── Argentina.gpkg       # ← place the geopackage here before running
├── docs/
│   ├── HELP_EN.md
│   └── HELP_ES.md
└── www/
    ├── config.css
    ├── AnalysisforAction_white.png
    └── default.ico
```

## Expected input columns

| Column       | Type      | Description                              |
|:-------------|:----------|:-----------------------------------------|
| `country`    | character | Country name                             |
| `level2`     | character | Department name                          |
| `level3`     | character | Censal fraction code (`cod_indec`)       |
| `year`       | integer   | Epidemiological year                     |
| `week`       | integer   | Epidemiological week                     |
| `n_cases`    | numeric   | Consulted cases in that area-week        |
| `population` | numeric   | Denominator population for the area      |

Column names are case-insensitive and common aliases are accepted  
(e.g. `pop`, `cases`, `count`).

## How to run

1. Place `Argentina.gpkg` inside `raw_data/`.  
2. Install required packages (once):

```r
install.packages(c(
  "jsonlite", "shiny", "shinyjs", "DT", "htmltools",
  "dplyr", "tidyr", "purrr", "rlang", "readr", "lubridate",
  "sf", "spdep", "leaflet", "here"
))
```

3. Open `app.R` in RStudio and click **Run App**, or:

```r
shiny::runApp('.')
```

## Geographic levels

| Level       | Description                                        | Recommended use          |
|:------------|:---------------------------------------------------|:-------------------------|
| Department  | Administrative Level 2 (~500 units nationally)     | Routine surveillance     |
| Censal Fraction | Census sub-area (Level 3, higher resolution)   | Exploratory micro-area   |

## Changelog v1.0 → v1.1

| File                  | Change                                                                             |
|:----------------------|:-----------------------------------------------------------------------------------|
| `global.R`            | Added missing `library(htmltools)` (required by `format_popup_label`)              |
| `functions_io.R`      | Fixed population aggregation: replaced `max(population)` with `first_pop()` (first non-NA value) to avoid inflating the denominator across weeks |
| `functions_spatial.R` | Fixed `compute_lisa()`: replaced `rlang::ensym()` + `!!` (broken for string arguments) with direct `sf_df[[value_col]]` indexing |
| `functions_spatial.R` | Added `compute_global_moran()` for a summary Moran's I statistic                  |
| `functions_spatial.R` | Cleaned up `prepare_results_table()` column names for readability                  |
| `functions_plot.R`    | Switched basemap from OSM tiles to `CartoDB.Positron` (cleaner background for thematic maps) |
| `functions_plot.R`    | LISA legend now drops absent cluster levels (avoids ghost entries)                 |
| `functions_plot.R`    | Popup numbers formatted with `formatC()` / `format()` (thousands separator, 2 decimal places) |
| `mod_geospatial.R`    | Fixed `selected_weeks()` reactive: added branch-specific `req()` calls to prevent NULL errors when `time_mode` switches |
| `mod_geospatial.R`    | Added `tryCatch` wrappers around `prepare_geospatial_dataset()` and `load_geospatial_layer()` to surface errors as Shiny notifications |
| `mod_geospatial.R`    | Added Global Moran's I summary banner above the tab panel                          |
| `mod_geospatial.R`    | LISA column in results table now colour-coded via `DT::formatStyle()`              |
