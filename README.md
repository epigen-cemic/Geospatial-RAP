# Geospatial RAP Tool (WS5.WP1.2)

The Geospatial RAP (Reproducible Analytical Pipeline) Tool is a Shiny application for spatial epidemiological analysis. It generates prevalence maps and spatial clustering outputs (LISA) using aggregated case data.

## Ecosystem Workflow

This tool is part of the RAP ecosystem and is designed to run sequentially:

1. **Data Collection:** Individual-level suspected case data is collected via RapidCase Report
2. **Aggregation:** Data is aggregated by week and spatial unit using API-POP
3. **Spatial Analysis:** The aggregated CSV output is used as input for this Geospatial RAP Tool

## Deployment & Setup

This application does not require database credentials or environment configuration. It operates entirely on uploaded CSV files.

The tool can be distributed as:

* A standalone desktop application (Electron)
* A web application (shinyapps.io or Shiny Server)

## Documentation

For detailed usage, configuration, and deployment instructions, refer to:

* 🇬🇧 `www/docs/HELP_EN.md`
* 🇪🇸 `www/docs/HELP_ES.md`

End-users can access the full user manual inside the application via the **Help** button.
