# Geospatial RAP Tool

The Geospatial RAP Tool is a Shiny application for spatial epidemiological analysis. It generates prevalence maps and local spatial cluster outputs using aggregated API-POP-style CSV files.

## Project structure

```text
Geospatial-RAP/
  app.R
    Main Shiny entry point.

  global.R
    Loads required packages, reads configuration, validates startup settings,
    and sources the application files.

  config.json
    Main configuration file. Defines offline mode, active country, default
    country/level/year, target variable, rate multiplier, and country-specific
    spatial mappings.

  R/
    shared/
      Reusable helper functions used by the tool, including API-POP import,
      column standardisation, input validation, configuration helpers, UI helpers,
      file-management helpers, pre-flight checks, and preparation-log utilities.

    geospatial/
      Geospatial-specific spatial processing, GeoPackage loading, spatial joins,
      prevalence/rate calculation, mapping functions, LISA/Moran helpers, plotting
      helpers, file/output helpers, and the main Geospatial Shiny module.

    scripts/
      Developer or maintenance scripts. These are not required during normal app
      execution and should not normally be sourced by `global.R`.

  raw_data/
    Spatial data used by the app, such as configured `.gpkg` files.

  www/
    Static app assets such as CSS, logos, images, icons, and documentation files
    served by Shiny.

  tests/
    TESTING.md
    Detailed testing instructions.
    Test runner and test files for validating import logic, configuration handling,
    pre-flight checks, geospatial preparation, spatial joins, and output helpers.

  README.md
    Project overview and local setup instructions.
```

## Recommended input

The app expects aggregated API-POP-style CSV files with one row per epidemiological week and geographic unit.

Required columns:

- `country`
- `level2`
- `level3`
- `year`
- `week`
- `n_cases`
- `population`

The importer accepts common aliases:

Case count aliases:

- `n_cases`
- `cases`
- `count`
- `n`

Population aliases:

- `population`
- `n_population`
- `pop`
- `denominator`

Population is required for the Geospatial RAP because prevalence and rate-based maps need a denominator.

## Key behavior

- Multiple CSV files can be uploaded and combined.
- Geographic IDs are zero-padded according to `config.json`.
- Users can run a single epidemiological week or a week range.
- Spatial results are joined to the configured GeoPackage layer.
- LISA requires enough spatial units. If too few units are available, LISA is not computed and the app continues without crashing.

## Main outputs

- **Prevalence Map:** observed prevalence per 100,000 for the selected time window.
- **LISA Cluster Map:** local spatial cluster categories.
- **Results Table:** downloadable spatial analysis results.

## Running locally

Open the project folder and run:

```r
shiny::runApp(".")
```
## Required R packages

The app requires the following R packages:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "DT",
  "htmltools",
  "jsonlite",
  "dplyr",
  "tidyr",
  "purrr",
  "rlang",
  "readr",
  "lubridate",
  "ISOweek",
  "stringr",
  "ggplot2",
  "sf",
  "spdep",
  "leaflet",
  "here"
))
```

## Configuration

The active country is read from `config.json`:

```json
{
  "active_country": "Argentina"
}
```

The country block defines:

- GeoPackage file
- available geographic levels
- CSV column mapping
- spatial layer names
- spatial ID columns
- ID code lengths

## Notes for desktop packaging

This tool can be packaged as part of a local desktop application using Electron and a bundled R runtime. If Windows blocks the executable, users may need to allow the app through local security settings or contact IT support.


## Shared input handling

This tool now uses the shared `R/shared/api_pop_io.R` helper used across the RAP tools. CSV upload, delimiter detection, column alias resolution, type conversion, and validation are handled through the same API-POP import workflow used by the companion tool.

## Help and prepared-data audit trail

The app includes a Help tab with data requirements and links to English/Spanish documentation. The Prepared Data tab includes a data preparation log describing key transformations applied to the uploaded CSV before analysis, such as column standardisation, numeric conversion, filtering, overlap resolution, aggregation, and gap filling where applicable.

## Configuration notes

Geospatial mappings support `name_col` in `config.json` to identify the display-name column for each spatial layer. `label_col` is still accepted for backwards compatibility, but `name_col` should be preferred for new country configurations.

## Current configuration notes

The app-level settings at the top of `config.json` are:

- `is_offline`: controls whether the map suppresses external tile providers.
- `active_country`: controls which country configuration is used by default. This can be a single country string, for example `"Argentina"`, or a list such as `["Argentina", "England"]`. When more than one active country is configured, the app shows a Country dropdown.
- `target_var`: controls the analysis variable used by the map, LISA calculation, legend, and results table. The current default is `prevalence_100k`, which is calculated from cases and population.

Each geographic level can define `name_col`. This is the column in the spatial layer used as the display name in maps and tables. `label_col` is still accepted as a backward-compatible fallback, but new country configurations should prefer `name_col`.

If the selected country does not have the configured `.gpkg` file in `raw_data/`, the app shows a clear warning and prevents spatial analysis until the file is added or another configured country is selected.

## Running tests

The project includes a `testthat` test suite for the main Geospatial helper functions, configuration checks, import workflow, and spatial-preparation logic.

For detailed testing instructions, see [Testing](tests/TESTING.md).

Quick run from the project root:

```r
source("tests/testthat.R")
```

## Configuration and pre-flight checks

The geospatial tool reads app-level settings from `config.json`. The most important settings are near the top of the file:

- `is_offline`: controls whether external map tiles are used.
- `active_country`: country or countries enabled in the app. If more than one country is configured, the app shows a country selector.
- `default_country`: fallback country when `active_country` is missing or invalid.
- `default_level`: default geographic level selected in the UI.
- `default_year`: default year selected when present in the uploaded data. Use `null` to select the latest uploaded year.
- `target_var`: calculated analysis variable used for maps, tables, and Moran/LISA analysis.
- `geospatial.rate_multiplier`: denominator multiplier for prevalence/rate calculation. A value of `100000` means prevalence per 100,000 population.

At startup, the app runs a pre-flight configuration check for each active country. It reports clear warnings before the user uploads data when:

- the configured `.gpkg` file is missing;
- a configured layer is missing from the `.gpkg`;
- a configured `spatial_id`, `parent_col`, `name_col`, or fallback `label_col` is missing from the configured layer;
- required config sections such as `file_name`, `levels`, or `mapping` are incomplete.

The warning panel does not prevent the app from opening, but spatial analysis for the affected country will not run until the configuration or geographic data are fixed.

## UI consistency update

The standalone CUSUM and Geospatial RAP tools now use the same sidebar upload pattern: the CSV upload control appears directly in the left-hand input panel, and Help is available as a main output tab. The older top navigation links for Files, Selection, and external Help were removed from CUSUM to align the user experience between tools.

## Uploaded file manager

The upload panel includes a file manager for the current Shiny session. After files are uploaded, users can open **Manage files** to review uploaded files, deactivate files that should not be included in the analysis, remove inactive files, or clear all uploaded files without restarting the app.

All active uploaded files are combined for the Geospatial workflow.

## Project

**Project:** Analysis for Action  – Argentina Work Package WS5
**Institution / Delivery Partner:** CEMIC. 
The Geospatial-RAP was developed at CEMIC, the Argentina Delivery Partner for Analysis for Action (formerly the Pandemic Preparedness Toolkit), a project led by the UK Office for National Statistics (ONS). This work was supported by the Wellcome (Grant number 226596/Z/22/Z).

## Citation

If you use Geospatial-RAP in research, publications, or other work, please cite the software using the following DOI:
Giordano, M. E., Gili, J. A., López-Camelo, J. S.& Poletta, F. A. (2026). Geospatial-RAP: A spatial epidemiology tool for prevalence mapping and local cluster detection (Version v1.2.6) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21894160 

** Geospatial-RAP v1.2.6**  
https://doi.org/10.5281/zenodo.21894160
