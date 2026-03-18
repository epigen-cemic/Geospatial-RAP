# Core Shiny & Web
# NOTE: shiny must load before jsonlite so shiny::validate() is not masked.
# DT also masks shiny::dataTableOutput/renderDataTable — harmless since we
# use DT:: prefixes everywhere, but listed after shiny for the same reason.
library(shiny)
library(shinyjs)
library(DT)
library(htmltools)
library(jsonlite)   

# Data manipulation
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(lubridate)
library(stringr)

# Spatial analysis & visualisation
library(sf)
library(spdep)
library(leaflet)

# Turn off s2 geometry for planar compatibility
sf::sf_use_s2(FALSE)

# Source modules and functions
source("R/functions_io.R")
source("R/functions_spatial.R")
source("R/functions_plot.R")
source("R/mod_geospatial.R")

# Load configuration
config_path <- "config.json"
config      <- jsonlite::fromJSON(config_path)

# 3. THE CONTROL CENTER (Parameters)
# Keep these together so they are easy to find!
active_country <- "Argentina"
target_var     <- "prevalence_100k"
is_offline     <- FALSE

# Dynamically build geo_choices from the config
country_mappings <- config[[active_country]]$mapping

# 1. Identify which keys correspond to level 2 and level 3
valid_keys <- names(country_mappings)[sapply(country_mappings, function(x) {
  x$csv_column %in% c("level2", "level3")
})]

# 2. Build the named vector for the Shiny UI dropdown
geo_choices <- valid_keys
names(geo_choices) <- sapply(valid_keys, function(k) {
  # Uses the layer_name (e.g., "Department") as the UI display text
  country_mappings[[k]]$layer_name 
})