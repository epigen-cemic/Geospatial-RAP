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

# 4. Load the silhouette using the level1 key
  # 4.1. Dynamically find the key used for Level 1 (e.g., "province")
level1_key <- names(country_mappings)[sapply(country_mappings, function(x) {
  x$csv_column == "level1"
})]

  # 4.2. Load the background silhouette using the correct key
bg_layer <- load_geospatial_layer(
  selected_level = level1_key[1], 
  config         = config, 
  active_country = active_country
)

# 5. Identify which keys correspond to level 2 and level 3
valid_keys <- names(country_mappings)[sapply(country_mappings, function(x) {
  x$csv_column %in% c("level2", "level3")
})]

# 6. Create the vector using the keys (internal names)
geo_choices <- valid_keys 

# 7. NOW assign the names for the UI dropdown
names(geo_choices) <- sapply(valid_keys, function(k) {
  cfg <- country_mappings[[k]]
  
  if (!is.null(cfg$ui_label)) {
    return(cfg$ui_label)
  } else {
    return(cfg$layer_name)
  }
})