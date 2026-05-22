# Core Shiny & Web
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

# Turn off s2 geometry for planar compatibility.
sf::sf_use_s2(FALSE)

# Load shared helpers before reading app-level settings.
source("R/shared/config_helpers.R")
source("R/shared/api_pop_io.R")

# Load configuration.
config_path <- "config.json"
config <- jsonlite::fromJSON(config_path)

# App-level settings from config.json.
is_offline <- isTRUE(rap_config_value(config, "is_offline", FALSE))
active_countries <- rap_active_countries(config)
active_country <- rap_default_country(config)
target_var <- rap_config_value(config, "target_var", "prevalence_100k")
default_level <- rap_config_value(config, "default_level", NULL)
default_year <- rap_default_year(config)
rate_multiplier <- rap_geospatial_rate_multiplier(config)
preflight_results <- rap_geospatial_preflight(config, active_countries)

# Source modules and functions.
source("R/geospatial/functions_io.R")
source("R/geospatial/functions_spatial.R")
source("R/geospatial/functions_plot.R")
source("R/geospatial/mod_geospatial.R")
source("R/help_ui.R")

