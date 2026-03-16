# Core Shiny & Web
# NOTE: shiny must load before jsonlite so shiny::validate() is not masked.
# DT also masks shiny::dataTableOutput/renderDataTable — harmless since we
# use DT:: prefixes everywhere, but listed after shiny for the same reason.
library(shiny)
library(shinyjs)
library(DT)
library(htmltools)
library(jsonlite)   # loads after shiny; jsonlite::validate() masks shiny::validate()
                    # but all calls in mod_geospatial.R are qualified as shiny::validate()

# Data manipulation
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(lubridate)

# Spatial analysis & visualisation
library(sf)
library(spdep)
library(leaflet)

# Utilities
library(here)

sf::sf_use_s2(FALSE)

source("R/functions_io.R")
source("R/functions_spatial.R")
source("R/functions_plot.R")
source("R/mod_geospatial.R")

config_path <- here::here("config.json")
config      <- jsonlite::fromJSON(config_path)

geo_choices        <- c("department", "fraction")
names(geo_choices) <- c("Department", "Censal Fraction")
