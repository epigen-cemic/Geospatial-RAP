# ==============================================================================
# SCRIPT: optimize_spatial_data.R
# PURPOSE: One-time optimization of Geopackage layers for high-performance 
#          offline mapping. Simplifies geometry and standardizes schema.
# ==============================================================================

library(sf)
library(dplyr)
library(jsonlite)

# --- Configuration ---
active_country <- "Argentina"
config         <- fromJSON("config.json")
input_gpkg     <- file.path("raw_data", config[[active_country]]$file_name)
output_dir <- "data"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir)

# --- Processing Logic ---
levels_to_process <- names(config[[active_country]]$mapping)

cat(sprintf("--- Starting Optimization for %s ---\n", active_country))

for (lvl in levels_to_process) {
  cat(sprintf("Processing level: %s... ", lvl))
  
  # 1. Get metadata from config
  lvl_cfg <- config[[active_country]]$mapping[[lvl]]
  
  # 2. Read layer with standardized SQL
  # We select only the columns defined in the JSON
  sql <- sprintf("SELECT %s AS id, %s AS label, geom FROM %s", 
                 lvl_cfg$spatial_id, lvl_cfg$label_col, lvl_cfg$layer_name)
  
  layer <- st_read(input_gpkg, query = sql, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(4326)
  
  # 3. Geometric Simplification (The "Secret Sauce")
  # 0.0005 is a good balance for 50k+ polygons. 
  # It removes micro-curves but keeps borders touching perfectly.
  if (nrow(layer) > 5000) {
    cat("(Simplifying complex geometry) ")
    layer <- st_simplify(layer, dTolerance = 0.0005, preserveTopology = TRUE)
  }
  
  # 4. Save as RDS (Serialized R Object)
  output_file <- file.path(output_dir, paste0(lvl, "_optimized.rds"))
  saveRDS(layer, output_file)
  
  cat("Done! Saved to", output_file, "\n")
}

cat("--- All levels optimized successfully ---\n")