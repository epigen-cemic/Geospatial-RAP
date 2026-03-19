# Geospatial Build Data Pipeline

## Overview
This folder contains "pre-baked" spatial objects (`.rds` files) used by the application. Instead of reading raw Geopackages at runtime, the app loads these optimized files to ensure high performance and offline compatibility.

## Why we use this:
1. **Speed:** Loading an `.rds` file is ~10x faster than querying a Geopackage.
2. **Memory:** Polygons are simplified to remove unnecessary detail, preventing browser crashes when displaying 50,000+ areas (e.g., Fractions).
3. **Standardization:** Columns are pre-renamed to `id` and `label`, ensuring the app logic never breaks.

## How to add a new country:
1. Add the raw `.gpkg` file to the `raw_data/` folder.
2. Update the `config.json` with the new country's layers and column names.
3. Open `optimize_spatial_data.R` and change `active_country <- "NewCountryName"`.
4. Run the script. It will generate new `.rds` files in this folder.
5. The App will now automatically detect and use these optimized files.

*Note: Do not delete these files unless you plan to re-run the optimization script.*