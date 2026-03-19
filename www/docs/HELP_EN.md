# Geospatial RAP Tool: Developer & Administrator Guide

This guide is intended for developers, IT administrators, and maintainers of the Geospatial RAP Tool. End-users should refer to the User Manual available within the application.

---

## 1. Project Structure

The repository follows a modular Shiny architecture.

* Root directory: main scripts (`app.R`, `global.R`, `config.json`)
* `R/`: core logic and modules
* `data/`: optimized spatial layers (`.rds`)
* `build_data/`: preprocessing scripts
* `www/`: UI assets and documentation

---

## 2. Running the App Locally

1. Open the project in RStudio
2. Open `app.R`
3. Run:

shiny::runApp()

---

## 3. Deployment

### Option A: shinyapps.io

* Install `rsconnect`
* Deploy using `rsconnect::deployApp()`

### Option B: Self-hosted

* Shiny Server (Linux)
* Docker containerization

### Option C: Desktop (Electron)

* Bundles R + app into standalone executable
* No user setup required

---

## 4. Configuration

### 4.1 Geographic Configuration (`config.json`)

Defines:

* Administrative levels
* Mapping between CSV and spatial layers
* Spatial IDs and labels

Supports multi-country setups without modifying core code.

---

### 4.2 UI Customization (`config.css`)

Controls:

* Layout and colors
* Sidebar appearance
* Map background

---

### 4.3 Offline Spatial Data

Optimized `.rds` layers are stored in:

data/

To generate them:

build_data/optimize_spatial_data.R

---

## 5. Data Pipeline

The application:

* Reads API-POP CSV files
* Standardizes column names
* Pads geographic identifiers
* Aggregates data
* Performs spatial joins

---

## 6. Spatial Analysis Engine

* Global Moran’s I
* Local Moran’s I (LISA)
* Handles sparse data using safe defaults

---

## 7. Troubleshooting

* Missing polygons → check spatial IDs
* Empty maps → verify configuration
* Performance issues → ensure `.rds` usage

---

## 8. Documentation

* User Manual: `www/docs/Documentation.pdf`
* Spanish version: `HELP_ES.md`
