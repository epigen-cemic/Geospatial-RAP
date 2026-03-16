# Geospatial-RAP – User Help

## What this app does

This app uses the same API-Pop aggregated CSV input as the CUSUM-RAP, but instead of running a temporal early-warning algorithm, it performs a simple geospatial analysis.

The app:
- calculates consultation-based prevalence per 100,000,
- displays a choropleth prevalence map,
- computes Local Moran's I (LISA),
- and displays a LISA cluster map.

## User inputs

1. Upload one or more API-Pop CSV files.
2. Select the geographic level:
   - Department
   - Censal Fraction
3. Select a year.
4. Select either:
   - one epidemiological week, or
   - a week range.
5. Click **Run Spatial Analysis**.

## Outputs

- Prevalence Map
- LISA Cluster Map
- Results Table (downloadable as CSV)
