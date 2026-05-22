# Geospatial RAP Test Suite

For running the test suite, install:

```r
install.packages("testthat")
```

Run the required tests from the project root:

```r
source("tests/testthat.R")
```

The suite validates:

- `config.json` geospatial settings.
- Active-country and default-level helpers.
- Startup pre-flight checks for missing spatial files.
- API-POP CSV import and column alias handling.
- Population being required for geospatial analysis.
- Multiple-file combine behavior and preparation logs.
- Geospatial data preparation and prevalence-per-100,000 calculation.
- Clear errors for missing geographic columns.
- Spatial joins.
- LISA/Global Moran graceful behavior with too few units.
- User-facing labels in maps/tables.

These tests are intended to be run before GitHub pushes and release builds.
