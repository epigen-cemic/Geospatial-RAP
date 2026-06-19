# User Manual

**Geospatial RAP Tool**  
Prevalence mapping and spatial-cluster analysis  
Documentation revision: June 2026  
Language: English

---

# 1. Purpose and scope

The Geospatial RAP Tool aggregates cases and population for a selected country, administrative level, year, and epidemiological week window. It produces prevalence maps, a Global Moran's I summary, and Local Moran's I (LISA) cluster classifications.

> **All units are analysed together.** The standalone application does not require selecting each commune, department, or other location separately. After a geographic level is chosen, all matching units available in the active data are processed in the same run.

> **Interpretation.** Spatial statistics describe patterns in the selected dataset. They do not establish causation, identify a source of transmission, or replace local epidemiological investigation.


# 2. Input data and spatial requirements

Upload one or more API-POP-compatible CSV files. Comma- and semicolon-delimited files are supported. Population is required because prevalence is calculated as cases divided by population times the configured rate multiplier, currently 100,000.

| Internal column | Required | Meaning | Accepted aliases or examples |
| --- | --- | --- | --- |
| year | Yes | Epidemiological year. | year, anio, año, epiyear |
| week | Yes | Epidemiological week. | week, semana, epiweek, se |
| country | Recommended | Country or national unit. | Argentina or England |
| level1 | Depends on selected level | First administrative level. | province, region, Level1 |
| level2 | Required for second-level maps | Second administrative level. | department, district, LAD, Level2 |
| level3 / level4 | Required for detailed maps | More detailed administrative levels. | fraction, MSOA, Level3 |
| n_cases | Yes | Observed cases or event count. | n_cases, cases, case_count, count, n |
| population | Yes | Population denominator used to calculate prevalence. | population, pop, denominator |

The application also requires a configured GeoPackage for the active country. Administrators map each geographic level to a GeoPackage layer, a spatial identifier, and a display-name column in config.json.

> **Identifier matching.** Geographic identifiers in the CSV must match either the configured map IDs or labels after standardisation and zero padding. Leading zeros are significant.


# 3. Configure and run

1. Upload one or more CSV files.
2. Use Manage files to activate, deactivate, remove, or clear files.
3. Select a Country when several are configured.
4. Choose the Geographic Level.
5. Choose Single Week or Week Range.
6. Select the Year and epidemiological week or range.
7. Select Run Analysis.

| Control | Current behavior |
| --- | --- |
| Manage files | All active files are combined. Deactivating a file removes it from the current analysis without deleting the original upload. |
| Geographic Level | Selects the administrative level and its configured GeoPackage layer. |
| Time Mode - Single Week | Uses one epidemiological week. |
| Time Mode - Week Range | Aggregates cases over the selected inclusive range. |
| Year | Limits the analysis to one epidemiological year. |
| Run Analysis | Prepares the data, checks spatial configuration, loads geometry, joins records, computes LISA, and refreshes maps and tables. |

For week ranges, cases are summed across weeks. Population is treated as a static denominator: detailed geographic records are deduplicated before population is rolled up to the selected level, preventing repeated weekly population values from being added multiple times.


# 4. Review the outputs

## Global Moran's I banner

The banner above the maps reports Global Moran's I and its p-value. A significant positive value suggests nearby areas tend to have similar prevalence; a value near zero suggests little overall spatial pattern; a negative value suggests neighbouring areas tend to differ.

## Prevalence Map

The choropleth displays the configured prevalence measure, normally cases per 100,000 people. The legend remains visible. Popups identify the area, map identifier, prevalence, LISA category, and p-value when available.

## LISA Cluster Map

| Category | Meaning |
| --- | --- |
| High-High | An area with high prevalence surrounded by areas with high prevalence; a local hotspot pattern. |
| Low-Low | A low-prevalence area surrounded by low-prevalence areas. |
| High-Low | A high-prevalence area surrounded by low-prevalence areas; a spatial outlier. |
| Low-High | A low-prevalence area surrounded by high-prevalence areas; an inverse outlier. |
| Not Significant | The local pattern did not meet the current p <= 0.05 significance rule. |

## Results Table

The results table contains identifiers, names, cases, population, weeks included, prevalence, LISA category, and p-value as available. Use Search by to select a text or numeric column and filter the display.

## Prepared Data

Shows the aggregated dataset before geometry display, plus the data-preparation log. It records time filtering, case aggregation, population deduplication, prevalence calculation, and the number of prepared spatial units.

> **Display settings.** The Help tab can increase interface font size and change table decimal places. Downloaded CSV values retain full precision.


# 5. Interpret results responsibly

- Global Moran's I summarizes the entire selected map; it does not identify which individual areas form clusters.
- LISA provides local categories, but multiple testing and sparse data should be considered during interpretation.
- A significant cluster may reflect surveillance intensity, population structure, access to services, or reporting practices as well as underlying risk.
- Compare maps across justified time windows rather than choosing a window only because it produces a desired pattern.

Before acting on a hotspot or outlier, check the numerator, population denominator, identifier match, selected weeks, and whether neighbouring polygons are epidemiologically meaningful for the question.


# 6. Downloads

| Download | Contents |
| --- | --- |
| Download Results CSV | Tabular geospatial results for all matched units. |
| Download Prepared Data CSV | Aggregated input used for the spatial join and prevalence calculation. |
| Map export, where enabled | Image of the current interactive map. Availability depends on the screenshot packages installed in the deployment. |

On-screen filters and decimal formatting do not change the underlying downloaded result values.


# 7. Troubleshooting

| Problem | What to check |
| --- | --- |
| Geographic data are not available | The configured raw_data/<file_name>.gpkg file is missing. |
| Layer or column pre-flight error | Check layer_name, spatial_id, name_col or label_col in config.json. |
| Spatial Join Failure | CSV codes do not match map IDs or labels; check leading zeros, level selection, and code length. |
| No data available | The active files may not contain the selected year or weeks. |
| No Global Moran banner | Fewer than four matched units, all values missing, zero variance, or an invalid neighbour structure. |
| LISA values are blank | The same sparse/constant-data conditions can prevent local analysis. |
| Population error | Population is missing, zero, invalid, or mapped under an unrecognised alias. |

> **Escalation package.** Include the selected country, level, year, week window, active file names, preparation log, pre-flight message, and a small non-sensitive sample of the geographic IDs.


# 8. Glossary

| Term | Meaning |
| --- | --- |
| Prevalence per 100,000 | Cases divided by population, multiplied by 100,000. |
| Global Moran's I | Whole-map measure of spatial autocorrelation. |
| Local Moran's I / LISA | Local statistic used to classify clusters and spatial outliers. |
| Spatial weights | Row-standardised neighbour relationships derived from polygon contiguity. |
| GeoPackage | Spatial database file containing configured polygon layers. |
| Spatial join | Match between prepared analysis_unit_id values and configured map IDs or labels. |

