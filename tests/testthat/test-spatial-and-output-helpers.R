make_test_sf <- function(n = 3, values = seq_len(n)) {
  polys <- lapply(seq_len(n), function(i) {
    x <- i - 1
    sf::st_polygon(list(matrix(
      c(x, 0,
        x + 1, 0,
        x + 1, 1,
        x, 1,
        x, 0),
      ncol = 2,
      byrow = TRUE
    )))
  })

  sf::st_sf(
    id = sprintf("ID%s", seq_len(n)),
    label = sprintf("Area %s", seq_len(n)),
    n_cases = values,
    population = rep(1000, n),
    prevalence_100k = values,
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

testthat::test_that("variable labels are user friendly", {
  testthat::expect_equal(pretty_var_label("prevalence_100k"), "Prevalence per 100,000")
  testthat::expect_equal(pretty_var_label("n_cases"), "Cases")
  testthat::expect_equal(pretty_var_label("custom_metric"), "Custom metric")
})

testthat::test_that("analysis data joins to geometry by ID", {
  sf_layer <- make_test_sf(2)
  analysis_df <- data.frame(
    analysis_unit_id = c("ID1", "ID2"),
    n_cases = c(5, 10),
    population = c(1000, 2000),
    prevalence_100k = c(500, 500)
  )

  joined <- join_analysis_to_geometry(sf_layer[, c("id", "label", "geometry")], analysis_df)

  testthat::expect_s3_class(joined, "sf")
  testthat::expect_equal(joined$n_cases, c(5, 10))
  testthat::expect_equal(joined$prevalence_100k, c(500, 500))
})

testthat::test_that("LISA and Global Moran fail gracefully with too few units", {
  sf_data <- make_test_sf(3)

  testthat::expect_warning(
    lisa <- compute_lisa(sf_data, target_var = "prevalence_100k"),
    "minimum 4 required"
  )

  testthat::expect_true("lisa_category" %in% names(lisa))
  testthat::expect_true("p_value" %in% names(lisa))
  testthat::expect_true(all(is.na(lisa$p_value)))
  testthat::expect_null(compute_global_moran(sf_data, target_var = "prevalence_100k"))
})

testthat::test_that("results table uses explicit prevalence label", {
  sf_data <- make_test_sf(3)
  sf_data$lisa_category <- factor("Not Significant", levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant"))
  sf_data$p_value <- 0.5

  tbl <- prepare_results_table(sf_data, "prevalence_100k")

  testthat::expect_false(inherits(tbl, "sf"))
  testthat::expect_true("Prevalence per 100,000" %in% names(tbl))
  testthat::expect_true("LISA_cluster" %in% names(tbl))
})
