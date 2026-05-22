testthat::test_that("geospatial config exposes required top-level settings", {
  testthat::expect_true(is.list(config))

  required_keys <- c(
    "is_offline",
    "active_country",
    "default_country",
    "default_level",
    "target_var",
    "geospatial"
  )

  testthat::expect_true(all(required_keys %in% names(config)))
  testthat::expect_equal(rap_config_value(config, "target_var"), "prevalence_100k")
  testthat::expect_equal(rap_geospatial_rate_multiplier(config), 100000)
})

testthat::test_that("active country and level helpers return valid configured values", {
  countries <- rap_active_countries(config)
  testthat::expect_true(length(countries) >= 1)
  testthat::expect_true(all(countries %in% rap_available_countries(config)))

  country <- rap_default_country(config)
  testthat::expect_true(country %in% names(config))

  choices <- rap_geospatial_choices(config, country)
  testthat::expect_true(length(choices) > 0)
  testthat::expect_true(rap_default_geospatial_level(config, country) %in% unname(choices))
})

testthat::test_that("pre-flight check reports missing Geopackage clearly", {
  tmp_dir <- tempfile("missing_gpkg_dir")
  dir.create(tmp_dir)

  local_config <- config
  local_config$active_country <- "England"
  local_config$default_country <- "England"

  result <- rap_geospatial_preflight(local_config, countries = "England", data_dir = tmp_dir)
  messages <- rap_format_preflight_messages(result$England)

  testthat::expect_true(length(messages) > 0)
  testthat::expect_true(any(grepl("Geopackage", messages, ignore.case = TRUE)))
})

testthat::test_that("pre-flight result object has errors and warnings fields", {
  result <- rap_geospatial_preflight(config, countries = rap_active_countries(config))
  testthat::expect_true(is.list(result))

  for (country in names(result)) {
    testthat::expect_true(all(c("errors", "warnings") %in% names(result[[country]])))
    testthat::expect_type(result[[country]]$errors, "character")
    testthat::expect_type(result[[country]]$warnings, "character")
  }
})
