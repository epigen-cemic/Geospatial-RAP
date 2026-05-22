testthat::test_that("Geospatial import requires population", {
  raw <- data.frame(
    country = "Argentina",
    level1 = "06",
    level2 = "06014",
    level3 = "0601401",
    year = 2026,
    week = 6,
    cases = 12
  )

  testthat::expect_error(
    api_pop_standardise_columns(raw, config = config, active_country = active_country, require_population = TRUE),
    "Missing required column"
  )
})

testthat::test_that("Geospatial import accepts aliases, pads configured codes, and standardises population", {
  raw <- data.frame(
    Country = "Argentina",
    Province = "6",
    Department = "14",
    Fraction = "1",
    Year = "2026",
    Week = "6",
    count = "12",
    pop = "150000",
    stringsAsFactors = FALSE
  )

  std <- api_pop_standardise_columns(
    raw,
    config = config,
    active_country = "Argentina",
    require_population = TRUE
  )

  testthat::expect_true(all(c("country", "level1", "level2", "level3", "year", "week", "n_cases", "population") %in% names(std)))
  testthat::expect_equal(std$n_cases, 12)
  testthat::expect_equal(std$population, 150000)
  testthat::expect_type(std$year, "integer")
  testthat::expect_type(std$week, "integer")
  testthat::expect_true(length(api_pop_get_log(std)) > 0)
  testthat::expect_null(api_pop_validate_data(std, require_population = TRUE, target_col = "level2"))
})

testthat::test_that("combined upload retains file index and preparation log", {
  file_one <- tempfile(fileext = ".csv")
  file_two <- tempfile(fileext = ".csv")

  writeLines(c(
    "country,level1,level2,level3,year,week,cases,population",
    "Argentina,6,14,1,2026,6,12,150000"
  ), file_one)
  writeLines(c(
    "country,level1,level2,level3,year,week,cases,population",
    "Argentina,6,14,1,2026,7,15,150000"
  ), file_two)

  combined <- api_pop_combine_files(
    c(file_one, file_two),
    config = config,
    active_country = "Argentina",
    require_population = TRUE
  )

  testthat::expect_equal(nrow(combined), 2)
  testthat::expect_true("file_index" %in% names(combined))
  testthat::expect_equal(sort(unique(combined$file_index)), c(1, 2))
  testthat::expect_true(any(grepl("Read 2 uploaded file", api_pop_get_log(combined))))
})
