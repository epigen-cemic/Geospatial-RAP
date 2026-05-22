testthat::test_that("geospatial preparation calculates prevalence per configured multiplier", {
  raw <- data.frame(
    country = "Argentina",
    level1 = c("06", "06", "06"),
    level2 = c("06014", "06014", "06014"),
    level3 = c("0601401", "0601401", "0601402"),
    year = c(2026L, 2026L, 2026L),
    week = c(6L, 7L, 6L),
    n_cases = c(10, 5, 20),
    population = c(100000, 100000, 50000)
  )

  raw <- api_pop_set_log(raw, "Synthetic test dataset.")

  prepared <- prepare_geospatial_dataset(
    df = raw,
    selected_level = "department",
    selected_year = 2026,
    week_from = 6,
    week_to = 7,
    config = config,
    active_country = "Argentina"
  )

  testthat::expect_equal(nrow(prepared), 1)
  testthat::expect_equal(prepared$n_cases, 35)
  testthat::expect_equal(prepared$population, 150000)
  testthat::expect_equal(prepared$prevalence_100k, (35 / 150000) * 100000)
  testthat::expect_true(any(grepl("Calculated prevalence_100k", api_pop_get_log(prepared))))
})

testthat::test_that("geospatial preparation returns NULL for empty selected time window", {
  raw <- data.frame(
    country = "Argentina",
    level1 = "06",
    level2 = "06014",
    level3 = "0601401",
    year = 2026L,
    week = 6L,
    n_cases = 10,
    population = 100000
  )

  out <- prepare_geospatial_dataset(
    df = raw,
    selected_level = "department",
    selected_year = 2025,
    week_from = 1,
    week_to = 2,
    config = config,
    active_country = "Argentina"
  )

  testthat::expect_null(out)
})

testthat::test_that("geospatial preparation errors clearly when selected level column is missing", {
  raw <- data.frame(
    country = "Argentina",
    level1 = "06",
    year = 2026L,
    week = 6L,
    n_cases = 10,
    population = 100000
  )

  testthat::expect_error(
    prepare_geospatial_dataset(
      df = raw,
      selected_level = "department",
      selected_year = 2026,
      week_from = 6,
      week_to = 6,
      config = config,
      active_country = "Argentina"
    ),
    "not present in the uploaded data"
  )
})
