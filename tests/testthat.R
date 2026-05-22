# Required test suite for Geospatial RAP
#
# Run from the project root with:
#   source("tests/testthat.R")
#
# These tests source the application helpers through global.R and then execute
# all tests in tests/testthat/.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run the test suite. Install it with install.packages('testthat').")
}

source("global.R")

testthat::test_dir("tests/testthat", reporter = "summary")
