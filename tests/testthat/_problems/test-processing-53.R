# Extracted from test-processing.R:53

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "f1predicter", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
result <- get_weekend_data(2023, 1)
