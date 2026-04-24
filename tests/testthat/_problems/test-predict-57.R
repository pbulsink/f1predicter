# Extracted from test-predict.R:57

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "f1predicter", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("f1dataR")
withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
historical_data <- f1predicter::clean_data()
