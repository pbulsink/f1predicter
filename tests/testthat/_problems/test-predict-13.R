# Extracted from test-predict.R:13

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "f1predicter", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("f1dataR")
withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
test_drivers <- data.frame(
  driver_id = "max_verstappen",
  constructor_id = "red_bull"
)
historical_data <- load_all_data()
historical_data <- f1predicter::clean_data(historical_data)
