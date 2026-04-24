test_that("generate_new_data() returns a tibble", {
  skip_if_not_installed("f1dataR")
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  # Create minimal test data
  test_drivers <- data.frame(
    driver_id = "max_verstappen",
    constructor_id = "red_bull"
  )

  # Use clean_data for mock historical data
  historical_data <- load_all_data()
  historical_data <- f1predicter::clean_data(historical_data)

  # Get a valid season and round
  season <- max(historical_data$season)
  round <- 1

  result <- generate_new_data(
    season = season,
    round = round,
    drivers = test_drivers,
    historical_data = historical_data
  )

  # Should return a tibble
  expect_s3_class(result, "tbl_df")

  # Should have at least one row
  expect_gte(nrow(result), 1)

  # Should have essential columns
  expect_true("driver_id" %in% names(result))
  expect_true("constructor_id" %in% names(result))
  expect_true("season" %in% names(result))
  expect_true("round" %in% names(result))
})

test_that("generate_new_data() errors on invalid season/round", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  test_drivers <- data.frame(
    driver_id = "max_verstappen",
    constructor_id = "red_bull"
  )

  # Use invalid season/round
  expect_error(
    generate_new_data(season = 9999, round = 1, drivers = test_drivers)
  )
})

test_that("generate_new_data() applies grid penalties", {
  skip_if_not_installed("f1dataR")
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  historical_data <- f1predicter::clean_data()
  season <- max(historical_data$season)

  penalties <- c("max_verstappen" = 5)

  suppressWarnings(
    result_norm <- generate_new_data(
      season = 2025,
      round = 1,
      historical_data = historical_data
    )
  )

  suppressWarnings(
    result_penalty <- generate_new_data(
      season = 2025,
      round = 1,
      historical_data = historical_data,
      penalties = penalties
    )
  )

  # Verify result was generated with penalty to Max Verstappen
  # Also promoted Russell to 3rd
  expect_equal(
    as.integer(result_norm[result_norm$driver_id == "max_verstappen", "grid"]),
    as.integer(result_penalty[
      result_penalty$driver_id == "max_verstappen",
      "grid"
    ]) -
      5
  )

  expect_equal(
    as.integer(result_norm[result_norm$driver_id == "russell", "grid"]),
    as.integer(result_penalty[result_penalty$driver_id == "russell", "grid"]) +
      1
  )
})

test_that("build_feature_set() includes numeric features", {
  skip_if_not_installed("f1dataR")
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  # Using generate_new_data as it builds the feature set
  test_drivers <- data.frame(
    driver_id = "max_verstappen",
    constructor_id = "red_bull"
  )

  historical_data <- f1predicter::clean_data()
  season <- max(historical_data$season)

  result <- generate_new_data(
    season = season,
    round = 1,
    drivers = test_drivers,
    historical_data = historical_data
  )

  # Check for numeric features
  numeric_cols <- sapply(result, is.numeric)
  expect_true(any(numeric_cols), "Should have numeric feature columns")
})
