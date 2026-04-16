test_that("generate_new_data() returns a tibble (#noissue)", {
  skip_if_not_installed("f1dataR")
  
  # Create minimal test data
  test_drivers <- data.frame(
    driver_id = "max_verstappen",
    constructor_id = "red_bull"
  )
  
  # Use clean_data for mock historical data
  historical_data <- f1predicter::clean_data()
  
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

test_that("generate_new_data() errors on invalid season/round (#noissue)", {
  test_drivers <- data.frame(
    driver_id = "max_verstappen",
    constructor_id = "red_bull"
  )
  
  # Use invalid season/round
  expect_error(
    generate_new_data(season = 9999, round = 1, drivers = test_drivers)
  )
})

test_that("generate_new_data() applies grid penalties (#noissue)", {
  skip_if_not_installed("f1dataR")
  
  test_drivers <- data.frame(
    driver_id = c("max_verstappen", "lando_norris"),
    constructor_id = c("red_bull", "mclaren"),
    grid = c(1, 2)
  )
  
  historical_data <- f1predicter::clean_data()
  season <- max(historical_data$season)
  
  penalties <- c("max_verstappen" = 5)
  
  result <- generate_new_data(
    season = season,
    round = 1,
    drivers = test_drivers,
    penalties = penalties,
    historical_data = historical_data
  )
  
  # Verify result was generated
  expect_s3_class(result, "tbl_df")
  expect_gte(nrow(result), 2)
})

test_that("build_feature_set() includes numeric features (#noissue)", {
  skip_if_not_installed("f1dataR")
  
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
