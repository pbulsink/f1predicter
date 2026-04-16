test_that("clean_data() returns a tibble with expected structure and valid data", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- clean_data()

  # Should be a tibble
  expect_s3_class(result, "tbl_df")

  # Should have rows
  expect_gt(nrow(result), 0)

  # Should have essential columns
  expected_cols <- c(
    "season", "round", "driver_id", "constructor_id",
    "grid", "position", "points", "finished"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                paste("Column", col, "should exist in clean_data()"))
  }

  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))

  # Grid and position should be positive (or zero/NA)
  expect_true(all(result$grid >= 0 | is.na(result$grid)))
  expect_true(all(result$position >= 0 | is.na(result$position)))

  # Finished should be 0, 1, or NA
  valid_vals <- c(0, 1, NA)
  expect_true(all(result$finished %in% valid_vals | is.na(result$finished)))

  # Points should be non-negative
  expect_true(all(result$points >= 0 | is.na(result$points)))
})

test_that("get_season_data() errors on invalid season", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  # Should error on future season or invalid year
  expect_error(get_season_data(9999))
})

test_that("get_weekend_data() returns data for valid race", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- get_weekend_data(2023, 1)

  expect_type(result, "list")
  result <- result$results
  expect_s3_class(result, 'data.frame')
  expect_gt(nrow(result), 0)
  expect_true(all(result$round == 1))
})

test_that("get_weekend_data() errors on invalid round", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  expect_error(get_weekend_data(2023, 99))
})
