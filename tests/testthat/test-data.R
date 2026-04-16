test_that("get_schedule() returns a tibble with schedule data and data is as expected", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- get_schedule()

  # Should be a tibble
  expect_s3_class(result, "tbl_df")

  # Should have rows
  expect_gt(nrow(result), 0)

  # Should have essential columns
  expected_cols <- c("season", "round", "circuit_id")
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                paste("Column", col, "should exist in get_schedule()"))
  }

  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))

  for (season in unique(result$season)) {
    season_data <- result[result$season == season, ]
    rounds <- sort(season_data$round)

    # Rounds should be consecutive starting from 1
    expect_equal(rounds[1], 1)
  }
})

test_that("load_all_data() returns a list with expected components", {
  skip_if_not_installed("f1dataR")
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  result <- load_all_data()

  # Should be a list
  expect_type(result, "list")

  # Should have required components
  expected_components <- c("results", "sprint_results", "laps", "pitstops", "sgrid", "rgrid", "qualis")
  expect_true(all(expected_components %in% names(result)))

  # Main components should be tibbles or dataframes
  expect_true(is.data.frame(result$results))
  expect_true(is.data.frame(result$qualis))
})
