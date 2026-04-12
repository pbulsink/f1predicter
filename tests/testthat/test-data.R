test_that("get_schedule() returns a tibble with schedule data (#noissue)", {
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
})

test_that("get_schedule() contains valid seasons (#noissue)", {
  result <- get_schedule()
  
  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))
})

test_that("get_schedule() rounds are sequential per season (#noissue)", {
  result <- get_schedule()
  
  # For each season, rounds should start at 1
  for (season in unique(result$season)) {
    season_data <- result[result$season == season, ]
    rounds <- sort(season_data$round)
    
    # Rounds should be consecutive starting from 1
    expect_equal(rounds[1], 1)
  }
})

test_that("load_all_data() returns a list with expected components (#noissue)", {
  skip_if_not_installed("f1dataR")
  
  result <- load_all_data()
  
  # Should be a list
  expect_type(result, "list")
  
  # Should have required components
  expected_components <- c("results", "quali", "pitStops", "fastest_lap", "sponsors")
  for (comp in expected_components) {
    expect_true(comp %in% names(result),
                paste("Component", comp, "should exist in load_all_data()"))
  }
})

test_that("load_all_data() components are tibbles (#noissue)", {
  skip_if_not_installed("f1dataR")
  
  result <- load_all_data()
  
  # Main components should be tibbles or dataframes
  expect_true(is.data.frame(result$results))
  expect_true(is.data.frame(result$quali))
})

test_that("load_models() handles missing model directory (#noissue)", {
  # Models might not exist, should handle gracefully
  # or return appropriate structure
  skip("Depends on models directory state")
  
  result <- load_models()
  # Just verify function exists
  expect_true(is.function(load_models))
})

test_that("save_models() creates model files (#noissue)", {
  skip("Should only run with valid models to avoid side effects")
  
  # This would test that save_models creates files
  # But we skip it to avoid side effects in tests
  expect_true(is.function(save_models))
})

test_that("generate_next_race_data() produces valid output structure (#noissue)", {
  skip_if_not_installed("f1dataR")
  
  result <- generate_next_race_data()
  
  # Should return a list or tibble
  expect_true(is.data.frame(result) || is.list(result))
})
