test_that("clean_data() returns a tibble with expected structure (#noissue)", {
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
})

test_that("clean_data() contains only valid seasons (#noissue)", {
  result <- clean_data()
  
  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))
})

test_that("clean_data() has valid grid and position values (#noissue)", {
  result <- clean_data()
  
  # Grid and position should be positive (or zero/NA)
  expect_true(all(result$grid >= 0 | is.na(result$grid)))
  expect_true(all(result$position >= 0 | is.na(result$position)))
})

test_that("clean_data() finished column is binary or NA (#noissue)", {
  result <- clean_data()
  
  # Finished should be 0, 1, or NA
  valid_vals <- c(0, 1, NA)
  expect_true(all(result$finished %in% valid_vals | is.na(result$finished)))
})

test_that("clean_data() points are non-negative (#noissue)", {
  result <- clean_data()
  
  # Points should be non-negative
  expect_true(all(result$points >= 0 | is.na(result$points)))
})

test_that("get_season_data() returns data for valid season (#noissue)", {
  result <- get_season_data(2023)
  
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(result$season == 2023))
})

test_that("get_season_data() errors on invalid season (#noissue)", {
  # Should error on future season or invalid year
  expect_error(get_season_data(9999))
})

test_that("get_weekend_data() returns data for valid race (#noissue)", {
  result <- get_weekend_data(2023, 1)
  
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(result$round == 1))
})

test_that("get_weekend_data() errors on invalid round (#noissue)", {
  expect_error(get_weekend_data(2023, 99))
})

test_that("normalize_vector() integrates with data processing (#noissue)", {
  # Test that predictions sum to reasonable values
  data <- clean_data()
  
  if (nrow(data) > 0) {
    sample_points <- data$points[!is.na(data$points)]
    
    if (length(sample_points) > 0) {
      normalized <- normalize_vector(sample_points)
      expect_equal(sum(normalized), 1.0)
    }
  }
})
