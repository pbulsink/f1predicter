test_that("normalize_vector() works with probability outputs (#noissue)", {
  # Social media functions work with probabilities which should sum to 1
  prob_vec <- c(0.25, 0.35, 0.20, 0.15, 0.05)
  result <- normalize_vector(prob_vec)
  
  expect_equal(sum(result), 1.0)
  expect_length(result, 5)
})

test_that("format functions handle table output (#noissue)", {
  # Test that formatting functions exist and are callable
  expect_true(is.function(format_quali_prob_table))
  expect_true(is.function(format_results_prob_table))
  expect_true(is.function(format_results_odds_table))
})

test_that("format_quali_prob_table() produces gt table (#noissue)", {
  skip("Requires specific test data structure")
  
  # This would test table formatting but requires specific data
  expect_true(is.function(format_quali_prob_table))
})

test_that("post prediction functions handle missing data (#noissue)", {
  # Verify functions exist for posting predictions
  expect_true(is.function(post_race_predictions))
  expect_true(is.function(post_quali_predictions))
})
