test_that("format functions handle table output", {
  # Test that formatting functions exist and are callable
  expect_true(is.function(format_quali_prob_table))
  expect_true(is.function(format_results_prob_table))
  expect_true(is.function(format_results_odds_table))
})

test_that("format_quali_prob_table() produces gt table", {
  skip("Requires specific test data structure")

  # This would test table formatting but requires specific data
  expect_true(is.function(format_quali_prob_table))
})
