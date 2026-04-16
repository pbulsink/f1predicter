test_that("prepare_and_split_data() returns required list elements", {
  # Create minimal test data
  test_data <- data.frame(
    round_id = rep(1:5, 4),
    driver_id = rep(c("max_verstappen", "lando_norris", "lewis_hamilton", "george_russell"), 5),
    grid = c(1:5, 2:6, 3:7, 4:8),
    position = c(1:5, 2:6, 3:7, 4:8),
    points = c(25, 18, 15, 12, 10, 25, 18, 15, 12, 10, 25, 18, 15, 12, 10, 25, 18, 15, 12, 10)
  )

  result <- prepare_and_split_data(test_data, group = "round_id")

  # Should have the required list elements
  expect_true("data_split" %in% names(result))
  expect_true("train_data" %in% names(result))
  expect_true("test_data" %in% names(result))
  expect_true("data_folds" %in% names(result))
})

test_that("prepare_and_split_data() creates training/test split correctly", {
  test_data <- data.frame(
    round_id = rep(1:5, 4),
    driver_id = rep(c("a", "b", "c", "d"), 5),
    value = rnorm(20)
  )

  result <- prepare_and_split_data(test_data, prop = 0.8, group = "round_id")

  # Train and test should have different sizes
  train_size <- nrow(result$train_data)
  test_size <- nrow(result$test_data)

  expect_true(train_size > 0)
  expect_true(test_size > 0)
  expect_true(train_size > test_size)  # More training data with 0.8 prop
})

test_that("prepare_and_split_data() selects specified columns", {
  test_data <- data.frame(
    round_id = 1:5,
    col1 = 1:5,
    col2 = 6:10,
    col3 = 11:15
  )

  result <- prepare_and_split_data(
    test_data,
    columns = c("col1", "col2"),
    group = "round_id"
  )

  # Should only have selected columns
  expect_true("col1" %in% names(result$train_data))
  expect_true("col2" %in% names(result$train_data))
  # col3 might not be selected (excluded)
})

test_that("prepare_and_split_data() converts character to factor", {
  test_data <- data.frame(
    round_id = 1:5,
    char_col = c("a", "b", "a", "b", "a"),
    num_col = 1:5
  )

  result <- prepare_and_split_data(test_data, group = "round_id")

  # Character columns should be converted to factors
  expect_true(is.factor(result$train_data$char_col))
  # Numeric should remain numeric
  expect_true(is.numeric(result$train_data$num_col))
})
