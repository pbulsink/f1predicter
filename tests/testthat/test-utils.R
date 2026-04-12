test_that("nroot() calculates roots correctly (#noissue)", {
  # Square root of 4 should be 2
  expect_equal(nroot(4, 2), 2)
  # Cube root of 8 should be 2
  expect_equal(nroot(8, 3), 2)
  # Fourth root of 16 should be 2
  expect_equal(nroot(16, 4), 2)
})

test_that("cubert() calculates cube roots correctly (#noissue)", {
  expect_equal(cubert(8), 2)
  expect_equal(cubert(27), 3)
  # Verify it handles vectorization
  result <- cubert(c(8, 27, 64))
  expect_equal(result, c(2, 3, 4))
})

test_that("expand_val() pads vectors correctly (#noissue)", {
  # Short vector should be padded
  result <- expand_val(c(1, 2, 3), 5, val = 0)
  expect_equal(result, c(1, 2, 3, 0, 0))
  
  # Exact length vector should remain unchanged
  result <- expand_val(c(1, 2, 3), 3, val = 0)
  expect_equal(result, c(1, 2, 3))
  
  # Long vector should be truncated
  result <- expand_val(c(1, 2, 3, 4, 5), 3, val = 0)
  expect_equal(result, c(1, 2, 3))
  
  # NA values should be used as padding
  result <- expand_val(c(1, 2), 4, val = NA)
  expect_equal(result, c(1, 2, NA, NA))
})

test_that("cumwmean() calculates cumulative weighted mean correctly (#noissue)", {
  x <- c(1, 2, 3)
  result <- cumwmean(x, na.val = 0)
  
  # Result should be a numeric vector
  expect_type(result, "double")
  # Should have same length as input
  expect_equal(length(result), 3)
  # Values should be increasing (due to cumulative nature)
  expect_true(all(diff(result) >= 0))
})

test_that("cumwmean() handles NA values (#noissue)", {
  x <- c(1, NA, 3)
  # With na.val = 0, NAs are replaced before calculation
  result <- cumwmean(x, na.val = 0)
  expect_equal(length(result), 3)
  expect_true(!is.na(result[2]))
})

test_that("ensure_tidy() cleans column names (#noissue)", {
  data <- data.frame(
    "Column One" = 1:3,
    "Column.Two" = 4:6,
    check.names = FALSE
  )
  result <- ensure_tidy(data)
  
  # Column names should be cleaned
  expect_true("column_one" %in% tolower(names(result)))
  expect_true("column_two" %in% tolower(names(result)))
})

test_that("ensure_tidy() renames 'race' to 'round' (#noissue)", {
  data <- data.frame(race = 1:3, value = 4:6)
  result <- ensure_tidy(data)
  
  expect_true("round" %in% names(result))
  expect_false("race" %in% names(result))
})

test_that("ensure_tidy() returns NULL if input is NULL (#noissue)", {
  result <- ensure_tidy(NULL)
  expect_null(result)
})

test_that("ensure_tidy() returns NA if input is all NA (#noissue)", {
  data <- data.frame(x = NA, y = NA)
  result <- ensure_tidy(data)
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$x)))
})

test_that("normalize_vector() sums to 1 (#noissue)", {
  x <- c(1, 2, 3, 4)
  result <- normalize_vector(x)
  
  expect_length(result, 4)
  expect_equal(sum(result), 1)
})

test_that("normalize_vector() handles NA values (#noissue)", {
  x <- c(1, 2, NA, 4)
  result <- normalize_vector(x)
  
  expect_length(result, 4)
  expect_equal(sum(result), 1)
  # NA in input should remain NA in output
  expect_true(is.na(result[3]))
})

test_that("normalize_vector() returns uniform distribution for zero sum (#noissue)", {
  x <- c(0, 0, 0)
  result <- normalize_vector(x)
  
  # Should be 1/3 for each element
  expect_equal(result, c(1/3, 1/3, 1/3))
})

test_that("normalize_vector() errors on non-numeric input (#noissue)", {
  expect_error(normalize_vector(c("a", "b", "c")))
})

test_that("normalize_vector() handles empty vector (#noissue)", {
  result <- normalize_vector(numeric(0))
  expect_length(result, 0)
})

test_that("wmean() returns weighted mean (#noissue)", {
  x <- c(1, 2, 3)
  result <- wmean(x, ln = 3, val = 0)
  
  # Result should be numeric
  expect_type(result, "double")
  # Result should be between min and max of x
  expect_true(result >= 1 && result <= 3)
})

test_that("wmean() pads vector with specified value (#noissue)", {
  x <- c(1, 2)
  result_with_zero <- wmean(x, ln = 4, val = 0)
  result_with_one <- wmean(x, ln = 4, val = 1)
  
  # Different padding values should produce different results
  expect_false(isTRUE(all.equal(result_with_zero, result_with_one)))
})

test_that("lagged_cumwmean_expanded() lags correctly (#noissue)", {
  x <- c(1, 2, 3)
  result <- lagged_cumwmean_expanded(x, ln = 3, val = 0)
  
  expect_length(result, 3)
  # First value should be the lag value (val = 0)
  expect_equal(result[1], 0)
  # Remaining values should come from cumulative mean
  expect_type(result, "double")
})

test_that("s_lagged_cumwmean_expanded() returns last value (#noissue)", {
  x <- c(1, 2, 3)
  result <- s_lagged_cumwmean_expanded(x, ln = 3, val = 0)
  
  # Result should be a single numeric value
  expect_type(result, "double")
  expect_length(result, 1)
})
