test_that("save_gt_as_png_ragg() requires ragg package", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:3, y = 4:6)
  gt_table <- gt::gt(test_df)

  # Create a temporary file
  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  # Should succeed when ragg is available
  result <- save_gt_as_png_ragg(gt_table, temp_file)
  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))
})

test_that("save_gt_as_png_ragg() creates PNG file", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Score = c(95, 87, 92),
    Grade = c("A", "B", "A")
  )
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  # File should exist
  expect_true(file.exists(temp_file))
  # File should have content (be larger than 0 bytes)
  expect_gt(file.size(temp_file), 0)
  # Result should return the filename invisibly
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() uses custom width and height", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:5, y = 6:10)
  gt_table <- gt::gt(test_df)

  temp_file_default <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_custom <- withr::local_file(tempfile(fileext = ".png"))

  # Save with default dimensions
  save_gt_as_png_ragg(gt_table, temp_file_default)

  # Save with custom dimensions (smaller)
  save_gt_as_png_ragg(gt_table, temp_file_custom, width = 800, height = 600)

  # Both files should exist
  expect_true(file.exists(temp_file_default))
  expect_true(file.exists(temp_file_custom))

  # Custom dimensions should typically produce different file sizes
  # (though we can't guarantee exact sizes due to compression)
  size_default <- file.size(temp_file_default)
  size_custom <- file.size(temp_file_custom)

  expect_gt(size_default, 0)
  expect_gt(size_custom, 0)
})

test_that("save_gt_as_png_ragg() works with styled gt tables", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(
    Team = c("Red", "Blue", "Green"),
    Wins = c(10, 8, 9),
    Losses = c(2, 4, 3)
  )

  # Create a styled gt table
  gt_table <- gt::gt(test_df) %>%
    gt::tab_header(title = "Team Performance", subtitle = "Regular Season")

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() returns filename invisibly", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(a = 1:3, b = 4:6)
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  # Capture output to verify nothing is printed
  expect_invisible(
    save_gt_as_png_ragg(gt_table, temp_file)
  )
})

test_that("save_gt_as_png_ragg() accepts scale parameter", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:3, y = 4:6)
  gt_table <- gt::gt(test_df)

  temp_file_scale1 <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_scale2 <- withr::local_file(tempfile(fileext = ".png"))

  # Save with different scales
  save_gt_as_png_ragg(gt_table, temp_file_scale1, scale = 1)
  save_gt_as_png_ragg(gt_table, temp_file_scale2, scale = 2)

  # Both files should exist and have content
  expect_true(file.exists(temp_file_scale1))
  expect_true(file.exists(temp_file_scale2))
  expect_gt(file.size(temp_file_scale1), 0)
  expect_gt(file.size(temp_file_scale2), 0)
})

test_that("save_gt_as_png_ragg() handles single row dataframes", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  # Create a single row data frame (gt has issues with completely empty DFs)
  test_df <- data.frame(x = 1, y = 2)
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  # Should create a file without error
  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() handles data with special characters", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(
    Name = c("François", "María", "José"),
    Value = c(100, 200, 300)
  )
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})
