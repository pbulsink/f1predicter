# Read width and height from a PNG file's IHDR chunk.
# PNG layout: 8-byte signature + 4-byte chunk length + 4-byte "IHDR" type +
# 4-byte width + 4-byte height (all big-endian integers).
png_dims <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", n = 16) # skip: 8-byte signature + 4-byte length + 4-byte "IHDR"
  width <- readBin(con, "integer", n = 1, size = 4, endian = "big")
  height <- readBin(con, "integer", n = 1, size = 4, endian = "big")
  list(width = width, height = height)
}

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

test_that("save_gt_as_png_ragg() creates PNG file with auto-detected size", {
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

test_that("save_gt_as_png_ragg() auto-detected size is smaller than a fixed large size", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:5, y = 6:10)
  gt_table <- gt::gt(test_df)

  temp_file_auto <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_fixed <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_file_auto)
  save_gt_as_png_ragg(gt_table, temp_file_fixed, width = 1400, height = 800)

  expect_true(file.exists(temp_file_auto))
  expect_true(file.exists(temp_file_fixed))
  expect_gt(file.size(temp_file_auto), 0)
  # Auto-detected dimensions should be strictly less than the fixed 1400x800 canvas
  dims_auto <- png_dims(temp_file_auto)
  dims_fixed <- png_dims(temp_file_fixed)
  expect_lt(dims_auto$width, dims_fixed$width)
  expect_lt(dims_auto$height, dims_fixed$height)
})

test_that("save_gt_as_png_ragg() uses explicit width and height when provided", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:5, y = 6:10)
  gt_table <- gt::gt(test_df)

  temp_file_small <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_large <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_file_small, width = 400, height = 300)
  save_gt_as_png_ragg(gt_table, temp_file_large, width = 1200, height = 900)

  expect_true(file.exists(temp_file_small))
  expect_true(file.exists(temp_file_large))
  expect_gt(file.size(temp_file_small), 0)
  expect_gt(file.size(temp_file_large), 0)
  # Verify the PNG files were created with the requested pixel dimensions
  dims_small <- png_dims(temp_file_small)
  dims_large <- png_dims(temp_file_large)
  expect_equal(dims_small$width, 400L)
  expect_equal(dims_small$height, 300L)
  expect_equal(dims_large$width, 1200L)
  expect_equal(dims_large$height, 900L)
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

test_that("save_gt_as_png_ragg() padding parameter controls extra space", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(Team = c("A", "B"), Points = c(10, 8))
  gt_table <- gt::gt(test_df)

  temp_no_pad <- withr::local_file(tempfile(fileext = ".png"))
  temp_with_pad <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_no_pad, padding = 0)
  save_gt_as_png_ragg(gt_table, temp_with_pad, padding = 100)

  expect_true(file.exists(temp_no_pad))
  expect_true(file.exists(temp_with_pad))
  # More padding means a larger canvas — verify via actual PNG dimensions
  dims_no_pad <- png_dims(temp_no_pad)
  dims_with_pad <- png_dims(temp_with_pad)
  expect_gt(dims_with_pad$width, dims_no_pad$width)
  expect_gt(dims_with_pad$height, dims_no_pad$height)
})
