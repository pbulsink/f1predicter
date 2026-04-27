# Extracted from test-gt_image_export.R:62

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "f1predicter", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
png_dims <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", n = 16) # skip: 8-byte signature + 4-byte length + 4-byte "IHDR"
  width <- readBin(con, "integer", n = 1, size = 4, endian = "big")
  height <- readBin(con, "integer", n = 1, size = 4, endian = "big")
  list(width = width, height = height)
}

# test -------------------------------------------------------------------------
skip_if_not_installed("ragg")
skip_if_not_installed("gt")
test_df <- data.frame(x = 1:5, y = 6:10)
gt_table <- gt::gt(test_df)
temp_file_auto <- withr::local_file(tempfile(fileext = ".png"))
temp_file_fixed <- withr::local_file(tempfile(fileext = ".png"))
save_gt_as_png_ragg(gt_table, temp_file_auto)
