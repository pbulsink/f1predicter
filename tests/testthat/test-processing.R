test_that("clean_data() returns a tibble with expected structure and valid data", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- clean_data()

  # Should be a tibble
  expect_s3_class(result, "tbl_df")

  # Should have rows
  expect_gt(nrow(result), 0)

  # Should have essential columns
  expected_cols <- c(
    "season",
    "round",
    "driver_id",
    "constructor_id",
    "grid",
    "position",
    "points",
    "finished"
  )
  for (col in expected_cols) {
    expect_true(
      col %in% names(result),
      paste("Column", col, "should exist in clean_data()")
    )
  }

  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))

  # Grid and position should be positive (or zero/NA)
  expect_true(all(result$grid >= 0 | is.na(result$grid)))
  expect_true(all(result$position >= 0 | is.na(result$position)))

  # Finished should be 0, 1, or NA
  valid_vals <- c(0, 1, NA)
  expect_true(all(result$finished %in% valid_vals | is.na(result$finished)))

  # Points should be non-negative
  expect_true(all(result$points >= 0 | is.na(result$points)))
})

test_that("get_season_data() errors on invalid season", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  # Should error on future season or invalid year
  expect_error(get_season_data(9999))
})

test_that("get_weekend_data() returns data for valid race", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- get_weekend_data(2023, 1)

  expect_type(result, "list")
  result <- result$results
  expect_s3_class(result, 'data.frame')
  expect_gt(nrow(result), 0)
  expect_true(all(result$round == 1))
})

test_that("get_weekend_data() errors on invalid round", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  expect_error(get_weekend_data(2023, 99))
})

# ---- get_processing_params() tests ----

test_that("get_processing_params() returns a named list with all expected keys", {
  params <- get_processing_params()

  expect_type(params, "list")

  expected_keys <- c(
    "grid",
    "position",
    "driver_finish_avg",
    "driver_failure_avg",
    "constructor_failure_avg",
    "grid_pos_corr_avg",
    "constructor_pit_duration_perc",
    "quali_avg_perc",
    "fastest_pit",
    "pos_change",
    "qgap",
    "constructor_pit_num_perc"
  )
  for (k in expected_keys) {
    expect_true(k %in% names(params), info = paste("key", k, "should exist"))
    expect_type(params[[k]], "double")
  }
})

test_that("get_processing_params() values are in plausible ranges", {
  params <- get_processing_params()

  expect_gt(params$grid, 0)
  expect_lt(params$grid, 25)
  expect_gt(params$position, 0)
  expect_lt(params$position, 25)
  expect_true(params$driver_finish_avg > 0 && params$driver_finish_avg < 1)
  expect_true(params$driver_failure_avg > 0 && params$driver_failure_avg < 1)
  expect_true(
    params$constructor_failure_avg > 0 && params$constructor_failure_avg < 1
  )
  expect_true(params$fastest_pit > 0)
})

test_that("clean_data() respects custom params", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  # Modify one parameter and verify it can be passed without error
  custom_params <- get_processing_params()
  custom_params$fastest_pit <- 3.0

  # Just check that it runs without error; we skip deep value verification
  # because that requires a full data reload which is expensive in CI.
  skip_if(
    !dir.exists(getOption(
      "f1predicter.cache",
      "~/Documents/f1predicter/cache"
    )),
    "Cache directory not available in this environment"
  )
  result <- clean_data(params = custom_params)
  expect_s3_class(result, "tbl_df")
})

# ---- cache_to_rds / load_rds_or_csv round-trip ----

test_that("cache_to_rds and load_rds_or_csv round-trip correctly", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  cache_to_rds(df, tmp)
  expect_true(file.exists(tmp))

  loaded <- load_rds_or_csv(tmp)
  expect_equal(loaded, df)
})

test_that("load_rds_or_csv falls back to CSV when RDS absent", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_rds <- tempfile(fileext = ".rds")
  on.exit({
    unlink(tmp_csv)
    unlink(tmp_rds)
  })

  df <- data.frame(a = 1:3, b = 4:6)
  utils::write.csv(df, tmp_csv, row.names = FALSE)

  loaded <- load_rds_or_csv(tmp_rds, tmp_csv)
  expect_equal(nrow(loaded), 3)
  expect_true("a" %in% names(loaded))
})

test_that("load_rds_or_csv returns NULL when neither file exists", {
  result <- load_rds_or_csv(
    tempfile(fileext = ".rds"),
    tempfile(fileext = ".csv")
  )
  expect_null(result)
})

# ---- migrate_cache_to_rds() ----

test_that("migrate_cache_to_rds() converts CSVs to RDS in a temp directory", {
  tmp_dir <- tempdir()

  # Write a small fake CSV file
  df <- data.frame(driver_id = "hamilton", points = 25L)
  csv_path <- file.path(tmp_dir, "2022_season_results.csv")
  rds_path <- file.path(tmp_dir, "2022_season_results.rds")
  on.exit({
    unlink(csv_path)
    unlink(rds_path)
  })
  utils::write.csv(df, csv_path, row.names = FALSE)

  withr::local_options(f1predicter.cache = tmp_dir)
  written <- migrate_cache_to_rds(cache = tmp_dir, years = 2022)

  expect_true(file.exists(rds_path))
  expect_true(rds_path %in% written)

  loaded <- readRDS(rds_path)
  expect_equal(nrow(loaded), 1)
})

test_that("migrate_cache_to_rds() skips files already migrated", {
  tmp_dir <- tempdir()

  df <- data.frame(driver_id = "leclerc", points = 18L)
  csv_path <- file.path(tmp_dir, "2023_season_results.csv")
  rds_path <- file.path(tmp_dir, "2023_season_results.rds")
  on.exit({
    unlink(csv_path)
    unlink(rds_path)
  })
  utils::write.csv(df, csv_path, row.names = FALSE)
  saveRDS(df, rds_path) # already exists

  withr::local_options(f1predicter.cache = tmp_dir)
  written <- migrate_cache_to_rds(cache = tmp_dir, years = 2023)

  # Should not re-write anything
  expect_length(written, 0)
})
