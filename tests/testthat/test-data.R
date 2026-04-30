test_that("get_schedule() returns a tibble with schedule data and data is as expected", {
  result <- get_schedule()

  # Should be a tibble
  expect_s3_class(result, "tbl_df")

  # Should have rows
  expect_gt(nrow(result), 0)

  # Should have essential columns
  expected_cols <- c("season", "round", "circuit_id")
  for (col in expected_cols) {
    expect_true(
      col %in% names(result),
      paste("Column", col, "should exist in get_schedule()")
    )
  }

  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))

  for (season in unique(result$season)) {
    season_data <- result[result$season == season, ]
    rounds <- sort(season_data$round)

    # Rounds should be consecutive starting from 1
    expect_equal(rounds[1], 1)
  }
})

test_that("load_all_data() returns a list with expected components", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  if (!dir.exists(getOption('f1predicter.cache'))) {
    skip("No cached data")
  }
  result <- load_all_data()

  # Should be a list
  expect_type(result, "list")

  # Should have required components
  expected_components <- c(
    "results",
    "sprint_results",
    "laps",
    "pitstops",
    "sgrid",
    "rgrid",
    "qualis"
  )
  expect_true(all(expected_components %in% names(result)))

  # Main components should be tibbles or dataframes
  expect_true(is.data.frame(result$results))
  expect_true(is.data.frame(result$qualis))
})

test_that("load_rds_or_csv() warns and returns NULL for corrupt RDS files (#noissue)", {
  bad_rds <- withr::local_tempfile(fileext = ".rds")
  writeLines("not an rds file", bad_rds)

  expect_warning(
    result <- load_rds_or_csv(bad_rds),
    "Failed to read"
  )
  expect_null(result)
})

test_that("get_laps_or_null() cleans successful lap data responses (#noissue)", {
  local_mocked_bindings(
    load_session_laps = function(...) {
      tibble::tibble(
        is_personal_best = list(TRUE, FALSE),
        track_status = list(1, 2),
        air_temp = list(23, 24),
        humidity = list(40, 41),
        pressure = list(1008, 1009),
        rainfall = list(FALSE, TRUE),
        track_temp = list(31, 32),
        wind_direction = list(100, 101),
        wind_speed = list(2.5, 2.7),
        fresh_tyre = list(TRUE, FALSE),
        is_accurate = list(TRUE, TRUE)
      )
    },
    .package = "f1dataR"
  )

  laps <- get_laps_or_null(season = 2026, round = 1, session = "FP1")

  expect_s3_class(laps, "data.frame")
  expect_named(
    laps,
    c(
      "is_personal_best",
      "track_status",
      "air_temp",
      "humidity",
      "pressure",
      "rainfall",
      "track_temp",
      "wind_direction",
      "wind_speed",
      "fresh_tyre",
      "is_accurate",
      "deleted_reason"
    )
  )
  expect_type(laps$track_status, "double")
  expect_type(laps$deleted_reason, "character")
  expect_identical(laps$deleted_reason, c(NA_character_, NA_character_))
  expect_identical(laps$rainfall, c(FALSE, TRUE))
})

test_that("get_laps_or_null() returns NULL when session data cannot be loaded (#noissue)", {
  local_mocked_bindings(
    load_session_laps = function(...) {
      stop("session unavailable")
    },
    .package = "f1dataR"
  )

  expect_null(get_laps_or_null(season = 2026, round = 1, session = "FP2"))
})

test_that("get_grids() returns qualifying-only data when race results are unavailable (#noissue)", {
  local_mocked_bindings(
    load_results = function(...) NULL,
    load_quali = function(...) {
      tibble::tibble(position = 1:2, driver_id = c("driver_a", "driver_b"))
    },
    .package = "f1dataR"
  )

  grid <- get_grids(season = 2026, round = 1, session = "R")

  expect_s3_class(grid, "data.frame")
  expect_identical(grid$position, c(1L, 2L))
  expect_identical(grid$quali_results, c("driver_a", "driver_b"))
  expect_true(all(is.na(grid$start_grid)))
  expect_true(all(is.na(grid$race_results)))
})

test_that("get_weekend_data() reads cached weekend results without fetching new data (#noissue)", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(list(f1predicter.cache = cache_dir))
  season <- 2002
  round <- 1

  # Use a pre-2003 season so uncached grid/quali branches are skipped.
  cached_results <- tibble::tibble(
    driver_id = "driver_a",
    constructor_id = "team_a",
    position = 1,
    grid = 1,
    points = 25,
    season = season,
    round = round
  )
  saveRDS(
    cached_results,
    file.path(cache_dir, paste0(season, "_", round, "_results.rds"))
  )

  local_mocked_bindings(
    load_results = function(...) {
      stop("network fetch should not be used when cache is present")
    },
    .package = "f1dataR"
  )

  result <- get_weekend_data(season = season, round = round)

  expect_s3_class(result$results, "data.frame")
  expect_identical(result$results$driver_id, "driver_a")
  expect_null(result$rgrid)
  expect_null(result$sprint_results)
  expect_null(result$pitstops)
  expect_null(result$laps)
  expect_null(result$quali)
})
