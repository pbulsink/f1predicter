test_that("generate_new_data() returns a tibble", {
  skip_if_not_installed("f1dataR")
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  # Use clean_data for mock historical data
  historical_data <- cleaned_data

  result <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = historical_data,
    use_live_data = FALSE
  )

  # Should return a tibble
  expect_s3_class(result, "tbl_df")

  # Should have at least one row
  expect_gte(nrow(result), 1)

  # Should have essential columns
  expect_true("driver_id" %in% names(result))
  expect_true("constructor_id" %in% names(result))
  expect_true("season" %in% names(result))
  expect_true("round" %in% names(result))

  # Use invalid season/round
  expect_error(
    generate_new_data(season = 9999, round = 1)
  )

  penalties <- c("max_verstappen" = 5)
  result_penalty <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = historical_data,
    penalties = penalties,
    use_live_data = FALSE
  )

  # Verify result was generated with penalty to Max Verstappen
  # Also promoted Russell to 3rd
  expect_equal(
    as.integer(result[result$driver_id == "max_verstappen", "grid"]),
    as.integer(result_penalty[
      result_penalty$driver_id == "max_verstappen",
      "grid"
    ]) -
      5
  )

  expect_equal(
    as.integer(result[result$driver_id == "hamilton", "grid"]),
    as.integer(result_penalty[result_penalty$driver_id == "hamilton", "grid"]) +
      1
  )
})

test_that("generate_new_data() fills fallback defaults from thinned historical data (#noissue)", {
  historical_data <- cleaned_data |>
    dplyr::filter(.data$season == 2025, .data$round %in% c(21, 22)) |>
    dplyr::filter(.data$circuit_id != "albert_park") |>
    dplyr::slice_head(n = 40) |>
    dplyr::mutate(
      grid = as.numeric(.data$grid),
      position = as.numeric(.data$position)
    )

  target_driver <- as.character(historical_data$driver_id[[1]])
  target_constructor <- as.character(historical_data$constructor_id[[1]])
  historical_data <- historical_data |>
    dplyr::mutate(
      driver_failure_avg = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_failure_avg
      ),
      driver_failure = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_failure
      ),
      driver_finish_avg = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_finish_avg
      ),
      finished = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$finished
      ),
      driver_grid_avg = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_grid_avg
      ),
      grid = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$grid
      ),
      position = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$position
      ),
      driver_position_avg = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_position_avg
      ),
      driver_avg_qgap = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_avg_qgap
      ),
      driver_practice_optimal_rank_avg = dplyr::if_else(
        .data$driver_id == target_driver,
        NA_real_,
        .data$driver_practice_optimal_rank_avg
      )
    )

  drivers <- tibble::tibble(
    driver_id = target_driver,
    constructor_id = target_constructor
  )

  params <- get_processing_params()
  expected_default_qgap <- 1
  expected_default_practice_avg_gap <- 1.5
  expected_default_practice_best_gap <- 1
  result <- generate_new_data(
    season = 2026,
    round = 1,
    drivers = drivers,
    historical_data = historical_data,
    use_live_data = FALSE
  )

  target_row <- result[result$driver_id == target_driver, ]

  expect_s3_class(result, "tbl_df")
  expect_identical(as.character(result$driver_id), drivers$driver_id)
  expect_true(target_row$driver_experience > 0)
  expect_equal(target_row$driver_failure_avg, params$driver_failure_avg)
  expect_equal(target_row$grid_pos_corr_avg, params$grid_pos_corr_avg)
  expect_equal(target_row$driver_failure_circuit_avg, params$driver_failure_avg)
  expect_equal(
    target_row$constructor_failure_circuit_avg,
    params$constructor_failure_avg
  )
  expect_equal(target_row$driver_avg_qgap, expected_default_qgap)
  expect_equal(
    target_row$practice_avg_gap,
    expected_default_practice_avg_gap
  )
  expect_equal(
    target_row$practice_best_gap,
    expected_default_practice_best_gap
  )
  expect_equal(target_row$quali_position, target_row$grid)
  expect_s3_class(result$driver_id, "factor")
  expect_s3_class(result$constructor_id, "factor")
  expect_s3_class(result$round_id, "factor")
})

test_that("generate_next_race_data() forwards the next scheduled race (#noissue)", {
  next_race <- schedule |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::filter(.data$date >= Sys.Date()) |>
    dplyr::arrange(.data$date) |>
    dplyr::slice(1)

  local_mocked_bindings(
    generate_new_data = function(season, round, marker = NULL, ...) {
      list(season = season, round = round, marker = marker)
    },
    .package = "f1predicter"
  )

  result <- generate_next_race_data(marker = "forwarded")

  expect_identical(result$season, as.numeric(next_race$season))
  expect_identical(result$round, as.numeric(next_race$round))
  expect_identical(result$marker, "forwarded")
})

test_that("apply_grid_penalty() caps penalties at the back of the grid (#noissue)", {
  race_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b", "driver_c"),
    quali_position = 1:3,
    grid = 1:3
  )

  penalized <- apply_grid_penalty(
    race_data = race_data,
    driver_id = "driver_b",
    penalty = 5
  )

  expect_equal(penalized$grid[penalized$driver_id == "driver_a"], 1)
  expect_equal(penalized$grid[penalized$driver_id == "driver_c"], 2)
  expect_equal(penalized$grid[penalized$driver_id == "driver_b"], 3)
})

test_that("apply_grid_penalty() validates driver IDs and penalty values (#noissue)", {
  race_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    quali_position = 1:2,
    grid = 1:2
  )

  expect_error(
    apply_grid_penalty(
      race_data = race_data,
      driver_id = "missing",
      penalty = 1
    ),
    "not found"
  )
  expect_error(
    apply_grid_penalty(
      race_data = race_data,
      driver_id = "driver_a",
      penalty = 0
    ),
    "positive number"
  )
})
