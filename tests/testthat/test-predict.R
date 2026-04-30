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
    dplyr::slice_head(n = 40)

  drivers <- tibble::tibble(
    driver_id = c("new_driver", as.character(historical_data$driver_id[[1]])),
    constructor_id = c(
      as.character(historical_data$constructor_id[[1]]),
      as.character(historical_data$constructor_id[[1]])
    )
  )

  params <- get_processing_params()
  result <- generate_new_data(
    season = 2026,
    round = 1,
    drivers = drivers,
    historical_data = historical_data,
    use_live_data = FALSE
  )

  new_driver <- result[result$driver_id == "new_driver", ]

  expect_s3_class(result, "tbl_df")
  expect_identical(sort(as.character(result$driver_id)), sort(drivers$driver_id))
  expect_equal(new_driver$driver_experience, 0)
  expect_equal(new_driver$driver_failure_avg, params$driver_failure_avg)
  expect_equal(new_driver$grid_pos_corr_avg, params$grid_pos_corr_avg)
  expect_equal(new_driver$driver_failure_circuit_avg, params$driver_failure_avg)
  expect_equal(new_driver$constructor_failure_circuit_avg, params$constructor_failure_avg)
  expect_equal(new_driver$practice_avg_gap, 1.5)
  expect_equal(new_driver$practice_best_gap, 1)
  expect_equal(new_driver$quali_position, new_driver$grid)
  expect_s3_class(result$driver_id, "factor")
  expect_s3_class(result$constructor_id, "factor")
  expect_s3_class(result$round_id, "factor")
})

test_that("generate_next_race_data() forwards the next scheduled race (#noissue)", {
  local_mocked_bindings(
    schedule = tibble::tibble(
      season = c("2030", "2030"),
      round = c("5", "6"),
      date = as.character(Sys.Date() + c(1, 10)),
      race_name = c("Test Grand Prix", "Later Grand Prix")
    ),
    generate_new_data = function(season, round, marker = NULL, ...) {
      list(season = season, round = round, marker = marker)
    }
  )

  result <- generate_next_race_data(marker = "forwarded")

  expect_identical(result$season, 2030)
  expect_identical(result$round, 5)
  expect_identical(result$marker, "forwarded")
})

test_that("generate_next_race_data() errors when no future races exist (#noissue)", {
  local_mocked_bindings(
    schedule = tibble::tibble(
      season = "2030",
      round = "1",
      date = as.character(Sys.Date() - 1),
      race_name = "Past Grand Prix"
    )
  )

  expect_error(
    generate_next_race_data(),
    "Could not find an upcoming race"
  )
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
    apply_grid_penalty(race_data = race_data, driver_id = "missing", penalty = 1),
    "not found"
  )
  expect_error(
    apply_grid_penalty(race_data = race_data, driver_id = "driver_a", penalty = 0),
    "positive number"
  )
})
