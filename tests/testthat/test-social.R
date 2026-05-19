test_that("format functions handle table output", {
  # Test that formatting functions exist and are callable
  expect_true(is.function(format_quali_prob_table))
  expect_true(is.function(format_results_prob_table))
  expect_true(is.function(format_results_odds_table))
})

test_that("is_sprint_weekend() detects sprint rounds from schedule (#noissue)", {
  sched <- tibble::tibble(
    season = c(2024L, 2024L),
    round = c(5L, 6L),
    sprint_date = as.Date(c("2024-05-03", NA))
  )

  expect_true(is_sprint_weekend(2024L, 5L, schedule = sched))
  expect_false(is_sprint_weekend(2024L, 6L, schedule = sched))
  expect_false(is_sprint_weekend(2024L, 7L, schedule = sched))
})

test_that("format_quali_skeet_predictions() uses sprint copy on sprint weekends (#noissue)", {
  predictions <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = 5L,
    season = 2024L,
    pole_odd = c(0.6, 0.4),
    likely_quali_position = c(1, 2)
  )

  local_mocked_bindings(
    get_driver_name = function(season, driver_ids) {
      c("Driver A", "Driver B")
    },
    get_race_name = function(season, round) {
      "Miami Grand Prix"
    },
    format_quali_prob_table = function(predictions, save_image = FALSE) {
      list(filename = "/tmp/quali.png")
    },
    is_sprint_weekend = function(
      season,
      round,
      schedule = f1predicter::schedule
    ) {
      TRUE
    }
  )

  result <- format_quali_skeet_predictions(predictions)
  expect_match(result[[1]]$text, "Sprint")
  expect_true("F1Sprint" %in% result[[1]]$tags)
})

test_that("post_sprint_predictions() formats then posts sprint skeets (#noissue)", {
  predictions <- tibble::tibble(
    driver_id = "driver_a",
    round = 1L,
    season = 2026L,
    sprint_win_odd = 0.5
  )
  captured <- NULL

  local_mocked_bindings(
    format_sprint_skeet_predictions = function(predictions) {
      list(list(text = "Sprint post"))
    },
    post_skeet_predictions = function(skeets) {
      captured <<- skeets
      invisible(list(uri = "at://example"))
    }
  )

  post_sprint_predictions(predictions)
  expect_identical(captured, list(list(text = "Sprint post")))
})

test_that("format_sprint_skeet_predictions() formats top sprint win odds (#noissue)", {
  predictions <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = 3L,
    season = 2026L,
    sprint_win_odd = c(0.7, 0.3)
  )

  local_mocked_bindings(
    get_driver_name = function(season, driver_ids) c("Driver A", "Driver B"),
    get_race_name = function(season, round) "Australian Grand Prix"
  )

  result <- format_sprint_skeet_predictions(predictions)

  expect_match(result[[1]]$text, "Sprint Win Predictions")
  expect_match(result[[1]]$text, "Driver A")
  expect_true("F1Sprint" %in% result[[1]]$tags)
})

test_that("format_quali_prob_table() produces gt table", {
  skip("Requires specific test data structure")

  # This would test table formatting but requires specific data
  expect_true(is.function(format_quali_prob_table))
})
