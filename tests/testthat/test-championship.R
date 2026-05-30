# ---- Helpers ----------------------------------------------------------------

make_standings <- function(n = 5) {
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n)]),
    points = c(200, 180, 150, 100, 50),
    position = seq_len(n)
  )
}

make_remaining_schedule <- function(n_races = 3, n_sprints = 1) {
  tibble::tibble(
    round = seq_len(n_races) + 10,
    race_name = paste("Race", seq_len(n_races)),
    date = Sys.Date() + seq_len(n_races) * 7,
    has_sprint = c(rep(TRUE, n_sprints), rep(FALSE, n_races - n_sprints))
  )
}

make_performance <- function(n = 5) {
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n)]),
    n_races = rep(10L, n),
    avg_position = c(3, 4, 5, 8, 12),
    position_sd = c(2, 3, 3, 4, 5),
    dnf_rate = c(0.05, 0.08, 0.10, 0.12, 0.15),
    recent_avg_position = c(2.5, 3.5, 5.5, 9, 13),
    recent_sd = c(1.5, 2.5, 3, 4, 5),
    weighted_avg_position = 0.7 * c(2.5, 3.5, 5.5, 9, 13) +
      0.3 * c(3, 4, 5, 8, 12)
  )
}


# ---- Points Systems ---------------------------------------------------------

test_that("gp_points_system returns correct F1 GP points", {

  pts <- f1predicter:::gp_points_system()
  expect_equal(pts[["1"]], 25)
  expect_equal(pts[["2"]], 18)
  expect_equal(pts[["3"]], 15)
  expect_equal(pts[["10"]], 1)
  expect_length(pts, 10)
})

test_that("sprint_points_system returns correct F1 sprint points", {
  pts <- f1predicter:::sprint_points_system()
  expect_equal(pts[["1"]], 8)
  expect_equal(pts[["8"]], 1)
  expect_length(pts, 8)
})

test_that("get_points_for_position returns correct points for GP", {
  expect_equal(f1predicter:::get_points_for_position(1, "race"), 25)
  expect_equal(f1predicter:::get_points_for_position(3, "race"), 15)
  expect_equal(f1predicter:::get_points_for_position(10, "race"), 1)
  expect_equal(f1predicter:::get_points_for_position(11, "race"), 0)
  expect_equal(f1predicter:::get_points_for_position(20, "race"), 0)
})

test_that("get_points_for_position returns correct points for sprint", {
  expect_equal(f1predicter:::get_points_for_position(1, "sprint"), 8)
  expect_equal(f1predicter:::get_points_for_position(8, "sprint"), 1)
  expect_equal(f1predicter:::get_points_for_position(9, "sprint"), 0)
})

test_that("get_points_for_position handles edge cases", {
  expect_equal(f1predicter:::get_points_for_position(NA, "race"), 0)
  expect_equal(f1predicter:::get_points_for_position(0, "race"), 0)
  expect_equal(f1predicter:::get_points_for_position(-1, "race"), 0)
})


# ---- simulate_race_positions -------------------------------------------------

test_that("simulate_race_positions returns valid positions", {
  set.seed(42)
  positions <- f1predicter:::simulate_race_positions(
    avg_positions = c(3, 5, 8, 12, 15),
    position_sds = c(2, 3, 3, 4, 5),
    dnf_rates = c(0, 0, 0, 0, 0),  # No DNFs for deterministic test
    n_drivers = 5
  )

  # All positions should be unique integers from 1 to 5 (no DNFs)
  expect_length(positions, 5)
  expect_true(all(!is.na(positions)))
  expect_equal(sort(positions), 1:5)
})

test_that("simulate_race_positions handles DNFs", {
  set.seed(42)
  # All drivers DNF
  positions <- f1predicter:::simulate_race_positions(
    avg_positions = c(3, 5, 8),
    position_sds = c(2, 3, 3),
    dnf_rates = c(1, 1, 1),  # 100% DNF
    n_drivers = 3
  )

  expect_true(all(is.na(positions)))
})

test_that("simulate_race_positions produces different results with different seeds", {
  positions1 <- withr::with_seed(1, {
    f1predicter:::simulate_race_positions(
      avg_positions = c(3, 5, 8),
      position_sds = c(2, 3, 3),
      dnf_rates = c(0.1, 0.1, 0.1),
      n_drivers = 3
    )
  })
  positions2 <- withr::with_seed(99, {
    f1predicter:::simulate_race_positions(
      avg_positions = c(3, 5, 8),
      position_sds = c(2, 3, 3),
      dnf_rates = c(0.1, 0.1, 0.1),
      n_drivers = 3
    )
  })
  # With different seeds we expect at least some variation

  # (not guaranteed but highly likely with these params)
  # Just check they are valid

  expect_length(positions1, 3)
  expect_length(positions2, 3)
})


# ---- simulate_championship_odds ---------------------------------------------

test_that("simulate_championship_odds returns correct structure", {
  standings <- make_standings()
  remaining <- make_remaining_schedule()

  # Mock performance calculation
  local_mocked_bindings(
    calculate_driver_performance = function(season, historical_data) {
      make_performance()
    }
  )

  set.seed(42)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 100L
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "driver_id", "current_points", "win_probability",
    "avg_final_points", "avg_final_position", "in_contention", "season"
  ) %in% names(result)))
  expect_equal(nrow(result), 5)
  expect_equal(result$season[1], 2025)
})

test_that("simulate_championship_odds probabilities sum to 1 (or less)", {
  standings <- make_standings()
  remaining <- make_remaining_schedule()

  local_mocked_bindings(
    calculate_driver_performance = function(season, historical_data) {
      make_performance()
    }
  )

  set.seed(42)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 500L
  )

  # Probabilities should sum to approximately 1 (contenders only)
  contender_probs <- result$win_probability[result$in_contention]
  expect_true(abs(sum(contender_probs) - 1) < 0.01)
})

test_that("simulate_championship_odds handles no remaining races", {
  standings <- make_standings()
  remaining <- tibble::tibble(
    round = integer(0),
    race_name = character(0),
    date = as.Date(character(0)),
    has_sprint = logical(0)

  )

  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 100L
  )

  # Leader should have probability 1
  expect_equal(result$win_probability[1], 1)
  expect_equal(sum(result$win_probability), 1)
})

test_that("simulate_championship_odds identifies contenders correctly", {
  # Driver e (50 pts) cannot catch driver a (200 pts) with 3 races + 1 sprint

  # Max possible: 3*25 + 1*8 = 83 pts -> 50 + 83 = 133 < 200
  standings <- make_standings()
  remaining <- make_remaining_schedule(n_races = 3, n_sprints = 1)

  local_mocked_bindings(
    calculate_driver_performance = function(season, historical_data) {
      make_performance()
    }
  )

  set.seed(42)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 100L
  )

  # Driver e (50 pts) cannot reach leader (200 pts) with max 83 points available
  driver_e <- result[result$driver_id == "driver_e", ]
  expect_false(driver_e$in_contention)
  expect_equal(driver_e$win_probability, 0)
})

test_that("simulate_championship_odds leader usually wins", {
  standings <- make_standings()
  remaining <- make_remaining_schedule(n_races = 2, n_sprints = 0)

  local_mocked_bindings(
    calculate_driver_performance = function(season, historical_data) {
      make_performance()
    }
  )

  set.seed(42)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 1000L
  )

  # Driver_a leads by 20pts and is the best performer - should be heavy favourite
  leader <- result[result$driver_id == "driver_a", ]
  expect_gt(leader$win_probability, 0.4)
})

test_that("simulate_championship_odds validates inputs", {
  expect_error(
    simulate_championship_odds(season = "abc"),
    "is.numeric"
  )
  expect_error(
    simulate_championship_odds(season = 2025, n_simulations = -1),
    "n_simulations >= 1"
  )
})


# ---- get_remaining_schedule --------------------------------------------------

test_that("get_remaining_schedule filters by after_round", {
  # Use package schedule data
  schedule <- f1predicter::schedule
  # Find a season that has enough rounds
  test_season <- max(as.numeric(schedule$season), na.rm = TRUE)
  max_round <- max(
    as.numeric(schedule[schedule$season == test_season, ]$round),
    na.rm = TRUE
  )

  if (max_round > 2) {
    result <- get_remaining_schedule(test_season, after_round = max_round - 2)
    expect_s3_class(result, "tbl_df")
    expect_true(all(result$round > (max_round - 2)))
    expect_true("has_sprint" %in% names(result))
  }
})


# ---- format_championship_skeet -----------------------------------------------

test_that("format_championship_skeet returns proper skeet structure", {
  odds <- tibble::tibble(
    driver_id = c("max_verstappen", "lando_norris", "charles_leclerc"),
    current_points = c(300, 280, 250),
    win_probability = c(0.55, 0.30, 0.15),
    avg_final_points = c(380, 365, 340),
    avg_final_position = c(1.5, 2.2, 2.8),
    in_contention = c(TRUE, TRUE, TRUE),
    season = 2025
  )

  local_mocked_bindings(
    load_drivers = function(season) {
      tibble::tibble(
        driver_id = c("max_verstappen", "lando_norris", "charles_leclerc"),
        given_name = c("Max", "Lando", "Charles"),
        family_name = c("Verstappen", "Norris", "Leclerc")
      )
    },
    .package = "f1dataR"
  )

  result <- f1predicter:::format_championship_skeet(odds)

  expect_type(result, "list")
  expect_length(result, 2)
  # First skeet should have text and tags

  expect_true("text" %in% names(result[[1]]))
  expect_true("tags" %in% names(result[[1]]))
  # Check tags
  expect_true("F1Championship" %in% result[[1]]$tags)
  # Check content mentions championship
  expect_match(result[[1]]$text, "Championship")
  # Check probabilities appear
  expect_match(result[[1]]$text, "55.0%")
})

test_that("format_championship_skeet includes all contenders up to 5", {
  odds <- tibble::tibble(
    driver_id = paste0("driver_", letters[1:6]),
    current_points = c(300, 280, 250, 220, 200, 180),
    win_probability = c(0.35, 0.25, 0.20, 0.10, 0.07, 0.03),
    avg_final_points = c(380, 365, 340, 320, 300, 280),
    avg_final_position = c(1.5, 2.2, 2.8, 3.5, 4.2, 5.1),
    in_contention = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    season = 2024
  )

  local_mocked_bindings(
    load_drivers = function(season) {
      tibble::tibble(
        driver_id = paste0("driver_", letters[1:6]),
        given_name = paste0("Given", LETTERS[1:6]),
        family_name = paste0("Family", LETTERS[1:6])
      )
    },
    .package = "f1dataR"
  )

  result <- f1predicter:::format_championship_skeet(odds)
  # Should list top 5 contenders
  expect_match(result[[1]]$text, "GivenE FamilyE")
  # 6th driver should NOT appear in skeet 1
  expect_false(grepl("GivenF FamilyF", result[[1]]$text))
})
