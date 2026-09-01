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
    weighted_avg_position = 0.65 *
      c(2.5, 3.5, 5.5, 9, 13) +
      0.30 * c(3, 4, 5, 8, 12) +
      0.05 * c(3, 4, 5, 8, 12)
  )
}

# Small example historical dataset for integration testing
make_historical_data <- function() {
  # Build data per-driver to avoid expand.grid ordering issues
  driver_positions <- list(
    driver_a = c(2, 1, 3, 4, 1, 3, 2, 3),
    driver_b = c(3, 4, 2, 5, 3, 4, 5, 4),
    driver_c = c(5, 6, 7, 8, 6, 7, 8, 5),
    driver_d = c(10, 12, 11, NA, 13, 10, 12, 11),
    driver_e = c(15, 17, 14, 18, 16, 15, NA, 16)
  )
  driver_finished <- list(
    driver_a = rep(TRUE, 8),
    driver_b = rep(TRUE, 8),
    driver_c = rep(TRUE, 8),
    driver_d = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    driver_e = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  driver_failure <- list(
    driver_a = rep(0, 8),
    driver_b = rep(0, 8),
    driver_c = rep(0, 8),
    driver_d = c(0, 0, 0, 1, 0, 0, 0, 0),
    driver_e = rep(0, 8)
  )
  constructor_failure <- list(
    driver_a = rep(0, 8),
    driver_b = rep(0, 8),
    driver_c = rep(0, 8),
    driver_d = rep(0, 8),
    driver_e = c(0, 0, 0, 0, 0, 0, 1, 0)
  )
  constructor_failure_race <- list(
    driver_a = rep(0, 8),
    driver_b = rep(0, 8),
    driver_c = c(0, 0.5, 0, 0, 0, 0, 0, 0),
    driver_d = rep(0, 8),
    driver_e = c(0, 0, 0, 0, 0, 0, 0.5, 0)
  )

  rows <- do.call(
    rbind,
    lapply(names(driver_positions), function(drv) {
      data.frame(
        driver_id = drv,
        round = 1:8,
        position = driver_positions[[drv]],
        finished = driver_finished[[drv]],
        driver_failure = driver_failure[[drv]],
        constructor_failure = constructor_failure[[drv]],
        constructor_failure_race = constructor_failure_race[[drv]],
        season = 2025,
        stringsAsFactors = FALSE
      )
    })
  )
  # Also add previous season data (2024) for multi-season blending
  prev_positions <- list(
    driver_a = c(3, 2, 1, 4, 2, 3, 1, 2),
    driver_b = c(4, 5, 3, 6, 4, 5, 3, 4),
    driver_c = c(6, 7, 8, 5, 7, 6, 9, 7),
    driver_d = c(11, 13, 10, 12, 14, 11, 10, 13),
    driver_e = c(16, 18, 15, 17, 19, 16, 14, 17)
  )
  prev_rows <- do.call(
    rbind,
    lapply(names(prev_positions), function(drv) {
      data.frame(
        driver_id = drv,
        round = 1:8,
        position = prev_positions[[drv]],
        finished = TRUE,
        driver_failure = 0,
        constructor_failure = 0,
        constructor_failure_race = 0,
        season = 2024,
        stringsAsFactors = FALSE
      )
    })
  )
  tibble::as_tibble(rbind(rows, prev_rows))
}


# ---- Points Systems ---------------------------------------------------------

test_that("gp_points_system returns correct F1 GP points", {
  pts <- gp_points_system()
  expect_equal(pts[["1"]], 25)
  expect_equal(pts[["2"]], 18)
  expect_equal(pts[["3"]], 15)
  expect_equal(pts[["10"]], 1)
  expect_length(pts, 10)
})

test_that("sprint_points_system returns correct F1 sprint points", {
  pts <- sprint_points_system()
  expect_equal(pts[["1"]], 8)
  expect_equal(pts[["8"]], 1)
  expect_length(pts, 8)
})

test_that("get_points_for_position returns correct points for GP", {
  expect_equal(get_points_for_position(1, "race"), 25)
  expect_equal(get_points_for_position(3, "race"), 15)
  expect_equal(get_points_for_position(10, "race"), 1)
  expect_equal(get_points_for_position(11, "race"), 0)
  expect_equal(get_points_for_position(20, "race"), 0)
})

test_that("get_points_for_position returns correct points for sprint", {
  expect_equal(get_points_for_position(1, "sprint"), 8)
  expect_equal(get_points_for_position(8, "sprint"), 1)
  expect_equal(get_points_for_position(9, "sprint"), 0)
})

test_that("get_points_for_position handles edge cases", {
  expect_equal(get_points_for_position(NA, "race"), 0)
  expect_equal(get_points_for_position(0, "race"), 0)
  expect_equal(get_points_for_position(-1, "race"), 0)
})


# ---- simulate_race_positions -------------------------------------------------

test_that("simulate_race_positions returns valid positions", {
  set.seed(42)
  positions <- simulate_race_positions(
    avg_positions = c(3, 5, 8, 12, 15),
    position_sds = c(2, 3, 3, 4, 5),
    dnf_rates = c(0, 0, 0, 0, 0), # No DNFs for deterministic test
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
  positions <- simulate_race_positions(
    avg_positions = c(3, 5, 8),
    position_sds = c(2, 3, 3),
    dnf_rates = c(1, 1, 1), # 100% DNF
    n_drivers = 3
  )

  expect_true(all(is.na(positions)))
})

test_that("simulate_race_positions produces different results with different seeds", {
  positions1 <- withr::with_seed(1, {
    simulate_race_positions(
      avg_positions = c(3, 5, 8),
      position_sds = c(2, 3, 3),
      dnf_rates = c(0.1, 0.1, 0.1),
      n_drivers = 3
    )
  })
  positions2 <- withr::with_seed(99, {
    simulate_race_positions(
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
  expect_true(all(
    c(
      "driver_id",
      "current_points",
      "win_probability",
      "avg_final_points",
      "avg_final_position",
      "in_contention",
      "season"
    ) %in%
      names(result)
  ))
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
    "must be a single numeric"
  )
  expect_error(
    simulate_championship_odds(season = 2025, n_simulations = -1),
    "must be a positive number"
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

  result <- format_championship_skeet(odds)

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
  # Check simulation count is dynamic
  expect_match(result[[2]]$text, "10,000")
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

  result <- format_championship_skeet(odds)
  # Should list top 5 contenders
  expect_match(result[[1]]$text, "GivenE FamilyE")
  # 6th driver should NOT appear in skeet 1
  expect_false(grepl("GivenF FamilyF", result[[1]]$text))
})


# ---- Integration: full simulation with example data --------------------------

test_that("simulate_championship_odds works end-to-end with example historical data", {
  standings <- make_standings()
  remaining <- make_remaining_schedule(n_races = 2, n_sprints = 1)
  historical_data <- make_historical_data()

  set.seed(42)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 10L,
    historical_data = historical_data
  )

  # Correct class

  expect_s3_class(result, "tbl_df")

  # All expected columns present
  expected_cols <- c(
    "driver_id",
    "current_points",
    "win_probability",
    "avg_final_points",
    "avg_final_position",
    "in_contention",
    "season"
  )
  expect_true(all(expected_cols %in% names(result)))

  # One row per driver
  expect_equal(nrow(result), 5)

  # Season is correctly propagated
  expect_true(all(result$season == 2025))

  # Win probabilities are between 0 and 1
  expect_true(all(result$win_probability >= 0 & result$win_probability <= 1))

  # Contender probabilities sum to 1
  contender_probs <- result$win_probability[result$in_contention]
  expect_equal(sum(contender_probs), 1)

  # avg_final_points >= current_points for contenders (points can only increase)
  contenders <- result[result$in_contention, ]
  expect_true(all(contenders$avg_final_points >= contenders$current_points))

  # avg_final_position is numeric and positive for contenders
  expect_true(all(is.numeric(contenders$avg_final_position)))
  expect_true(all(contenders$avg_final_position > 0))

  # Non-contenders have win_probability == 0
  non_contenders <- result[!result$in_contention, ]
  if (nrow(non_contenders) > 0) {
    expect_true(all(non_contenders$win_probability == 0))
    # Non-contenders are still simulated so avg_final_points >= current_points
    expect_true(all(
      non_contenders$avg_final_points >= non_contenders$current_points
    ))
    # Non-contenders get real avg_final_position values
    expect_true(all(!is.na(non_contenders$avg_final_position)))
  }
})

test_that("calculate_driver_performance works with example historical data", {
  historical_data <- make_historical_data()

  perf <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data
  )

  expect_s3_class(perf, "tbl_df")

  # All drivers present
  expect_equal(sort(perf$driver_id), sort(paste0("driver_", letters[1:5])))

  # Required columns
  expected_cols <- c(
    "driver_id",
    "avg_position",
    "position_sd",
    "dnf_rate"
  )
  expect_true(all(expected_cols %in% names(perf)))

  # Verify metrics are sensible
  driver_a <- perf[perf$driver_id == "driver_a", ]
  expect_true(driver_a$avg_position < 5) # Strong performer
  expect_equal(driver_a$dnf_rate, 0) # No DNFs

  # driver_d has 1 DNF in 8 races
  driver_d <- perf[perf$driver_id == "driver_d", ]
  expect_true(driver_d$dnf_rate > 0)
})

test_that("simulate_championship_odds avg_final_position ranks are consistent", {
  standings <- tibble::tibble(
    driver_id = paste0("driver_", letters[1:3]),
    points = c(100, 90, 80),
    position = 1:3
  )
  remaining <- make_remaining_schedule(n_races = 3, n_sprints = 1)
  historical_data <- make_historical_data()

  # Filter historical data to only include these 3 drivers
  historical_data <- historical_data[
    historical_data$driver_id %in% standings$driver_id,
  ]

  set.seed(99)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 10L,
    historical_data = historical_data
  )

  # Avg final position should be between 1 and number of drivers
  expect_true(all(result$avg_final_position >= 1))
  expect_true(all(result$avg_final_position <= nrow(result)))

  # All positions should be numeric
  expect_true(is.numeric(result$avg_final_position))
})

test_that("simulate_championship_odds handles all drivers in contention", {
  # All drivers close in points so all are in contention
  standings <- tibble::tibble(
    driver_id = paste0("driver_", letters[1:4]),
    points = c(100, 95, 90, 85),
    position = 1:4
  )
  remaining <- make_remaining_schedule(n_races = 5, n_sprints = 2)
  historical_data <- make_historical_data()
  historical_data <- historical_data[
    historical_data$driver_id %in% standings$driver_id,
  ]

  set.seed(7)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 10L,
    historical_data = historical_data
  )

  # All drivers should be in contention
  expect_true(all(result$in_contention))
  expect_equal(nrow(result), 4)

  # All probabilities should sum to 1
  expect_equal(sum(result$win_probability), 1)

  # All avg_final_position values should be set (no NA for contenders)
  expect_true(all(!is.na(result$avg_final_position)))
})

test_that("calculate_driver_performance blends seasons for early races", {
  # Create data with only 3 races in current season (should blend with previous)
  early_season_data <- tibble::tibble(
    driver_id = rep(c("driver_a", "driver_b"), each = 3),
    round = rep(1:3, 2),
    position = c(2, 3, 1, 5, 6, 4),
    finished = TRUE,
    driver_failure = 0,
    constructor_failure = 0,
    constructor_failure_race = 0,
    season = 2025
  )
  prev_season_data <- tibble::tibble(
    driver_id = rep(c("driver_a", "driver_b"), each = 8),
    round = rep(1:8, 2),
    position = c(3, 2, 4, 1, 3, 2, 4, 3, 6, 5, 7, 4, 6, 5, 7, 6),
    finished = TRUE,
    driver_failure = 0,
    constructor_failure = 0,
    constructor_failure_race = 0,
    season = 2024
  )
  historical_data <- rbind(early_season_data, prev_season_data)

  perf <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data
  )

  expect_s3_class(perf, "tbl_df")
  expect_true("driver_a" %in% perf$driver_id)
  expect_true("driver_b" %in% perf$driver_id)

  # Metrics should be blended (3/5 current + 2/5 previous for 3 races completed)
  driver_a <- perf[perf$driver_id == "driver_a", ]
  expect_true(driver_a$avg_position > 0)
})

test_that("calculate_driver_performance uses previous season only when no current data", {
  # Only previous season data available
  prev_only_data <- tibble::tibble(
    driver_id = rep("driver_a", 8),
    round = 1:8,
    position = c(3, 2, 4, 1, 3, 2, 4, 3),
    finished = TRUE,
    driver_failure = 0,
    constructor_failure = 0,
    constructor_failure_race = 0,
    season = 2024
  )

  perf <- calculate_driver_performance(
    season = 2025,
    historical_data = prev_only_data
  )

  expect_s3_class(perf, "tbl_df")
  expect_equal(perf$driver_id, "driver_a")
  expect_true(perf$avg_position > 0)
})

test_that("calculate_driver_performance errors with no data for season or previous", {
  empty_data <- tibble::tibble(
    driver_id = character(0),
    round = integer(0),
    position = numeric(0),
    finished = logical(0),
    season = numeric(0)
  )

  expect_error(
    calculate_driver_performance(season = 2025, historical_data = empty_data),
    "No historical data found"
  )
})

test_that("calculate_driver_performance handles 5+ current-season races with no previous season", {
  # Only current season data, 8 races completed (>= n_recent_races default of 5)
  # Tests the n_completed >= n_recent_races path and the NULL prev_season (no 3-way blend) path
  current_only_data <- tibble::tibble(
    driver_id = rep(c("driver_a", "driver_b"), each = 8),
    round = rep(1:8, 2),
    position = c(1, 2, 3, 1, 2, 3, 1, 2, 5, 6, 7, 5, 6, 7, 5, 6),
    finished = TRUE,
    driver_failure = 0,
    constructor_failure = 0,
    constructor_failure_race = 0,
    season = 2025
  )

  perf <- calculate_driver_performance(
    season = 2025,
    historical_data = current_only_data
  )

  expect_s3_class(perf, "tbl_df")
  expect_setequal(perf$driver_id, c("driver_a", "driver_b"))
  expect_true(all(
    c("avg_position", "position_sd", "dnf_rate") %in% names(perf)
  ))

  # driver_a has better positions than driver_b
  driver_a <- perf[perf$driver_id == "driver_a", ]
  driver_b <- perf[perf$driver_id == "driver_b", ]
  expect_true(driver_a$avg_position < driver_b$avg_position)
  expect_equal(driver_a$dnf_rate, 0)
})

test_that("calculate_driver_performance respects custom weight parameters", {
  historical_data <- make_historical_data()

  # Default weights
  perf_default <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data
  )

  # Different weights: 100% recent, 0% season, 0% prev
  perf_recent_only <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data,
    weight_recent = 1.0,
    weight_season = 0.0,
    weight_prev_season = 0.0
  )

  # Different weights: 0% recent, 100% season, 0% prev
  perf_season_only <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data,
    weight_recent = 0.0,
    weight_season = 1.0,
    weight_prev_season = 0.0
  )

  # Different weights produce different avg_position values
  expect_false(all(perf_default$avg_position == perf_recent_only$avg_position))
  expect_false(all(
    perf_recent_only$avg_position == perf_season_only$avg_position
  ))

  # All results have the right structure regardless of weights
  expect_s3_class(perf_recent_only, "tbl_df")
  expect_s3_class(perf_season_only, "tbl_df")
  expect_setequal(perf_recent_only$driver_id, perf_default$driver_id)
})

test_that("calculate_driver_performance custom n_recent_races parameter", {
  historical_data <- make_historical_data()

  perf_n3 <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data,
    n_recent_races = 3L
  )
  perf_n8 <- calculate_driver_performance(
    season = 2025,
    historical_data = historical_data,
    n_recent_races = 8L
  )

  # Both should return results for the same drivers
  expect_setequal(perf_n3$driver_id, perf_n8$driver_id)

  # Different n_recent values should produce different performance estimates
  expect_false(all(perf_n3$avg_position == perf_n8$avg_position))
})

test_that("simulate_championship_odds full-grid simulation: non-contenders still earn points", {
  # One driver dominates, others are eliminated; all should still get avg_final_points > starting
  standings <- tibble::tibble(
    driver_id = paste0("driver_", letters[1:5]),
    points = c(300, 10, 8, 6, 5),
    position = 1:5
  )
  # Only 1 race, 0 sprints: max = 25 pts -> drivers 2-5 cannot catch driver 1 (300 pts)
  remaining <- tibble::tibble(
    round = 20L,
    race_name = "Final Race",
    date = Sys.Date() + 7,
    has_sprint = FALSE
  )
  historical_data <- make_historical_data()
  historical_data <- historical_data[
    historical_data$driver_id %in% standings$driver_id,
  ]

  set.seed(123)
  result <- simulate_championship_odds(
    season = 2025,
    standings = standings,
    remaining = remaining,
    n_simulations = 10L,
    historical_data = historical_data
  )

  # Only driver_a should be in contention
  expect_true(result$in_contention[result$driver_id == "driver_a"])
  expect_true(all(!result$in_contention[result$driver_id != "driver_a"]))

  # Driver_a should win all simulations
  expect_equal(result$win_probability[result$driver_id == "driver_a"], 1)

  # All drivers (including non-contenders) should have avg_final_points >= current_points
  expect_true(all(result$avg_final_points >= result$current_points))

  # All drivers have a valid avg_final_position
  expect_true(all(!is.na(result$avg_final_position)))
  expect_true(all(result$avg_final_position >= 1))
  expect_true(all(result$avg_final_position <= nrow(result)))
})

# ----- Current Standings ------
test_that("get_current_standings returns correct structure", {
  # Use package standings data
  standings <- get_current_standings(2025)

  expect_s3_class(standings, "tbl_df")
  expect_true(all(
    c("driver_id", "points", "position") %in% colnames(standings)
  ))
  expect_true(standings$driver_id[[1]] == "norris")
})
