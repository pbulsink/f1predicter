# ---- Helpers ---------------------------------------------------------------

make_new_data <- function(n_drivers = 5, season = 2025, round = 1) {
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n_drivers)]),
    constructor_id = rep("constructor_1", n_drivers),
    season = season,
    round = round,
    circuit_id = "bahrain",
    grid = seq_len(n_drivers),
    grid_pos_corr_avg = rep(0.6, n_drivers),
    driver_failure_avg = rep(0.05, n_drivers),
    constructor_failure_avg = rep(0.03, n_drivers)
  )
}

make_sim_matrix <- function(n_drivers = 5, n_sims = 100) {
  mat <- matrix(NA_integer_, nrow = n_drivers, ncol = n_sims)
  for (sim in seq_len(n_sims)) {
    mat[, sim] <- sample.int(n_drivers, n_drivers, replace = FALSE)
  }
  mat
}

# ---- simulation_params() ---------------------------------------------------

test_that("simulation_params() returns a list with expected keys (#noissue)", {
  p <- simulation_params()
  expect_type(p, "list")
  expect_named(
    p,
    c(
      "n_simulations",
      "circuit_sd_scale",
      "sprint_mean_weight",
      "sprint_dnf_scale",
      "wet_sd_multiplier",
      "default_position_sd",
      "default_dnf_rate"
    ),
    ignore.order = TRUE
  )
  expect_equal(p$n_simulations, 10000L)
  expect_true(p$n_simulations > 0)
  expect_true(p$sprint_mean_weight > 0 && p$sprint_mean_weight < 1)
  expect_true(p$wet_sd_multiplier > 1)
})

# ---- summarise_simulations() -----------------------------------------------

test_that("summarise_simulations() returns correct structure (#noissue)", {
  n_drivers <- 5
  n_sims <- 200
  ids <- paste0("driver_", letters[seq_len(n_drivers)])
  mat <- make_sim_matrix(n_drivers, n_sims)

  result <- summarise_simulations(mat, ids, 2025L, 1L, n_sims)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), n_drivers)
  expected_cols <- c(
    "driver_id",
    "season",
    "round",
    "win_prob",
    "podium_prob",
    "top10_prob",
    "likely_position",
    "expected_points",
    "position_sd",
    ".probs"
  )
  expect_named(result, expected_cols, ignore.order = FALSE)
})

test_that("summarise_simulations() probabilities sum to approximately 1 (#noissue)", {
  n_drivers <- 5
  n_sims <- 1000
  ids <- paste0("driver_", letters[seq_len(n_drivers)])
  mat <- make_sim_matrix(n_drivers, n_sims)

  result <- summarise_simulations(mat, ids, 2025L, 1L, n_sims)

  # win probabilities should sum to ~1 (one winner per sim, no DNFs here)
  expect_equal(sum(result$win_prob), 1, tolerance = 0.01)

  # each driver's row in .probs should sum to ~1
  probs <- result$.probs
  row_sums <- rowSums(probs)
  expect_true(all(abs(row_sums - 1) < 0.01))
})

test_that("summarise_simulations() .probs matrix has correct dimensions (#noissue)", {
  n_drivers <- 5
  n_sims <- 100
  ids <- paste0("driver_", letters[seq_len(n_drivers)])
  mat <- make_sim_matrix(n_drivers, n_sims)

  result <- summarise_simulations(mat, ids, 2025L, 1L, n_sims)
  probs <- result$.probs

  expect_equal(nrow(probs), n_drivers)
  expect_equal(ncol(probs), n_drivers) # positions 1..n_drivers
})

test_that("summarise_simulations() handles DNFs (NA positions) (#noissue)", {
  n_drivers <- 4
  n_sims <- 100
  ids <- paste0("driver_", letters[seq_len(n_drivers)])

  # Half the sims have driver_a DNF (NA)
  mat <- make_sim_matrix(n_drivers, n_sims)
  mat[1, seq(1, n_sims, by = 2)] <- NA_integer_

  result <- summarise_simulations(mat, ids, 2025L, 1L, n_sims)

  # driver_a's win_prob should be halved compared to if they always finished
  expect_true(result$win_prob[1] < 0.5)
  expect_false(is.na(result$win_prob[1]))
})

test_that("summarise_simulations() expected_points is non-negative (#noissue)", {
  n_drivers <- 10
  n_sims <- 500
  ids <- paste0("driver_", letters[seq_len(n_drivers)])

  # Deterministic: driver 1 always wins (position = 1), driver n always last
  mat <- matrix(NA_integer_, nrow = n_drivers, ncol = n_sims)
  for (sim in seq_len(n_sims)) {
    mat[, sim] <- seq_len(n_drivers)
  }

  result <- summarise_simulations(mat, ids, 2025L, 1L, n_sims)

  expect_true(all(result$expected_points >= 0))
  # Driver in P1 every sim should have more expected points than P10
  expect_gt(
    result$expected_points[result$driver_id == ids[1]],
    result$expected_points[result$driver_id == ids[n_drivers]]
  )
})

# ---- .apply_sprint_update() ------------------------------------------------

test_that(".apply_sprint_update() adjusts means toward sprint results (#noissue)", {
  driver_ids <- c("driver_a", "driver_b", "driver_c")
  avg_positions <- c(3, 6, 9)
  sprint_results <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    sprint_position = c(1L, 10L)
  )

  updated <- f1predicter:::.apply_sprint_update(
    driver_ids,
    avg_positions,
    sprint_results,
    weight = 0.15
  )

  # driver_a: sprint was better (1 < 3), so mean should decrease
  expect_lt(updated[1], avg_positions[1])
  # driver_b: sprint was worse (10 > 6), so mean should increase
  expect_gt(updated[2], avg_positions[2])
  # driver_c: no sprint data, mean unchanged
  expect_equal(updated[3], avg_positions[3])
})

test_that(".apply_sprint_update() weight = 0 leaves means unchanged (#noissue)", {
  driver_ids <- c("driver_a", "driver_b")
  avg_positions <- c(3, 6)
  sprint_results <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    sprint_position = c(1L, 20L)
  )

  updated <- f1predicter:::.apply_sprint_update(
    driver_ids,
    avg_positions,
    sprint_results,
    weight = 0
  )

  expect_equal(updated, avg_positions)
})

# ---- .resolve_weather() ----------------------------------------------------

test_that(".resolve_weather() returns valid weather strings (#noissue)", {
  nd <- make_new_data()
  valid <- c("dry", "wet", "cold", "warm", "cloudy", "unknown")
  for (w in valid) {
    expect_equal(f1predicter:::.resolve_weather(nd, w), w)
  }
})

test_that(".resolve_weather() falls back to 'dry' for unrecognised strings (#noissue)", {
  nd <- make_new_data()
  expect_warning(
    result <- f1predicter:::.resolve_weather(nd, "foggy"),
    regexp = "not recognised"
  )
  expect_equal(result, "dry")
})
