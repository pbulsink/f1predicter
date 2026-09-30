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
      "default_dnf_rate",
      "quali_default_position_sd",
      "quali_qgap_sd_weight",
      "quali_practice_weight",
      "quali_wet_sd_multiplier",
      "ordinal_class_weight"
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

# ---- .calculate_race_sim_metrics() ------------------------------------------

test_that(".calculate_race_sim_metrics() returns a tibble with correct columns (#noissue)", {
  nd <- make_new_data()
  fake_perf <- tibble::tibble(
    driver_id = nd$driver_id,
    avg_position = seq_len(nrow(nd)),
    position_sd = rep(2, nrow(nd)),
    dnf_rate = rep(0.05, nrow(nd))
  )

  local_mocked_bindings(
    calculate_driver_performance = function(...) fake_perf
  )

  result <- f1predicter:::.calculate_race_sim_metrics(
    season = 2025L,
    circuit_id = "bahrain",
    new_data = nd,
    historical_data = tibble::tibble()
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "position_sd", "dnf_rate"))
  expect_equal(nrow(result), nrow(nd))
  expect_true(all(result$position_sd > 0))
  expect_true(all(result$dnf_rate >= 0 & result$dnf_rate <= 1))
})

test_that(".calculate_race_sim_metrics() uses default SD/DNF when performance data is missing for a driver (#noissue)", {
  nd <- make_new_data(n_drivers = 3)
  # Performance data only covers one of the three drivers; the other two
  # should fall back to params$default_position_sd / default_dnf_rate.
  fake_perf <- tibble::tibble(
    driver_id = nd$driver_id[1],
    avg_position = 1,
    position_sd = 3,
    dnf_rate = 0.1
  )

  local_mocked_bindings(
    calculate_driver_performance = function(...) fake_perf
  )

  params <- simulation_params()
  result <- f1predicter:::.calculate_race_sim_metrics(
    season = 2025L,
    circuit_id = "bahrain",
    new_data = nd,
    historical_data = tibble::tibble(),
    params = params
  )

  missing_rows <- result[result$driver_id != nd$driver_id[1], ]
  expect_true(all(
    missing_rows$dnf_rate == params$default_dnf_rate
  ))
})

test_that(".calculate_race_sim_metrics() scales SD by circuit overtaking difficulty (#noissue)", {
  fake_perf_fn <- function(...) {
    tibble::tibble(
      driver_id = "driver_a",
      avg_position = 1,
      position_sd = 2,
      dnf_rate = 0.05
    )
  }
  local_mocked_bindings(calculate_driver_performance = fake_perf_fn)

  params <- simulation_params()
  params$circuit_sd_scale <- TRUE

  nd_low_corr <- tibble::tibble(
    driver_id = "driver_a",
    grid_pos_corr_avg = 0.9 # high correlation -> tight order -> low multiplier
  )
  nd_high_corr <- tibble::tibble(
    driver_id = "driver_a",
    grid_pos_corr_avg = 0.1 # low correlation -> wide spread -> high multiplier
  )

  res_low <- f1predicter:::.calculate_race_sim_metrics(
    season = 2025L,
    circuit_id = "bahrain",
    new_data = nd_low_corr,
    historical_data = tibble::tibble(),
    params = params
  )
  res_high <- f1predicter:::.calculate_race_sim_metrics(
    season = 2025L,
    circuit_id = "bahrain",
    new_data = nd_high_corr,
    historical_data = tibble::tibble(),
    params = params
  )

  expect_gt(res_high$position_sd, res_low$position_sd)
})

test_that(".calculate_race_sim_metrics() disables circuit SD scaling when circuit_sd_scale is FALSE (#noissue)", {
  local_mocked_bindings(
    calculate_driver_performance = function(...) {
      tibble::tibble(
        driver_id = "driver_a",
        avg_position = 1,
        position_sd = 2,
        dnf_rate = 0.05
      )
    }
  )

  params <- simulation_params()
  params$circuit_sd_scale <- FALSE

  nd <- tibble::tibble(driver_id = "driver_a", grid_pos_corr_avg = 0.1)

  result <- f1predicter:::.calculate_race_sim_metrics(
    season = 2025L,
    circuit_id = "bahrain",
    new_data = nd,
    historical_data = tibble::tibble(),
    params = params
  )

  expect_equal(result$position_sd, 2)
})

# ---- simulate_race() --------------------------------------------------------

test_that("simulate_race() returns correct structure with mock model (#noissue)", {
  n_d <- 6L
  nd <- make_new_data(n_drivers = n_d)

  fake_model <- list(position = structure(list(), class = "workflow"))

  local_mocked_bindings(
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = seq_len(nrow(new_data))
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = nd$driver_id,
        position_sd = rep(1.5, n_d),
        dnf_rate = rep(0, n_d)
      )
    }
  )

  result <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    weather = "dry",
    n_simulations = 100L
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
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
  )
  expect_equal(nrow(result), n_d)
  expect_true(all(result$win_prob >= 0 & result$win_prob <= 1))
  expect_equal(sum(result$win_prob), 1, tolerance = 0.01)
})

test_that("simulate_race() errors when position model is missing (#noissue)", {
  nd <- make_new_data()
  expect_error(
    simulate_race(
      new_data = nd,
      results_models = list(other_model = structure(list(), class = "workflow"))
    ),
    "position"
  )
})

test_that("simulate_race() applies sprint result updates when provided (#noissue)", {
  n_d <- 4L
  nd <- make_new_data(n_drivers = n_d)
  fake_model <- list(position = structure(list(), class = "workflow"))

  local_mocked_bindings(
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = rep(2.5, nrow(new_data)) # tied ML mean for all
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = nd$driver_id,
        position_sd = rep(0.01, n_d), # near-deterministic
        dnf_rate = rep(0, n_d)
      )
    }
  )

  sprint_results <- tibble::tibble(
    driver_id = nd$driver_id,
    sprint_position = seq_len(n_d) # driver_a finished the sprint 1st, etc.
  )

  set.seed(321L)
  res_no_sprint <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    n_simulations = 50L
  )

  set.seed(321L)
  res_with_sprint <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    sprint_results = sprint_results,
    n_simulations = 50L
  )

  # With tied ML means, sprint results should be the only thing that
  # differentiates drivers, so the winner should track the sprint result.
  expect_false(
    identical(res_no_sprint$likely_position, res_with_sprint$likely_position)
  )
  expect_equal(
    res_with_sprint$driver_id[which.max(res_with_sprint$win_prob)],
    nd$driver_id[1]
  )
})

test_that("simulate_race() scales SD up for wet weather (#noissue)", {
  n_d <- 5L
  nd <- make_new_data(n_drivers = n_d)
  fake_model <- list(position = structure(list(), class = "workflow"))

  local_mocked_bindings(
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = seq_len(nrow(new_data))
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = nd$driver_id,
        position_sd = rep(1, n_d),
        dnf_rate = rep(0, n_d)
      )
    }
  )

  params <- simulation_params()

  set.seed(654L)
  res_dry <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    weather = "dry",
    n_simulations = 500L,
    params = params
  )

  set.seed(654L)
  res_wet <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    weather = "wet",
    n_simulations = 500L,
    params = params
  )

  # Wet weather widens the position SD, which should reduce how dominant the
  # top ML-favoured driver is (lower win_prob for driver_a).
  expect_lt(
    res_wet$win_prob[res_wet$driver_id == "driver_a"],
    res_dry$win_prob[res_dry$driver_id == "driver_a"]
  )
})

test_that("simulate_race() loads models from disk when results_models is a valid timing string (#noissue)", {
  loaded_timing <- NULL
  local_mocked_bindings(
    load_models = function(model_type, model_timing, engine) {
      loaded_timing <<- model_timing
      list(position = structure(list(), class = "workflow"))
    },
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = seq_len(nrow(new_data))
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = new_data_global$driver_id,
        position_sd = rep(1, nrow(new_data_global)),
        dnf_rate = rep(0, nrow(new_data_global))
      )
    },
    .package = "f1predicter"
  )

  nd <- make_new_data(n_drivers = 3)
  new_data_global <- nd

  expect_message(
    result <- simulate_race(
      new_data = nd,
      results_models = "late",
      n_simulations = 20L
    ),
    "Loading 'late'"
  )

  expect_identical(loaded_timing, "late")
  expect_s3_class(result, "tbl_df")
})

test_that("simulate_race() errors on invalid string timing (#noissue)", {
  nd <- make_new_data()
  expect_error(
    simulate_race(new_data = nd, results_models = "bad_timing"),
    "must be one of"
  )
})

test_that("simulate_race() blends in position_class ordinal predictions when weighted (#noissue)", {
  n_d <- 4L
  nd <- make_new_data(n_drivers = n_d)
  fake_model <- list(
    position = structure(list(), class = "workflow"),
    position_class = structure(list(), class = "workflow")
  )

  local_mocked_bindings(
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = rep(2.5, nrow(new_data)) # tied ML mean
      )
    },
    .predict_position_class = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        expected_position_class = as.numeric(seq_len(nrow(new_data)))
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = nd$driver_id,
        position_sd = rep(0.01, n_d), # near-deterministic
        dnf_rate = rep(0, n_d)
      )
    }
  )

  params_no_blend <- simulation_params()
  params_no_blend$ordinal_class_weight <- 0

  params_with_blend <- simulation_params()
  params_with_blend$ordinal_class_weight <- 1

  set.seed(777L)
  res_no_blend <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    n_simulations = 100L,
    params = params_no_blend
  )

  set.seed(777L)
  res_with_blend <- simulate_race(
    new_data = nd,
    results_models = fake_model,
    n_simulations = 100L,
    params = params_with_blend
  )

  expect_false(
    identical(res_no_blend$likely_position, res_with_blend$likely_position)
  )
  expect_equal(
    res_with_blend$driver_id[which.max(res_with_blend$win_prob)],
    nd$driver_id[1]
  )
})

test_that("simulate_race() ignores position_class when ordinal_class_weight is 0 (default) (#noissue)", {
  n_d <- 4L
  nd <- make_new_data(n_drivers = n_d)
  fake_model <- list(
    position = structure(list(), class = "workflow"),
    position_class = structure(list(), class = "workflow")
  )

  class_called <- FALSE
  local_mocked_bindings(
    .predict_position = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_position = seq_len(nrow(new_data))
      )
    },
    .predict_position_class = function(new_data, model) {
      class_called <<- TRUE
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        expected_position_class = as.numeric(seq_len(nrow(new_data)))
      )
    },
    .calculate_race_sim_metrics = function(...) {
      tibble::tibble(
        driver_id = nd$driver_id,
        position_sd = rep(1, n_d),
        dnf_rate = rep(0, n_d)
      )
    }
  )

  simulate_race(
    new_data = nd,
    results_models = fake_model,
    n_simulations = 50L
  )

  expect_false(class_called)
})
