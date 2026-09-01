# Tests for Monte Carlo qualifying simulation functions.

# ---- simulation_params() qualifying params ----------------------------------

test_that("simulation_params() includes qualifying-specific parameters (#noissue)", {
  p <- simulation_params()
  expect_in(
    c(
      "quali_default_position_sd",
      "quali_qgap_sd_weight",
      "quali_practice_weight",
      "quali_wet_sd_multiplier"
    ),
    names(p)
  )
  expect_gt(p$quali_default_position_sd, 0)
  expect_gt(p$quali_wet_sd_multiplier, 1)
  expect_gte(p$quali_practice_weight, 0)
  expect_lte(p$quali_practice_weight, 1)
})

# ---- .calculate_quali_sim_metrics() -----------------------------------------

test_that(".calculate_quali_sim_metrics() returns a tibble with correct columns (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("VER", "HAM", "NOR"),
    season = 2025L,
    round = 1L
  )
  hist_data <- tibble::tibble()

  result <- f1predicter:::.calculate_quali_sim_metrics(
    new_data = new_data,
    historical_data = hist_data,
    season = 2025L,
    round = 1L
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "position_sd"))
  expect_equal(nrow(result), 3L)
  expect_true(all(result$position_sd > 0))
})

test_that(".calculate_quali_sim_metrics() uses fallback SD when no historical data (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("VER", "HAM"),
    season = 2025L,
    round = 1L
  )
  params <- simulation_params()

  result <- f1predicter:::.calculate_quali_sim_metrics(
    new_data = new_data,
    historical_data = tibble::tibble(),
    season = 2025L,
    round = 1L,
    params = params
  )

  expect_equal(result$position_sd, rep(params$quali_default_position_sd, 2L))
})

test_that(".calculate_quali_sim_metrics() scales SD by driver_avg_qgap (#noissue)", {
  params <- simulation_params()

  new_data_low <- tibble::tibble(
    driver_id = "VER",
    season = 2025L,
    round = 1L,
    driver_avg_qgap = 0
  )
  new_data_high <- tibble::tibble(
    driver_id = "VER",
    season = 2025L,
    round = 1L,
    driver_avg_qgap = 2
  )

  res_low <- f1predicter:::.calculate_quali_sim_metrics(
    new_data_low,
    tibble::tibble(),
    2025L,
    1L
  )
  res_high <- f1predicter:::.calculate_quali_sim_metrics(
    new_data_high,
    tibble::tibble(),
    2025L,
    1L
  )

  expect_gt(res_high$position_sd, res_low$position_sd)
})

test_that(".calculate_quali_sim_metrics() applies wet-weather SD multiplier (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = "VER",
    season = 2025L,
    round = 1L
  )
  params <- simulation_params()

  res_dry <- f1predicter:::.calculate_quali_sim_metrics(
    new_data,
    tibble::tibble(),
    2025L,
    1L,
    weather = "dry",
    params = params
  )
  res_wet <- f1predicter:::.calculate_quali_sim_metrics(
    new_data,
    tibble::tibble(),
    2025L,
    1L,
    weather = "wet",
    params = params
  )

  expect_equal(
    res_wet$position_sd,
    res_dry$position_sd * params$quali_wet_sd_multiplier
  )
})

# ---- summarise_quali_simulations() ------------------------------------------

test_that("summarise_quali_simulations() returns expected column names (#noissue)", {
  n_d <- 5L
  n_s <- 100L
  mat <- matrix(
    sample(seq_len(n_d), n_d * n_s, replace = TRUE),
    nrow = n_d
  )
  ids <- paste0("driver_", letters[seq_len(n_d)])

  result <- summarise_quali_simulations(mat, ids, 2025L, 1L, n_s)

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "driver_id",
      "season",
      "round",
      "pole_prob",
      "top3_prob",
      "top10_prob",
      "likely_quali_position",
      "position_sd",
      ".probs"
    )
  )
  expect_equal(nrow(result), n_d)
})

test_that("summarise_quali_simulations() probabilities sum to ~1 (#noissue)", {
  n_d <- 10L
  n_s <- 1000L
  # Each simulation column must have unique positions 1:n_d (like real qualifying)
  mat <- replicate(n_s, sample(seq_len(n_d)))
  ids <- paste0("d", seq_len(n_d))
  result <- summarise_quali_simulations(mat, ids, 2025L, 1L, n_s)

  expect_equal(sum(result$pole_prob), 1, tolerance = 0.01)
  expect_true(all(result$top10_prob >= result$top3_prob))
  expect_true(all(result$top3_prob >= result$pole_prob))
})

test_that("summarise_quali_simulations() .probs is a matrix (#noissue)", {
  n_d <- 5L
  n_s <- 50L
  mat <- matrix(sample(seq_len(n_d), n_d * n_s, replace = TRUE), nrow = n_d)
  ids <- paste0("d", seq_len(n_d))
  result <- summarise_quali_simulations(mat, ids, 2025L, 1L, n_s)

  expect_true(is.matrix(result$.probs))
  expect_equal(nrow(result$.probs), n_d)
  expect_equal(ncol(result$.probs), n_d)
})

test_that("summarise_quali_simulations() likely_quali_position is in valid range (#noissue)", {
  n_d <- 8L
  n_s <- 200L
  mat <- matrix(sample(seq_len(n_d), n_d * n_s, replace = TRUE), nrow = n_d)
  ids <- paste0("d", seq_len(n_d))
  result <- summarise_quali_simulations(mat, ids, 2025L, 1L, n_s)

  expect_true(all(result$likely_quali_position >= 1))
  expect_true(all(result$likely_quali_position <= n_d))
})

# ---- simulate_quali() -------------------------------------------------------

test_that("simulate_quali() returns correct structure with mock model (#noissue)", {
  n_d <- 6L
  driver_ids <- paste0("driver_", letters[seq_len(n_d)])
  new_data <- tibble::tibble(
    driver_id = driver_ids,
    season = 2025L,
    round = 1L
  )

  fake_model <- list(
    quali_pos = structure(list(), class = "workflow")
  )

  local_mocked_bindings(
    .predict_quali_pos = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_quali_position = seq_len(nrow(new_data))
      )
    }
  )

  result <- simulate_quali(
    new_data = new_data,
    historical_data = tibble::tibble(),
    quali_models = fake_model,
    n_simulations = 100L
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "driver_id",
      "season",
      "round",
      "pole_prob",
      "top3_prob",
      "top10_prob",
      "likely_quali_position",
      "position_sd",
      ".probs"
    )
  )
  expect_equal(nrow(result), n_d)
  expect_true(all(result$pole_prob >= 0 & result$pole_prob <= 1))
  expect_equal(sum(result$pole_prob), 1, tolerance = 0.01)
})

test_that("simulate_quali() errors when quali_pos model is missing (#noissue)", {
  new_data <- tibble::tibble(driver_id = "VER", season = 2025L, round = 1L)
  expect_error(
    simulate_quali(
      new_data,
      tibble::tibble(),
      quali_models = list(other_model = structure(list(), class = "workflow"))
    ),
    "quali_pos"
  )
})

test_that("simulate_quali() practice weight blends mean position (#noissue)", {
  n_d <- 5L
  driver_ids <- paste0("d", seq_len(n_d))
  new_data <- tibble::tibble(
    driver_id = driver_ids,
    season = 2025L,
    round = 1L,
    practice_optimal_rank = rev(seq_len(n_d)) # reverse order of ML prediction
  )

  fake_model <- list(
    quali_pos = structure(list(), class = "workflow")
  )

  local_mocked_bindings(
    .predict_quali_pos = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_quali_position = as.numeric(seq_len(nrow(new_data)))
      )
    }
  )

  params_no_practice <- simulation_params()
  params_no_practice$quali_practice_weight <- 0

  params_with_practice <- simulation_params()
  params_with_practice$quali_practice_weight <- 0.5

  set.seed(42L)
  res_no_prac <- simulate_quali(
    new_data,
    tibble::tibble(),
    quali_models = fake_model,
    n_simulations = 200L,
    params = params_no_practice
  )

  set.seed(42L)
  res_with_prac <- simulate_quali(
    new_data,
    tibble::tibble(),
    quali_models = fake_model,
    n_simulations = 200L,
    params = params_with_practice
  )

  # With practice blending the mean positions become more uniform, so the
  # likely_quali_positions should differ between the two runs.
  expect_false(
    identical(
      res_no_prac$likely_quali_position,
      res_with_prac$likely_quali_position
    )
  )
})

test_that("simulate_quali() produces reproducible results within same day (#noissue)", {
  n_d <- 5L
  driver_ids <- paste0("d", seq_len(n_d))
  new_data <- tibble::tibble(
    driver_id = driver_ids,
    season = 2025L,
    round = 1L
  )
  fake_model <- list(
    quali_pos = structure(list(), class = "workflow")
  )
  local_mocked_bindings(
    .predict_quali_pos = function(new_data, model) {
      tibble::tibble(
        driver_id = new_data$driver_id,
        round = new_data$round,
        season = new_data$season,
        likely_quali_position = as.numeric(seq_len(nrow(new_data)))
      )
    }
  )

  res1 <- simulate_quali(
    new_data,
    tibble::tibble(),
    quali_models = fake_model,
    n_simulations = 200L
  )
  res2 <- simulate_quali(
    new_data,
    tibble::tibble(),
    quali_models = fake_model,
    n_simulations = 200L
  )

  expect_equal(res1$pole_prob, res2$pole_prob)
  expect_equal(res1$likely_quali_position, res2$likely_quali_position)
})
