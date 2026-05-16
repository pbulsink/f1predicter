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
  expect_true("has_sprint" %in% names(result))
  expect_true("sprint_grid" %in% names(result))
  expect_true("sprint_finish_pos" %in% names(result))
  expect_true("sprint_points" %in% names(result))
  expect_true("sprint_era" %in% names(result))
  expect_true(all(result$has_sprint == "No"))

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

  driver_seed <- historical_data |>
    dplyr::slice(1)
  target_driver <- as.character(driver_seed$driver_id)
  target_constructor <- as.character(driver_seed$constructor_id)
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
  # These gap defaults are hard-coded in generate_new_data() when no live laps exist.
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

test_that("ensemble prediction helpers error clearly when stacks is unavailable (#noissue)", {
  new_data <- tibble::tibble(driver_id = "driver_a", round = 1L, season = 2024L)
  fake_stack <- structure(list(), class = "model_stack")

  local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) {
      if (identical(package, "stacks")) {
        return(FALSE)
      }
      base::requireNamespace(package, quietly = quietly)
    },
    .package = "base"
  )

  expect_error(
    predict_quali_pole(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_quali_pos(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_quali_pos_class(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_winner(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_podium(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_t10(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_position(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
  expect_error(
    predict_position_class(new_data, fake_stack),
    "must be installed to predict with an ensemble model"
  )
})

# ---- load_models() validation -----------------------------------------------

test_that("load_models() errors when f1predicter.models option is unset (#noissue)", {
  withr::local_options(list(f1predicter.models = NULL))
  expect_error(
    load_models("quali", "early"),
    "not set"
  )
})

test_that("load_models() errors on invalid model_type (#noissue)", {
  withr::local_options(list(f1predicter.models = tempdir()))
  expect_error(
    load_models("bad_type", "early"),
    "quali.*results"
  )
})

test_that("load_models() errors on invalid model_timing for quali (#noissue)", {
  withr::local_options(list(f1predicter.models = tempdir()))
  expect_error(
    load_models("quali", "after_quali"),
    "invalid"
  )
})

test_that("load_models() errors on invalid model_timing for results (#noissue)", {
  withr::local_options(list(f1predicter.models = tempdir()))
  expect_error(
    load_models("results", "bad_timing"),
    "invalid"
  )
})

test_that("load_models() errors when model file does not exist (#noissue)", {
  tmp <- withr::local_tempdir()
  withr::local_options(list(f1predicter.models = tmp))
  expect_error(
    load_models("quali", "early", "ranger"),
    "not found"
  )
})

# ---- Helpers: skip condition ------------------------------------------------

.models_dir <- local({
  # system.file("", package = "f1predicter") returns the package root when
  # loaded via devtools::load_all(), so models/ lives directly inside it.
  candidate <- file.path(system.file("", package = "f1predicter"), "models")
  if (dir.exists(candidate)) candidate else NULL
})

.has_ensemble_models <- function(type, timing) {
  if (is.null(.models_dir)) {
    return(FALSE)
  }
  fname <- paste0(type, "_", timing, "_ensemble_models.rds")
  file.exists(file.path(.models_dir, fname))
}

# TRUE when the position_class/quali_pos_class sub-model is a valid model object
# (not a plain list, which can happen when butcher strips too much).
.has_usable_class_model <- function(type, timing) {
  if (!.has_ensemble_models(type, timing)) {
    return(FALSE)
  }
  fname <- paste0(type, "_", timing, "_ensemble_models.rds")
  models <- readRDS(file.path(.models_dir, fname))
  key <- if (type == "quali") "quali_pos_class" else "position_class"
  m <- models[[key]]
  inherits(m, "model_stack") ||
    inherits(m, "workflow") ||
    inherits(m, "_last_fit")
}

# ---- Cached model: load_models() round-trip ---------------------------------

test_that("load_models() returns a named list for quali early ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  models <- load_models("quali", "early", "ensemble")

  expect_type(models, "list")
  expect_in(c("quali_pole", "quali_pos", "quali_pos_class"), names(models))
})

test_that("load_models() returns a named list for results early ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  models <- load_models("results", "early", "ensemble")

  expect_type(models, "list")
  expect_in(
    c("win", "podium", "t10", "position", "position_class"),
    names(models)
  )
})

test_that("load_models() returns a named list for results after_quali ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "after_quali"),
    "Cached after_quali models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  models <- load_models("results", "after_quali", "ensemble")

  expect_type(models, "list")
  expect_in(
    c("win", "podium", "t10", "position", "position_class"),
    names(models)
  )
})

# ---- Cached model: individual predict_* functions ---------------------------

test_that("predict_quali_pole() returns correct structure with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("quali", "early", "ensemble")

  result <- predict_quali_pole(new_data, models$quali_pole)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "round", "season", "pole_odd"))
  expect_equal(nrow(result), nrow(new_data))
  expect_true(all(result$pole_odd >= 0 & result$pole_odd <= 1))
  expect_equal(sum(result$pole_odd), 1, tolerance = 1e-6)
})

test_that("predict_quali_pos() returns correct structure with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("quali", "early", "ensemble")

  result <- predict_quali_pos(new_data, models$quali_pos)

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("driver_id", "round", "season", "likely_quali_position")
  )
  expect_equal(nrow(result), nrow(new_data))
  expect_true(all(is.finite(result$likely_quali_position)))
})

test_that("predict_quali_pos_class() returns probs matrix with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  skip_if(
    !.has_usable_class_model("quali", "early"),
    "quali_pos_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("quali", "early", "ensemble")

  # The ordinal model expects ensemble_pole_pred and ensemble_pos_pred
  pole_preds <- stats::predict(models$quali_pole, new_data, type = "prob")
  pos_preds <- stats::predict(models$quali_pos, new_data, type = "numeric")
  nd_aug <- new_data |>
    dplyr::mutate(
      ensemble_pole_pred = pole_preds$.pred_1,
      ensemble_pos_pred = pos_preds$.pred
    )

  result <- predict_quali_pos_class(nd_aug, models$quali_pos_class)

  expect_s3_class(result, "tbl_df")
  expect_in(
    c("driver_id", "round", "season", "likely_quali_position_class", ".probs"),
    names(result)
  )
  expect_equal(nrow(result), nrow(new_data))
  expect_true(is.matrix(result$.probs[[1]]))
})

# ---- Cached model: predict_quali_round() wrapper ----------------------------

test_that("predict_quali_round() returns joined predictions with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  skip_if(
    !.has_usable_class_model("quali", "early"),
    "quali_pos_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("quali", "early", "ensemble")

  result <- predict_quali_round(new_data, models)

  expect_s3_class(result, "tbl_df")
  expect_in(
    c(
      "driver_id",
      "round",
      "season",
      "pole_odd",
      "likely_quali_position",
      "likely_quali_position_class"
    ),
    names(result)
  )
  expect_equal(nrow(result), nrow(new_data))
  expect_equal(sum(result$pole_odd), 1, tolerance = 1e-6)
})

test_that("predict_quali_round() auto-loads models when NULL is passed (#noissue)", {
  skip_if(
    !.has_ensemble_models("quali", "early"),
    "Cached quali early models not found"
  )
  skip_if(
    !.has_usable_class_model("quali", "early"),
    "quali_pos_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )

  result <- expect_message(
    predict_quali_round(new_data, quali_models = NULL, engine = "ensemble"),
    "Loading"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_quali_round() errors when required models are missing from list (#noissue)", {
  expect_error(
    predict_quali_round(
      tibble::tibble(),
      quali_models = list(quali_pole = NULL)
    ),
    "must contain"
  )
})

# ---- Cached model: individual results predict_* functions -------------------

test_that("predict_winner() returns win_odd between 0 and 1 with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "early", "ensemble")

  result <- predict_winner(new_data, models$win)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "round", "season", "win_odd"))
  expect_equal(nrow(result), nrow(new_data))
  expect_true(all(result$win_odd >= 0 & result$win_odd <= 1))
})

test_that("predict_podium() returns podium_odd between 0 and 1 with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "early", "ensemble")

  result <- predict_podium(new_data, models$podium)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "round", "season", "podium_odd"))
  expect_true(all(result$podium_odd >= 0 & result$podium_odd <= 1))
})

test_that("predict_t10() returns t10_odd between 0 and 1 with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "early", "ensemble")

  result <- predict_t10(new_data, models$t10)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "round", "season", "t10_odd"))
  expect_true(all(result$t10_odd >= 0 & result$t10_odd <= 1))
})

test_that("predict_position() returns numeric position with cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "early", "ensemble")

  result <- predict_position(new_data, models$position)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("driver_id", "round", "season", "likely_position"))
  expect_equal(nrow(result), nrow(new_data))
  expect_true(all(is.finite(result$likely_position)))
})

# ---- Cached model: predict_round() wrapper ----------------------------------

test_that("predict_round() returns all outcome columns with cached early ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  skip_if(
    !.has_usable_class_model("results", "early"),
    "position_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "early", "ensemble")

  result <- predict_round(new_data, models)

  expect_s3_class(result, "tbl_df")
  expect_in(
    c(
      "driver_id",
      "round",
      "season",
      "win_odd",
      "podium_odd",
      "t10_odd",
      "likely_position",
      "likely_position_class"
    ),
    names(result)
  )
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() auto-loads models when NULL is passed (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "early"),
    "Cached results early models not found"
  )
  skip_if(
    !.has_usable_class_model("results", "early"),
    "position_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )

  result <- expect_message(
    predict_round(new_data, results_models = NULL, engine = "ensemble"),
    "Loading"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() errors when required models are missing from list (#noissue)", {
  expect_error(
    predict_round(
      tibble::tibble(),
      results_models = list(win = NULL)
    ),
    "must contain"
  )
})

test_that("predict_round() with after_quali timing uses cached ensemble (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "after_quali"),
    "Cached after_quali models not found"
  )
  skip_if(
    !.has_usable_class_model("results", "after_quali"),
    "position_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )
  models <- load_models("results", "after_quali", "ensemble")

  result <- predict_round(new_data, models)

  expect_s3_class(result, "tbl_df")
  expect_in(
    c("win_odd", "podium_odd", "t10_odd", "likely_position"),
    names(result)
  )
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() with late quali timing loads late models (#noissue)", {
  skip_if(
    !.has_ensemble_models("results", "late"),
    "Cached late results models not found"
  )
  skip_if(
    !.has_usable_class_model("results", "late"),
    "position_class model is not a usable model object (may be over-butchered)"
  )
  withr::local_options(list(f1predicter.models = .models_dir))

  new_data <- generate_new_data(
    season = 2025,
    round = 1,
    historical_data = cleaned_data,
    use_live_data = FALSE
  )

  result <- expect_message(
    predict_round(new_data, results_models = "late", engine = "ensemble"),
    "Loading 'late'"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

# ---- predict_round() / predict_quali_round() with invalid string timing -----

test_that("predict_round() errors on invalid string timing (#noissue)", {
  withr::local_options(list(f1predicter.models = tempdir()))
  expect_error(
    predict_round(tibble::tibble(), results_models = "bad_timing"),
    "must be one of"
  )
})

test_that("predict_quali_round() errors on invalid string timing (#noissue)", {
  withr::local_options(list(f1predicter.models = tempdir()))
  expect_error(
    predict_quali_round(tibble::tibble(), quali_models = "bad_timing"),
    "must be one of"
  )
})

test_that("predict_round() auto-detects after_quali timing from q percentage columns (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026),
    q_min_perc = c(1.01, 1.02)
  )
  loaded_timing <- NULL
  mock_models <- list(
    win = structure(list(), class = "mock_model"),
    podium = structure(list(), class = "mock_model"),
    t10 = structure(list(), class = "mock_model"),
    position = structure(list(), class = "mock_model"),
    position_class = structure(list(), class = "mock_model")
  )

  local_mocked_bindings(
    load_models = function(model_type, model_timing, engine) {
      loaded_timing <<- model_timing
      mock_models
    },
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(1, 2))
    },
    predict_position_class = function(new_data, position_class_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(1, 2),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )

  expect_message(
    result <- predict_round(
      new_data,
      results_models = NULL,
      engine = "ensemble"
    ),
    "Loading 'after_quali'"
  )

  expect_identical(loaded_timing, "after_quali")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() auto-detects late timing from practice columns (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026),
    practice_best_rank = c(3, 7)
  )
  loaded_timing <- NULL
  mock_models <- list(
    win = structure(list(), class = "mock_model"),
    podium = structure(list(), class = "mock_model"),
    t10 = structure(list(), class = "mock_model"),
    position = structure(list(), class = "mock_model"),
    position_class = structure(list(), class = "mock_model")
  )

  local_mocked_bindings(
    load_models = function(model_type, model_timing, engine) {
      loaded_timing <<- model_timing
      mock_models
    },
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(1, 2))
    },
    predict_position_class = function(new_data, position_class_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(1, 2),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )

  expect_message(
    result <- predict_round(
      new_data,
      results_models = NULL,
      engine = "ensemble"
    ),
    "Loading 'late'"
  )

  expect_identical(loaded_timing, "late")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() auto-detects early timing when no late indicators exist (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026)
  )
  loaded_timing <- NULL
  mock_models <- list(
    win = structure(list(), class = "mock_model"),
    podium = structure(list(), class = "mock_model"),
    t10 = structure(list(), class = "mock_model"),
    position = structure(list(), class = "mock_model"),
    position_class = structure(list(), class = "mock_model")
  )

  local_mocked_bindings(
    load_models = function(model_type, model_timing, engine) {
      loaded_timing <<- model_timing
      mock_models
    },
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(1, 2))
    },
    predict_position_class = function(new_data, position_class_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(1, 2),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )

  expect_message(
    result <- predict_round(
      new_data,
      results_models = NULL,
      engine = "ensemble"
    ),
    "Loading 'early'"
  )

  expect_identical(loaded_timing, "early")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() loads explicit timing strings before prediction (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026)
  )
  loaded_timing <- NULL
  mock_models <- list(
    win = structure(list(), class = "mock_model"),
    podium = structure(list(), class = "mock_model"),
    t10 = structure(list(), class = "mock_model"),
    position = structure(list(), class = "mock_model"),
    position_class = structure(list(), class = "mock_model")
  )

  local_mocked_bindings(
    load_models = function(model_type, model_timing, engine) {
      loaded_timing <<- model_timing
      mock_models
    },
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(1, 2))
    },
    predict_position_class = function(new_data, position_class_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(1, 2),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )

  expect_message(
    result <- predict_round(
      new_data,
      results_models = "late",
      engine = "ensemble"
    ),
    "Loading 'late'"
  )

  expect_identical(loaded_timing, "late")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})

test_that("predict_round() adds ensemble features for model_stack position_class (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026)
  )
  mock_models <- list(
    win = structure(list(id = "win"), class = "mock_model"),
    podium = structure(list(id = "podium"), class = "mock_model"),
    t10 = structure(list(id = "t10"), class = "mock_model"),
    position = structure(list(id = "position"), class = "mock_model"),
    position_class = structure(
      list(id = "position_class"),
      class = c("model_stack", "list")
    )
  )

  local_mocked_bindings(
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(4, 7))
    },
    predict_position_class = function(new_data, position_class_model) {
      expect_true(all(
        c("ensemble_win_pred", "ensemble_pos_pred") %in% names(new_data)
      ))
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(4, 7),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )
  local_mocked_bindings(
    predict = function(object, newdata, type, ...) {
      if (identical(object$id, "win") && identical(type, "prob")) {
        return(tibble::tibble(.pred_1 = c(0.61, 0.39)))
      }
      if (identical(object$id, "position") && identical(type, "numeric")) {
        return(tibble::tibble(.pred = c(4, 7)))
      }
      stop("unexpected stats::predict() call")
    },
    .package = "stats"
  )

  expect_message(
    result <- predict_round(new_data, results_models = mock_models),
    "Adding ensemble predictions"
  )

  expect_s3_class(result, "tbl_df")
  expect_in(
    c(
      "win_odd",
      "podium_odd",
      "t10_odd",
      "likely_position",
      "likely_position_class"
    ),
    names(result)
  )
})

test_that("predict_round() leaves new_data unchanged for non-ensemble class models (#noissue)", {
  new_data <- tibble::tibble(
    driver_id = c("driver_a", "driver_b"),
    round = c(1L, 1L),
    season = c(2026, 2026)
  )
  mock_models <- list(
    win = structure(list(), class = "mock_model"),
    podium = structure(list(), class = "mock_model"),
    t10 = structure(list(), class = "mock_model"),
    position = structure(list(), class = "mock_model"),
    position_class = structure(list(), class = "mock_model")
  )

  local_mocked_bindings(
    predict_winner = function(new_data, win_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(win_odd = c(0.6, 0.4))
    },
    predict_podium = function(new_data, podium_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(podium_odd = c(0.8, 0.7))
    },
    predict_t10 = function(new_data, t10_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(t10_odd = c(0.9, 0.85))
    },
    predict_position = function(new_data, position_model) {
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(likely_position = c(4, 7))
    },
    predict_position_class = function(new_data, position_class_model) {
      expect_false(any(
        c("ensemble_win_pred", "ensemble_pos_pred") %in% names(new_data)
      ))
      new_data |>
        dplyr::select("driver_id", "round", "season") |>
        dplyr::mutate(
          likely_position_class = c(4, 7),
          .probs = I(list(diag(2), diag(2)))
        )
    },
    .package = "f1predicter"
  )

  result <- predict_round(new_data, results_models = mock_models)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(new_data))
})
