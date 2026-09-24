test_that("train_quali_position_ensemble() trains only position models, not quali_pole (#noissue)", {
  captured <- list()

  local_mocked_bindings(
    train_stacked_model = function(
      outcome_var,
      model_name,
      train_data,
      data_split,
      data_folds,
      predictor_vars,
      hyperparams,
      model_mode,
      save_model = TRUE,
      quiet = FALSE
    ) {
      captured$stacked[[outcome_var]] <<- list(
        model_name = model_name,
        model_mode = model_mode,
        predictor_vars = predictor_vars
      )
      structure(list(payload = outcome_var), class = "model_stack")
    },
    train_ordinal_ensemble = function(
      outcome_var,
      model_name,
      train_data,
      data_split,
      data_folds,
      predictor_vars,
      hyperparams,
      save_model = TRUE,
      model_timing = NULL
    ) {
      captured$ordinal[[outcome_var]] <<- list(model_name = model_name)
      structure(
        list(payload = paste0(outcome_var, "_class")),
        class = "model_stack"
      )
    },
    .package = "f1predicter"
  )

  data <- tibble::tibble(
    season = rep(2023L, 20),
    round = rep(1:4, each = 5),
    round_id = as.character(rep(1:4, each = 5)),
    driver_id = paste0("d", seq_len(20)),
    constructor_id = paste0("c", rep(1:2, 10)),
    quali_position = rep(1:5, 4),
    driver_experience = rnorm(20),
    driver_failure_avg = rnorm(20),
    constructor_grid_avg = rnorm(20),
    constructor_finish_avg = rnorm(20),
    constructor_failure_avg = rnorm(20),
    driver_grid_avg = rnorm(20),
    driver_position_avg = rnorm(20),
    driver_finish_avg = rnorm(20),
    driver_failure_circuit_avg = rnorm(20),
    driver_avg_qgap = rnorm(20),
    constructor_failure_circuit_avg = rnorm(20)
  )

  result <- train_quali_position_ensemble(
    data,
    use_practice_data = FALSE,
    seed = 101
  )

  expect_named(result, c("quali_pos", "quali_pos_class"))
  expect_s3_class(result$quali_pos, "model_stack")
  expect_s3_class(result$quali_pos_class, "model_stack")

  # Only the position outcome was trained via train_stacked_model(); the
  # "pole" binary classifier is never invoked.
  expect_named(captured$stacked, "quali_position")
  expect_identical(captured$stacked$quali_position$model_mode, "regression")
  expect_false("pole" %in% names(captured$stacked))
  expect_named(captured$ordinal, "quali_position")
})

test_that("train_results_position_ensemble() trains only position models, not win/podium/t10 (#noissue)", {
  captured <- list()

  local_mocked_bindings(
    train_stacked_model = function(
      outcome_var,
      model_name,
      train_data,
      data_split,
      data_folds,
      predictor_vars,
      hyperparams,
      model_mode,
      save_model = TRUE,
      quiet = FALSE
    ) {
      captured$stacked[[outcome_var]] <<- list(
        model_name = model_name,
        model_mode = model_mode
      )
      structure(list(payload = outcome_var), class = "model_stack")
    },
    train_ordinal_ensemble = function(
      outcome_var,
      model_name,
      train_data,
      data_split,
      data_folds,
      predictor_vars,
      hyperparams,
      save_model = TRUE,
      model_timing = NULL
    ) {
      captured$ordinal[[outcome_var]] <<- list(model_name = model_name)
      structure(
        list(payload = paste0(outcome_var, "_class")),
        class = "model_stack"
      )
    },
    .package = "f1predicter"
  )

  data <- tibble::tibble(
    season = rep(2023L, 20),
    round = rep(1:4, each = 5),
    round_id = as.character(rep(1:4, each = 5)),
    driver_id = paste0("d", seq_len(20)),
    constructor_id = paste0("c", rep(1:2, 10)),
    grid = rep(1:5, 4),
    quali_position = rep(1:5, 4),
    position = rep(1:5, 4),
    driver_experience = rnorm(20),
    driver_failure_avg = rnorm(20),
    constructor_grid_avg = rnorm(20),
    constructor_finish_avg = rnorm(20),
    constructor_failure_avg = rnorm(20),
    driver_grid_avg = rnorm(20),
    driver_position_avg = rnorm(20),
    driver_finish_avg = rnorm(20),
    grid_pos_corr_avg = rnorm(20),
    driver_avg_qgap = rnorm(20),
    driver_failure_circuit_avg = rnorm(20),
    constructor_failure_circuit_avg = rnorm(20)
  )

  result <- train_results_position_ensemble(
    data,
    scenario = "early",
    seed = 202
  )

  expect_named(result, c("position", "position_class"))
  expect_s3_class(result$position, "model_stack")
  expect_s3_class(result$position_class, "model_stack")

  expect_named(captured$stacked, "position")
  expect_identical(captured$stacked$position$model_mode, "regression")
  expect_false(any(c("win", "podium", "t10") %in% names(captured$stacked)))
  expect_named(captured$ordinal, "position")
})

test_that("train_quali_position_ensemble() and train_results_position_ensemble() require stacks (#noissue)", {
  local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) {
      if (identical(package, "stacks")) {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )

  expect_error(
    train_quali_position_ensemble(
      data = tibble::tibble(season = 2024L, quali_position = 1L)
    ),
    "must be installed"
  )
  expect_error(
    train_results_position_ensemble(
      data = tibble::tibble(season = 2024L, position = 1L, round_id = "1"),
      scenario = "early"
    ),
    "must be installed"
  )
})

test_that("save_position_models() writes to a distinct 'ensemble_light' engine path (#noissue)", {
  model_dir <- withr::local_tempdir()
  withr::local_options(list(f1predicter.models = model_dir))

  local_mocked_bindings(
    butcher_model_list = function(model_list) model_list,
    .package = "f1predicter"
  )

  fake_models <- list(
    quali_pos = structure(list(payload = "pos"), class = "model_stack"),
    quali_pos_class = structure(
      list(payload = "pos_class"),
      class = "model_stack"
    )
  )

  result <- save_position_models(
    fake_models,
    model_type = "quali",
    model_timing = "early"
  )

  expected_path <- file.path(model_dir, "quali_early_ensemble_light_models.rds")
  expect_true(file.exists(expected_path))
  expect_named(result, c("quali_pos", "quali_pos_class"))

  # The file must not collide with the full-suite ensemble path.
  full_suite_path <- file.path(model_dir, "quali_early_ensemble_models.rds")
  expect_false(file.exists(full_suite_path))

  loaded <- load_models("quali", "early", engine = "ensemble_light")
  expect_identical(loaded$quali_pos$payload, "pos")
})

test_that("save_position_models() warns and skips writing when the model list is empty (#noissue)", {
  model_dir <- withr::local_tempdir()
  withr::local_options(list(f1predicter.models = model_dir))

  local_mocked_bindings(
    butcher_model_list = function(model_list) model_list,
    .package = "f1predicter"
  )

  expect_warning(
    save_position_models(list(), model_type = "results", model_timing = "late"),
    "No valid models"
  )
  expect_false(
    file.exists(file.path(model_dir, "results_late_ensemble_light_models.rds"))
  )
})

test_that("model_quali_position_early()/late() wrap the light trainer and save under 'ensemble_light' (#noissue)", {
  seen <- list()

  local_mocked_bindings(
    train_quali_position_ensemble = function(
      data,
      use_practice_data,
      seed = NULL
    ) {
      seen$use_practice_data <<- use_practice_data
      seen$seed <<- seed
      list(quali_pos = "pos", quali_pos_class = "pos_class")
    },
    save_position_models = function(model_list, model_type, model_timing) {
      seen$model_type <<- model_type
      seen$model_timing <<- model_timing
      invisible(model_list)
    },
    .package = "f1predicter"
  )

  model_quali_position_early(data = NULL, seed = 11)
  expect_false(seen$use_practice_data)
  expect_identical(seen$seed, 11)
  expect_identical(seen$model_type, "quali")
  expect_identical(seen$model_timing, "early")

  model_quali_position_late(data = NULL, seed = 22)
  expect_true(seen$use_practice_data)
  expect_identical(seen$seed, 22)
  expect_identical(seen$model_timing, "late")
})

test_that("model_results_position_*() wrap the light trainer and save under 'ensemble_light' (#noissue)", {
  seen <- list()

  local_mocked_bindings(
    train_results_position_ensemble = function(data, scenario, seed = NULL) {
      seen$scenario <<- scenario
      seen$seed <<- seed
      list(position = "pos", position_class = "pos_class")
    },
    save_position_models = function(model_list, model_type, model_timing) {
      seen$model_type <<- model_type
      seen$model_timing <<- model_timing
      invisible(model_list)
    },
    .package = "f1predicter"
  )

  model_results_position_early(data = NULL, seed = 33)
  expect_identical(seen$scenario, "early")
  expect_identical(seen$model_timing, "early")

  model_results_position_late(data = NULL, seed = 44)
  expect_identical(seen$scenario, "late")
  expect_identical(seen$model_timing, "late")

  model_results_position_after_quali(data = NULL, seed = 55)
  expect_identical(seen$scenario, "after_quali")
  expect_identical(seen$model_timing, "after_quali")
  expect_identical(seen$model_type, "results")
})

test_that("model_*_position_*() do not save when save_model = FALSE (#noissue)", {
  save_called <- FALSE

  local_mocked_bindings(
    train_quali_position_ensemble = function(
      data,
      use_practice_data,
      seed = NULL
    ) {
      list(quali_pos = "pos")
    },
    train_results_position_ensemble = function(data, scenario, seed = NULL) {
      list(position = "pos")
    },
    save_position_models = function(model_list, model_type, model_timing) {
      save_called <<- TRUE
      invisible(model_list)
    },
    .package = "f1predicter"
  )

  model_quali_position_early(data = NULL, save_model = FALSE)
  expect_false(save_called)

  model_results_position_after_quali(data = NULL, save_model = FALSE)
  expect_false(save_called)
})
