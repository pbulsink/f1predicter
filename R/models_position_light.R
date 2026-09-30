# --------------------- Lightweight Position Ensembles ---------------------
#
# `model_quali_early()`/`model_results_early()` (and their "late"/
# "after_quali" siblings) train a full suite of models (pole/win/podium/t10
# plus position) whenever `engine = "ensemble"`. That's expensive when all
# `simulate_race()`/`simulate_quali()` actually consume is the `position`
# (and, optionally, `position_class`) model (see R/simulate_race.R).
#
# The functions below train *only* the regression (`quali_pos`/`position`)
# and ordinal (`quali_pos_class`/`position_class`) ensembles for a given
# timing, skipping pole/win/podium/t10 entirely. They are saved under a
# distinct `"ensemble_light"` engine tag so they never collide with files
# produced by the full-suite `model_*_*(engine = "ensemble")` functions.

#' Train a Lightweight Qualifying Position Ensemble
#'
#' Internal helper that trains only the regression (`quali_pos`) and ordinal
#' (`quali_pos_class`) ensembles for qualifying position, skipping the
#' `quali_pole` classifier trained by `train_quali_models()`.
#'
#' @inheritParams train_quali_models
#' @return A list containing `quali_pos` and `quali_pos_class`, each a fitted
#'   `model_stack` object.
#' @noRd
train_quali_position_ensemble <- function(
  data,
  use_practice_data = FALSE,
  seed = NULL
) {
  check_seed(seed)
  if (!requireNamespace("stacks", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg stacks} must be installed to use ensemble models."
    )
  }

  model_timing <- ifelse(use_practice_data, "late", "early")
  cli::cli_h1("Training Lightweight Qualifying Position Ensemble")
  cli::cli_inform("Scenario: {.val {model_timing}}")

  data <- data[data$season >= 2018, ]

  base_cols <- c(
    "quali_position",
    "driver_experience",
    "driver_failure_avg",
    "constructor_grid_avg",
    "constructor_finish_avg",
    "constructor_failure_avg",
    "driver_grid_avg",
    "driver_position_avg",
    "driver_finish_avg",
    "driver_failure_circuit_avg",
    "driver_avg_qgap",
    "constructor_failure_circuit_avg",
    "season",
    "round",
    "round_id",
    "driver_id",
    "constructor_id"
  )
  practice_cols <- c(
    "driver_practice_optimal_rank_avg",
    "practice_avg_rank",
    "practice_best_rank",
    "practice_optimal_rank"
  )
  id_cols <- c("season", "round", "round_id", "driver_id", "constructor_id")

  pos_cols <- if (use_practice_data) c(base_cols, practice_cols) else base_cols

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  test_groups <- select_test_groups(data)

  pos_data <- data |>
    dplyr::filter(!is.na(.data$quali_position)) |>
    dplyr::select(dplyr::all_of(pos_cols))

  pos_splits <- prepare_and_split_data(pos_data, test_groups = test_groups)
  predictor_vars <- setdiff(pos_cols, c("quali_position", id_cols))

  all_hyperparams <- get_hyperparameters("quali", model_timing)

  position_final_fit <- train_stacked_model(
    outcome_var = "quali_position",
    model_name = paste(
      "Quali Position (Light)",
      tools::toTitleCase(model_timing)
    ),
    train_data = pos_splits$train_data,
    data_split = pos_splits$data_split,
    data_folds = pos_splits$data_folds,
    predictor_vars = predictor_vars,
    hyperparams = all_hyperparams$position_hyperparameters,
    model_mode = "regression",
    save_model = FALSE
  )

  cli::cli_rule("Training Qualifying Position Model (Ordinal, Light)")

  pos_class_data <- pos_data |>
    dplyr::mutate(
      quali_position = cap_ordinal_position(.data$quali_position)
    ) |>
    dplyr::arrange(.data$season, .data$round, .data$quali_position)

  pos_class_splits <- prepare_and_split_data(
    pos_class_data,
    test_groups = test_groups
  )

  position_class_final_fit <- train_ordinal_ensemble(
    outcome_var = "quali_position",
    model_name = paste(
      "Quali Position Class (Light)",
      tools::toTitleCase(model_timing)
    ),
    train_data = pos_class_splits$train_data,
    data_split = pos_class_splits$data_split,
    data_folds = pos_class_splits$data_folds,
    predictor_vars = predictor_vars,
    hyperparams = all_hyperparams$ordinal_class_hyperparameters,
    save_model = FALSE
  )

  list(
    quali_pos = position_final_fit,
    quali_pos_class = position_class_final_fit
  )
}

#' Train a Lightweight Race Position Ensemble
#'
#' Internal helper that trains only the regression (`position`) and ordinal
#' (`position_class`) ensembles for race finishing position, skipping the
#' `win`/`podium`/`t10` classifiers trained by `train_results_models()`.
#'
#' @inheritParams train_results_models
#' @return A list containing `position` and `position_class`, each a fitted
#'   `model_stack` object.
#' @noRd
train_results_position_ensemble <- function(
  data,
  scenario,
  seed = NULL
) {
  check_seed(seed)
  if (!requireNamespace("stacks", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg stacks} must be installed to use ensemble models."
    )
  }
  scenario <- rlang::arg_match(scenario, c("early", "late", "after_quali"))

  cli::cli_h1("Training Lightweight Race Position Ensemble")
  cli::cli_inform("Scenario: {.val {scenario}}")

  data <- data[data$season >= 2018, ]
  data$round_id <- as.factor(data$round_id)

  base_cols <- c(
    "driver_id",
    "constructor_id",
    "grid",
    "quali_position",
    "driver_experience",
    "driver_failure_avg",
    "constructor_grid_avg",
    "constructor_finish_avg",
    "constructor_failure_avg",
    "driver_grid_avg",
    "driver_position_avg",
    "driver_finish_avg",
    "grid_pos_corr_avg",
    "driver_avg_qgap",
    "driver_failure_circuit_avg",
    "constructor_failure_circuit_avg",
    "season",
    "round",
    "round_id"
  )
  practice_cols <- c(
    "driver_practice_optimal_rank_avg",
    "practice_avg_rank",
    "practice_best_rank",
    "practice_optimal_rank",
    "practice_avg_gap",
    "practice_best_gap"
  )
  quali_perf_cols <- c("q_min_perc", "q_avg_perc")
  id_cols <- c("season", "round", "round_id", "driver_id", "constructor_id")

  pos_cols <- switch(
    scenario,
    "early" = c(base_cols, "position"),
    "late" = c(base_cols, practice_cols, "position"),
    "after_quali" = c(base_cols, practice_cols, quali_perf_cols, "position")
  )

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  test_groups <- select_test_groups(data)

  pos_data <- data |>
    dplyr::select(dplyr::all_of(pos_cols))

  pos_splits <- prepare_and_split_data(pos_data, test_groups = test_groups)
  predictor_vars <- setdiff(pos_cols, c("position", id_cols))

  all_hyperparams <- get_hyperparameters("results", scenario)

  position_final_fit <- train_stacked_model(
    outcome_var = "position",
    model_name = paste("Position (Light)", tools::toTitleCase(scenario)),
    train_data = pos_splits$train_data,
    data_split = pos_splits$data_split,
    data_folds = pos_splits$data_folds,
    predictor_vars = predictor_vars,
    hyperparams = all_hyperparams$position_hyperparameters,
    model_mode = "regression",
    save_model = FALSE
  )

  cli::cli_rule("Training Position Model (Ordinal, Light)")

  pos_class_data <- pos_data |>
    dplyr::mutate(position = cap_ordinal_position(.data$position))

  pos_class_splits <- prepare_and_split_data(
    pos_class_data,
    test_groups = test_groups
  )

  position_class_final_fit <- train_ordinal_ensemble(
    outcome_var = "position",
    model_name = paste(
      "Position Class (Light)",
      tools::toTitleCase(scenario)
    ),
    train_data = pos_class_splits$train_data,
    data_split = pos_class_splits$data_split,
    data_folds = pos_class_splits$data_folds,
    predictor_vars = predictor_vars,
    hyperparams = all_hyperparams$ordinal_class_hyperparameters,
    save_model = FALSE
  )

  list(
    position = position_final_fit,
    position_class = position_class_final_fit
  )
}

#' Save Lightweight Position Models
#'
#' Internal helper mirroring `save_models()`, but writing to a file tagged
#' with the `"ensemble_light"` engine so lightweight position-only model
#' lists never collide with files produced by the full-suite
#' `model_*_*(engine = "ensemble")` functions (whose names, for "results"
#' models, may not include `win`/`podium`/`t10` and so cannot be
#' disambiguated by `save_models()`'s name-based inference).
#'
#' @param model_list A named list containing `quali_pos`/`quali_pos_class` or
#'   `position`/`position_class`.
#' @param model_type Either `"quali"` or `"results"`.
#' @param model_timing One of `"early"`, `"late"`, or (for `"results"` only)
#'   `"after_quali"`.
#' @return Invisibly returns the (butchered) `model_list`.
#' @noRd
save_position_models <- function(model_list, model_type, model_timing) {
  file_path <- construct_model_path(
    model_type = model_type,
    model_timing = model_timing,
    engine = "ensemble_light"
  )
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  final_list <- butcher_model_list(model_list)

  cli::cli_inform("Writing lightweight position {.param model_list} to file.")

  if (length(final_list) > 0) {
    saveRDS(final_list, file = file_path)
    cli::cli_inform(
      "Models successfully butchered and saved to {.path {file_path}}."
    )
  } else {
    cli::cli_warn("No valid models were found to save.")
  }

  invisible(final_list)
}

#' Train Lightweight Early Qualifying Position Models
#'
#' @description
#' Trains only the ensemble regression (`quali_pos`) and ordinal
#' (`quali_pos_class`) models for qualifying position, using data available
#' before any practice sessions have occurred. Unlike
#' `model_quali_early(engine = "ensemble")`, this skips the `quali_pole`
#' classifier entirely, making it much cheaper to (re)train when only the
#' position models are needed by `simulate_quali()`.
#'
#' @param data A data frame containing the modeling data. Defaults to the
#'   output of `clean_data()`.
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are butchered and saved to `options('f1predicter.models')`, tagged with
#'   the `"ensemble_light"` engine (see `save_position_models()`).
#' @param seed Optional single integer. If provided, `set.seed(seed)` is
#'   called once before the shared train/test split is created, making the
#'   run reproducible. If `NULL` (the default), no seed is set.
#' @return A list containing fitted `model_stack` objects: `quali_pos` and
#'   `quali_pos_class`.
#' @export
#' @examples
#' \dontrun{
#' models <- model_quali_position_early()
#' }
model_quali_position_early <- function(
  data = clean_data(),
  save_model = TRUE,
  seed = NULL
) {
  models <- train_quali_position_ensemble(
    data,
    use_practice_data = FALSE,
    seed = seed
  )
  if (save_model) {
    tryCatch(
      save_position_models(
        models,
        model_type = "quali",
        model_timing = "early"
      ),
      error = function(e) paste0("Error saving models: ", e)
    )
  }
  invisible(models)
}

#' Train Lightweight Late Qualifying Position Models
#'
#' @description
#' Trains only the ensemble regression (`quali_pos`) and ordinal
#' (`quali_pos_class`) models for qualifying position, using data available
#' *after* all practice sessions have occurred. Unlike
#' `model_quali_late(engine = "ensemble")`, this skips the `quali_pole`
#' classifier entirely, making it much cheaper to (re)train when only the
#' position models are needed by `simulate_quali()`.
#'
#' @inherit model_quali_position_early params return
#' @export
#' @examples
#' \dontrun{
#' models <- model_quali_position_late()
#' }
model_quali_position_late <- function(
  data = clean_data(),
  save_model = TRUE,
  seed = NULL
) {
  models <- train_quali_position_ensemble(
    data,
    use_practice_data = TRUE,
    seed = seed
  )
  if (save_model) {
    tryCatch(
      save_position_models(models, model_type = "quali", model_timing = "late"),
      error = function(e) paste0("Error saving models: ", e)
    )
  }
  invisible(models)
}

#' Train Lightweight Pre-Practice Race Position Models
#'
#' @description
#' Trains only the ensemble regression (`position`) and ordinal
#' (`position_class`) models for race finishing position, using data
#' available before any practice or qualifying sessions have occurred.
#' Unlike `model_results_early(engine = "ensemble")`, this skips the
#' `win`/`podium`/`t10` classifiers entirely, making it much cheaper to
#' (re)train when only the position models are needed by `simulate_race()`.
#'
#' @param data A data frame containing the modeling data. Defaults to the
#'   output of `clean_data()`.
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are butchered and saved to `options('f1predicter.models')`, tagged with
#'   the `"ensemble_light"` engine (see `save_position_models()`).
#' @param seed Optional single integer. If provided, `set.seed(seed)` is
#'   called once before the shared train/test split is created, making the
#'   run reproducible. If `NULL` (the default), no seed is set.
#' @return A list containing fitted `model_stack` objects: `position` and
#'   `position_class`.
#' @export
#' @examples
#' \dontrun{
#' models <- model_results_position_early()
#' }
model_results_position_early <- function(
  data = clean_data(),
  save_model = TRUE,
  seed = NULL
) {
  models <- train_results_position_ensemble(
    data,
    scenario = "early",
    seed = seed
  )
  if (save_model) {
    tryCatch(
      save_position_models(
        models,
        model_type = "results",
        model_timing = "early"
      ),
      error = function(e) paste0("Error saving models: ", e)
    )
  }
  invisible(models)
}

#' Train Lightweight Post-Practice Race Position Models
#'
#' @description
#' Trains only the ensemble regression (`position`) and ordinal
#' (`position_class`) models for race finishing position, using data
#' available after practice sessions but before qualifying. Unlike
#' `model_results_late(engine = "ensemble")`, this skips the
#' `win`/`podium`/`t10` classifiers entirely, making it much cheaper to
#' (re)train when only the position models are needed by `simulate_race()`.
#'
#' @inherit model_results_position_early params return
#' @export
#' @examples
#' \dontrun{
#' models <- model_results_position_late()
#' }
model_results_position_late <- function(
  data = clean_data(),
  save_model = TRUE,
  seed = NULL
) {
  models <- train_results_position_ensemble(
    data,
    scenario = "late",
    seed = seed
  )
  if (save_model) {
    tryCatch(
      save_position_models(
        models,
        model_type = "results",
        model_timing = "late"
      ),
      error = function(e) paste0("Error saving models: ", e)
    )
  }
  invisible(models)
}

#' Train Lightweight Post-Qualifying Race Position Models
#'
#' @description
#' Trains only the ensemble regression (`position`) and ordinal
#' (`position_class`) models for race finishing position, using data
#' available after qualifying has completed. Unlike
#' `model_results_after_quali(engine = "ensemble")`, this skips the
#' `win`/`podium`/`t10` classifiers entirely, making it much cheaper to
#' (re)train when only the position models are needed by `simulate_race()`.
#'
#' @inherit model_results_position_early params return
#' @export
#' @examples
#' \dontrun{
#' models <- model_results_position_after_quali()
#' }
model_results_position_after_quali <- function(
  data = clean_data(),
  save_model = TRUE,
  seed = NULL
) {
  models <- train_results_position_ensemble(
    data,
    scenario = "after_quali",
    seed = seed
  )
  if (save_model) {
    tryCatch(
      save_position_models(
        models,
        model_type = "results",
        model_timing = "after_quali"
      ),
      error = function(e) paste0("Error saving models: ", e)
    )
  }
  invisible(models)
}
