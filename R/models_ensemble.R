# --------------------------- Ensemble Models ----------------------------
# Register ordinal regression engines from the `ordered` package.
# This import is what triggers `ordered::.onLoad()`, which calls
# `parsnip::set_engine()` for polr/ordinalNet/ordinalForest/rpartScore.
# Without this, ordinal_reg() has no engines and training will fail.
#' @importFrom ordered predict_ordinalNet_wrapper
NULL

#' Train a Stacked Ensemble Model
#'
#' @description
#' This function trains multiple model types (e.g., ranger, glmnet, kknn) for a
#' single prediction task and combines them into a stacked ensemble model using
#' the `stacks` package. It uses pre-defined hyperparameters for each model,
#' bypassing the tuning step.
#'
#' @details
#' The function iterates through a provided list of `engines`. For each engine,
#' it defines a `parsnip` model specification, finalizes it with the provided
#' hyperparameters, and then fits the model to resamples using
#' `tune::fit_resamples()` with a special control setting
#' (`control_stack_resamples()`) that saves the out-of-fold predictions
#' necessary for stacking.
#'
#' After all candidate models are tuned, they are added to a `data_stack`.
#' The `blend_predictions()` function is called to find the optimal linear
#' combination of the member models. Finally, `fit_members()` fits the final
#' ensemble model.
#'
#' This approach often yields a model with better predictive performance than any
#' of the individual member models.
#'
#' @param outcome_var A character string specifying the name of the outcome variable.
#' @param model_name A character string for the model's display name (e.g., "Quali Position Ensemble").
#' @param train_data The training data frame.
#' @param data_split The data split object from `rsample`.
#' @param data_folds The cross-validation folds object.
#' @param predictor_vars A character vector of predictor variable names.
#' @param hyperparams A named list where each name is an engine and the value is
#'   a tibble or list of the optimal hyperparameters for that engine. This is
#'   required.
#' @param model_mode A character string, either `"regression"` or `"classification"`.
#' @param save_model A logical value. If `TRUE` (default), the trained ensemble model
#'   is automatically butchered and saved to the path specified in `options('f1predicter.models')`.
#' @return A fitted `model_stack` object, ready for prediction.
#' @export
#' @examples
#' \dontrun{
#' # This is a conceptual example.
#' # You would first need to prepare your data, splits, and folds.
#'
#' # optimal_params <- list(
#' #   ranger = tibble::tibble(mtry = 5, min_n = 10),
#' #   glmnet = tibble::tibble(penalty = 0.01, mixture = 0.5)
#' # )
#'
#' # data <- clean_data()
#' # ... (create splits, folds, predictor_vars)
#'
#' # quali_pos_ensemble <- train_stacked_model(
#' #   outcome_var = "quali_position",
#' #   model_name = "Quali Position Ensemble",
#' #   train_data = my_train_data,
#' #   data_split = my_data_split,
#' #   data_folds = my_data_folds,
#' #   predictor_vars = my_predictor_vars,
#' #   hyperparams = optimal_params,
#' #   model_mode = "regression"
#' # )
#'
#' # # Make predictions
#' # predictions <- stats::predict(quali_pos_ensemble, new_data = my_test_data)
#' }
train_stacked_model <- function(
  outcome_var,
  model_name,
  train_data,
  data_split,
  data_folds,
  predictor_vars,
  hyperparams,
  model_mode = "regression",
  save_model = TRUE
) {
  # Ensure stacks is installed
  if (missing(hyperparams)) {
    cli::cli_abort("{.arg hyperparams} must be provided.")
  }
  if (!requireNamespace("stacks", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg stacks} must be installed to use this function."
    )
  }

  engines <- names(hyperparams)

  cli::cli_h1("Training Stacked Ensemble: {model_name}")
  cli::cli_inform("Member engines: {.val {engines}}")

  # Define the control object to save predictions for stacking
  ctrl_stack <- stacks::control_stack_resamples()

  # Define the recipe once
  formula <- stats::reformulate(predictor_vars, response = outcome_var)
  base_recipe <- recipes::recipe(formula, data = train_data) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Define metrics based on model mode
  metrics <- if (model_mode == "regression") {
    yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)
  } else {
    yardstick::metric_set(
      yardstick::accuracy,
      yardstick::mn_log_loss,
      yardstick::roc_auc
    )
  }

  # Train and tune each candidate model
  candidate_resamples <- purrr::map(engines, function(engine) {
    cli::cli_rule("Training candidate: {.val {engine}}")

    # Get the hyperparameters for the current engine
    engine_params <- hyperparams[[engine]]
    if (is.null(engine_params)) {
      cli::cli_abort(
        "No hyperparameters provided for engine {.val {engine}} in the {.arg hyperparams} list."
      )
    }
    cli::cli_inform(
      "Using hyperparameters: {paste(names(engine_params), unlist(engine_params), sep = ' = ', collapse = ', ')}"
    )

    # Define model spec with tune() placeholders for each engine
    model_spec_tuned <- switch(
      engine,
      "ranger" = parsnip::rand_forest(
        trees = 1000,
        mtry = tune::tune(),
        min_n = tune::tune()
      ),
      "glmnet" = if (model_mode == "regression") {
        parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune())
      } else {
        parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune())
      },
      "kknn" = parsnip::nearest_neighbor(neighbors = tune::tune()),
      "nnet" = parsnip::mlp(
        hidden_units = tune::tune(),
        penalty = tune::tune(),
        epochs = tune::tune()
      ),
      "kernlab" = parsnip::svm_rbf(
        cost = tune::tune(),
        rbf_sigma = tune::tune()
      ),
      cli::cli_abort("Unsupported engine: {.val {engine}}")
    )

    model_spec <- model_spec_tuned %>%
      parsnip::set_mode(model_mode) %>%
      parsnip::set_engine(engine)

    wflow <- workflows::workflow(base_recipe, model_spec)

    # Finalize the workflow with the provided hyperparameters
    final_wflow <- tune::finalize_workflow(wflow, engine_params)

    # Fit to resamples to get out-of-fold predictions
    tictoc::tic(glue::glue("Fitted {engine} to resamples for stacking"))
    res <- tune::fit_resamples(
      final_wflow,
      resamples = data_folds,
      metrics = metrics,
      control = ctrl_stack # CRITICAL for stacking
    )
    tictoc::toc()

    return(res)
  })

  # Name the list of results for easier identification in the stack
  names(candidate_resamples) <- engines

  # Initialize the stack and add candidates
  cli::cli_rule("Building the Ensemble")
  cli::cli_inform("Initializing data stack...")
  model_stack <- stacks::stacks()

  for (i in seq_along(candidate_resamples)) {
    cli::cli_inform("Adding candidate: {.val {names(candidate_resamples)[i]}}")
    tryCatch(
      model_stack <- stacks::add_candidates(
        model_stack,
        candidate_resamples[[i]],
        name = names(candidate_resamples)[i]
      ),
      error = function(e) {
        cli::cli_warn(
          "Model stacking error: {e}. Continuing with one less model."
        )
      }
    )
  }

  cli::cli_inform("Stack members and their resampling performance:")
  print(model_stack)

  # Blend the predictions to find ensemble weights
  cli::cli_inform("Blending predictions to find ensemble weights...")
  tictoc::tic("Blended predictions")
  blended_ensemble <- stacks::blend_predictions(model_stack, penalty = 0.01)
  tictoc::toc()

  cli::cli_inform("Ensemble weights:")
  print(stacks::autoplot(blended_ensemble, type = "weights"))

  # Fit the final ensemble
  cli::cli_inform("Fitting final ensemble members on all training data...")
  tictoc::tic("Fitted final ensemble")
  final_ensemble <- stacks::fit_members(blended_ensemble)
  tictoc::toc()

  cli::cli_alert_success(
    "Stacked ensemble '{model_name}' trained successfully!"
  )

  return(final_ensemble)
}


#' Train a Stacked Ordinal Classification Ensemble
#'
#' @description
#' This function trains multiple ordinal model types for a single ordinal
#' classification task and combines them into a stacked ensemble using the
#' `stacks` package. It uses pre-defined hyperparameters for each model,
#' bypassing the tuning step.
#'
#' @details
#' The function iterates through a provided list of ordinal `engines`. For each
#' engine, it defines a `parsnip` model specification, finalizes it with the
#' provided hyperparameters, and then fits the model to resamples using
#' `tune::fit_resamples()` with a special control setting
#' (`control_stack_resamples()`) that saves the out-of-fold predictions
#' necessary for stacking.
#'
#' Supported ordinal engines (from the `ordered` package):
#' \itemize{
#'   \item `"polr"`: Proportional-odds ordered logistic regression via `MASS::polr()`.
#'   \item `"ordinalNet"`: Penalized elastic-net ordinal regression (tunable:
#'     `penalty`, `mixture`).
#'   \item `"ordinalForest"`: Latent-variable ordinal random forest (tunable:
#'     `mtry`, `min_n`; `trees` fixed at 500).
#'   \item `"rpartScore"`: Ordinal classification tree (tunable:
#'     `cost_complexity`, `tree_depth`).
#' }
#'
#' The models are evaluated using a metric set that includes **Log Loss**
#' (`yardstick::mn_log_loss()`), which is required by `stacks` for candidate
#' blending, as well as the **Ranked Probability Score** (RPS)
#' (`yardstick::ranked_prob_score()`), which is the primary metric for
#' post-hoc ordinal evaluation. RPS rewards calibrated probability distributions
#' and penalises predictions that are far from the true rank. Linear-weighted
#' Kappa and accuracy are collected as additional monitoring metrics.
#'
#' The outcome variable must be an **ordered factor** in `train_data`.
#'
#' @param outcome_var A character string specifying the name of the ordered
#'   factor outcome variable.
#' @param model_name A character string for the model's display name (e.g.,
#'   "Position Class Ensemble").
#' @param train_data The training data frame.
#' @param data_split The data split object from `rsample`.
#' @param data_folds The cross-validation folds object.
#' @param predictor_vars A character vector of predictor variable names.
#' @param hyperparams A named list where each name is an ordinal engine and the
#'   value is a tibble of optimal hyperparameters for that engine. Use an empty
#'   `tibble::tibble()` for engines with no tunable parameters (e.g., `"polr"`).
#' @param save_model A logical value. If `TRUE` (default), the trained ensemble
#'   model is automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @return A fitted `model_stack` object, ready for ordinal classification
#'   prediction via `stats::predict(model, new_data, type = "class")` or
#'   `stats::predict(model, new_data, type = "prob")`.
#' @export
#' @examples
#' \dontrun{
#' # Example with a minimal two-engine ordinal ensemble
#' # hyperparams <- list(
#' #   polr = tibble::tibble(),
#' #   ordinalNet = tibble::tibble(penalty = 0.01, mixture = 0.5)
#' # )
#' # ordinal_ensemble <- train_ordinal_ensemble(
#' #   outcome_var = "position",
#' #   model_name  = "Position Ordinal Ensemble",
#' #   train_data  = my_train_data,
#' #   data_split  = my_data_split,
#' #   data_folds  = my_data_folds,
#' #   predictor_vars = my_predictor_vars,
#' #   hyperparams = hyperparams,
#' #   save_model  = FALSE
#' # )
#' # stats::predict(ordinal_ensemble, new_data = my_test_data, type = "class")
#' }
train_ordinal_ensemble <- function(
  outcome_var,
  model_name,
  train_data,
  data_split,
  data_folds,
  predictor_vars,
  hyperparams,
  save_model = TRUE
) {
  if (missing(hyperparams)) {
    cli::cli_abort("{.arg hyperparams} must be provided.")
  }
  if (!requireNamespace("stacks", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg stacks} must be installed to use this function."
    )
  }

  engines <- names(hyperparams)

  cli::cli_h1("Training Ordinal Classification Ensemble: {model_name}")
  cli::cli_inform("Member engines: {.val {engines}}")

  # Load ordered to register the ordinal engines with parsnip
  requireNamespace("ordered", quietly = TRUE)

  # Define metrics for ordinal classification:
  # - mn_log_loss: required by stacks for blending (probability metric it knows about)
  # - ranked_prob_score (RPS): primary ordinal metric, penalises predictions far from
  #   the true rank; requires an ordered factor outcome (yardstick >= 1.4.0)
  # - kap with linear weighting: gives partial credit for close misses
  # - accuracy: exact-match baseline
  kap_linear <- purrr::partial(yardstick::kap, weighting = "linear")
  class(kap_linear) <- c("class_metric", "metric", "function")
  attr(kap_linear, "direction") <- "maximize"

  metrics_ordinal <- yardstick::metric_set(
    yardstick::mn_log_loss,
    yardstick::ranked_prob_score,
    kap_linear,
    yardstick::accuracy
  )

  # Control object required by stacks to save out-of-fold predictions
  ctrl_stack <- stacks::control_stack_resamples()

  # Build the recipe once: dummy-encode nominals, remove zero-variance, normalise
  formula <- stats::reformulate(predictor_vars, response = outcome_var)
  # Reset environment to base to avoid capturing large objects from the
  # calling frame, which significantly inflates model size on disk.
  rlang::f_env(formula) <- rlang::base_env()

  base_recipe <- recipes::recipe(formula, data = train_data) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Train each candidate ordinal model on the resamples
  candidate_resamples <- purrr::map(engines, function(engine) {
    cli::cli_rule("Training ordinal candidate: {.val {engine}}")

    engine_params <- hyperparams[[engine]]
    if (is.null(engine_params)) {
      cli::cli_abort(
        "No hyperparameters provided for engine {.val {engine}} in {.arg hyperparams}."
      )
    }
    if (nrow(engine_params) > 0) {
      cli::cli_inform(
        "Using hyperparameters: {paste(names(engine_params), unlist(engine_params), sep = ' = ', collapse = ', ')}"
      )
    }

    # Build model specification; trees for ordinalForest fixed at 500 for speed
    model_spec_tuned <- switch(
      engine,
      "polr" = parsnip::ordinal_reg(),
      "ordinalNet" = parsnip::ordinal_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ),
      "ordinalForest" = parsnip::rand_forest(
        trees = 500,
        mtry = tune::tune(),
        min_n = tune::tune()
      ),
      "rpartScore" = parsnip::decision_tree(
        cost_complexity = tune::tune(),
        tree_depth = tune::tune()
      ),
      cli::cli_abort("Unsupported ordinal engine: {.val {engine}}")
    )

    model_spec <- model_spec_tuned %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine(engine)

    wflow <- workflows::workflow(base_recipe, model_spec)

    # Finalize the workflow with the provided hyperparameters.
    # For engines with no tunable parameters (e.g., "polr"), engine_params will
    # be an empty tibble and finalize_workflow returns the workflow unchanged.
    final_wflow <- tune::finalize_workflow(wflow, engine_params)

    tictoc::tic(glue::glue("Fitted ordinal {engine} to resamples for stacking"))
    res <- tune::fit_resamples(
      final_wflow,
      resamples = data_folds,
      metrics = metrics_ordinal,
      control = ctrl_stack
    )
    tictoc::toc()

    return(res)
  })

  names(candidate_resamples) <- engines

  # Build the stacked ensemble from the candidate resamples
  cli::cli_rule("Building the Ordinal Ensemble")
  cli::cli_inform("Initializing ordinal data stack...")
  model_stack <- stacks::stacks()

  for (i in seq_along(candidate_resamples)) {
    cli::cli_inform(
      "Adding ordinal candidate: {.val {names(candidate_resamples)[i]}}"
    )
    tryCatch(
      model_stack <- stacks::add_candidates(
        model_stack,
        candidate_resamples[[i]],
        name = names(candidate_resamples)[i]
      ),
      error = function(e) {
        cli::cli_warn(
          "Ordinal stacking error for {names(candidate_resamples)[i]}: {e}. Continuing."
        )
      }
    )
  }

  cli::cli_inform("Ordinal stack members and their resampling performance:")
  print(model_stack)

  cli::cli_inform("Blending ordinal predictions to find ensemble weights...")
  tictoc::tic("Blended ordinal predictions")
  blended_ensemble <- stacks::blend_predictions(model_stack, penalty = 0.01)
  tictoc::toc()

  cli::cli_inform("Ordinal ensemble weights:")
  print(stacks::autoplot(blended_ensemble, type = "weights"))

  cli::cli_inform(
    "Fitting final ordinal ensemble members on all training data..."
  )
  tictoc::tic("Fitted final ordinal ensemble")
  final_ensemble <- stacks::fit_members(blended_ensemble)
  tictoc::toc()

  cli::cli_alert_success(
    "Ordinal ensemble '{model_name}' trained successfully!"
  )

  return(final_ensemble)
}


#' Get Pre-tuned Model Hyperparameters
#'
#' @description
#' Retrieves a list of pre-tuned hyperparameters for various model types and
#' prediction scenarios. These hyperparameters were determined through a tuning
#' process in September 2025 and are hard-coded into this function for
#' convenience and reproducibility.
#'
#' This function is a helper to avoid re-running computationally expensive
#' tuning grids when training ensemble models.
#'
#' @param model A character string specifying the type of prediction model.
#'   Valid options are:
#'   \itemize{
#'     \item `"quali"`: For qualifying prediction models (pole position and final position).
#'     \item `"results"`: For race result prediction models (win, podium, top 10, and final position).
#'   }
#' @param timing A character string indicating the timing of the prediction
#'   within a race weekend. This determines which set of features were
#'   available for tuning. Valid options depend on `model`:
#'   \itemize{
#'     \item If `model = 'quali'`: `"early"` or `"late"`.
#'     \item If `model = 'results'`: `"early"`, `"late"`, or `"after-quali"`.
#'   }
#'
#' @return A named list. Each name corresponds to a specific prediction task
#'   (e.g., `pole_hyperparameters`, `position_hyperparameters`), and each value
#'   is another named list where keys are the `parsnip` engine (e.g., 'glmnet',
#'   'ranger') and values are the corresponding optimal hyperparameters.
#' @noRd
get_hyperparameters <- function(model = 'quali', timing = 'early') {
  # Default ordinal classification hyperparameters, shared across all scenarios.
  # polr has no tunable hyperparameters; an empty tibble triggers no-op finalization.
  # ordinalNet: elastic net (penalty = L2 strength, mixture = L1/L2 blend).
  # ordinalForest: random forest with trees fixed at 500 for speed.
  # rpartScore: ordinal scoring tree (moderate cost_complexity, moderate depth).
  ordinal_defaults <- list(
    'polr' = tibble::tibble(),
    'ordinalNet' = tibble::tibble(penalty = 0.01, mixture = 0.5),
    'ordinalForest' = tibble::tibble(mtry = 3L, min_n = 11L),
    'rpartScore' = tibble::tibble(cost_complexity = 0.01, tree_depth = 5L)
  )

  if (model == 'quali') {
    if (timing == 'early') {
      return(
        list(
          pole_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.75),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 10, penalty = 1, epochs = 1000),
            'ranger' = list(mtry = 1, min_n = 30), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 4, penalty = 1, epochs = 1000),
            'ranger' = list(mtry = 3, min_n = 30), # trees = 1000
            'kernlab' = list(cost = 1, rbf_sigma = 0.000464158883361278)
          ),
          ordinal_class_hyperparameters = ordinal_defaults
        )
      )
    } else if (timing == 'late') {
      return(
        list(
          pole_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 10, penalty = 1, epochs = 1000),
            'ranger' = list(mtry = 1, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.50),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 340),
            'ranger' = list(mtry = 4, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          ordinal_class_hyperparameters = ordinal_defaults
        )
      )
    } else {
      cli::cli_abort(
        "Error in f1predicter:::get_hyperparameters: {.param timing} must be {.val early} or {.val late}."
      )
    }
  } else if (model == 'results') {
    if (timing == 'early') {
      return(
        list(
          win_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 0.000464158883361278,
              epochs = 670
            ),
            'ranger' = list(mtry = 4, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 1, rbf_sigma = 0.000464158883361278)
          ),
          podium_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 0.000464158883361278,
              epochs = 340
            ),
            'ranger' = list(mtry = 1, min_n = 30), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          t10_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 0.5),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 2.15443469003189e-07,
              epochs = 670
            ),
            'ranger' = list(mtry = 1, min_n = 30), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 340),
            'ranger' = list(mtry = 4, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          ordinal_class_hyperparameters = ordinal_defaults
        )
      )
    } else if (timing == 'late') {
      return(
        list(
          win_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 4,
              penalty = 2.15443469003189e-07,
              epochs = 340
            ),
            'ranger' = list(mtry = 5, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 1, rbf_sigma = 0.000464158883361278)
          ),
          podium_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 0.75),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1e-10, epochs = 1000),
            'ranger' = list(mtry = 5, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          t10_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 0.000464158883361278,
              epochs = 1000
            ),
            'ranger' = list(mtry = 1, min_n = 11), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 340),
            'ranger' = list(mtry = 5, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          ordinal_class_hyperparameters = ordinal_defaults
        )
      )
    } else if (timing == 'after-quali') {
      return(
        list(
          win_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 10, penalty = 1, epochs = 1000),
            'ranger' = list(mtry = 6, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 1, rbf_sigma = 0.000464158883361278)
          ),
          podium_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 0.5),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 2.15443469003189e-07,
              epochs = 1000
            ),
            'ranger' = list(mtry = 1, min_n = 11), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          t10_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(
              hidden_units = 1,
              penalty = 2.15443469003189e-07,
              epochs = 340
            ),
            'ranger' = list(mtry = 1, min_n = 2), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 670),
            'ranger' = list(mtry = 6, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          ),
          ordinal_class_hyperparameters = ordinal_defaults
        )
      )
    } else {
      cli::cli_abort(
        "Error in f1predicter:::get_hyperparameters: {.param timing} must be {.val early}, {.val late}, or {.val after-quali}."
      )
    }
  } else {
    cli::cli_abort(
      "Error in f1predicter:::get_hyperparameters: {.param model} must be {.val quali} or {.val results}."
    )
  }
}
