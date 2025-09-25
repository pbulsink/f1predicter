# --------------------------- Ensemble Models ----------------------------

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
    model_stack <- stacks::add_candidates(
      model_stack,
      candidate_resamples[[i]],
      name = names(candidate_resamples)[i]
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

  # Save the model if requested
  if (save_model) {
    cli::cli_rule("Saving Ensemble Model")
    # Define a path for the ensemble model
    ensemble_file_path <- construct_ensemble_model_path(model_name)

    dir.create(
      dirname(ensemble_file_path),
      showWarnings = FALSE,
      recursive = TRUE
    )

    tryCatch(
      {
        # Butcher the final_ensemble object to reduce its size
        butchered_ensemble <- butcher::butcher(final_ensemble)
        saveRDS(butchered_ensemble, file = ensemble_file_path)
        cli::cli_alert_success(
          "Ensemble model successfully butchered and saved to {.path {ensemble_file_path}}."
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to butcher and save ensemble model '{model_name}': {e$message}"
        )
      }
    )
  }

  return(final_ensemble)
}

#' Construct Ensemble Model File Path
#'
#' Internal helper to construct a standardized file path for saving/loading ensemble models.
#'
#' @param model_name The display name for the model (e.g., "Quali Position Ensemble").
#' @return A full file path string.
#' @noRd
construct_ensemble_model_path <- function(model_name) {
  base_path <- getOption("f1predicter.models")
  if (is.null(base_path)) {
    cli::cli_abort(c(
      "Model directory path is not set.",
      "i" = "Please set the path using `options(f1predicter.models = 'path/to/your/models')`."
    ))
  }
  # Sanitize model_name for filename
  safe_model_name <- gsub("[^a-zA-Z0-9_]", "_", tolower(model_name))
  file_name <- paste0("ensemble_", safe_model_name, "_model.rds")
  file.path(base_path, file_name)
}

#' Load a Stacked Ensemble Model
#'
#' Loads a previously saved `model_stack` object.
#'
#' @param model_name The original name of the ensemble model (e.g., "Quali Position Ensemble").
#' @return A loaded `model_stack` object.
#' @export
load_ensemble_model <- function(model_name) {
  file_path <- construct_ensemble_model_path(model_name)
  if (!file.exists(file_path)) {
    cli::cli_abort("Ensemble model file not found at {.path {file_path}}.")
  }
  model <- readRDS(file_path)
  cli::cli_inform("Ensemble model loaded from {.path {file_path}}.")
  return(model)
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
#'     \item `"results"`: For race result prediction models (win, podium, top 10, finish status, and final position).
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
#' @examples
#' \dontrun{
#'   get_hyperparameters(model = 'quali', timing = 'early')
#' }
get_hyperparameters <- function(model = 'quali', timing = 'early') {
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
          )
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
          )
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
          finish_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 7, penalty = 1, epochs = 10),
            'ranger' = list(mtry = 1, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 2.15443469003189e-07)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 340),
            'ranger' = list(mtry = 4, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          )
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
          finish_hyperparameters = list(
            'glmnet' = list(penalty = 0.00316227766016838, mixture = 1),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1e-10, epochs = 670),
            'ranger' = list(mtry = 1, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 1, rbf_sigma = 0.000464158883361278)
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0.25),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 340),
            'ranger' = list(mtry = 5, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          )
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
          finish_hyperparameters = list(
            'glmnet' = list(penalty = 1, mixture = 0),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1e-10, epochs = 10),
            'ranger' = list(mtry = 1, min_n = 40), # trees = 1000
            'kernlab' = list(
              cost = 0.0009765625,
              rbf_sigma = 0.000464158883361278
            )
          ),
          position_hyperparameters = list(
            'glmnet' = list(penalty = 1e-10, mixture = 0),
            'kknn' = list(neighbors = 25),
            'nnet' = list(hidden_units = 1, penalty = 1, epochs = 670),
            'ranger' = list(mtry = 6, min_n = 40), # trees = 1000
            'kernlab' = list(cost = 32, rbf_sigma = 0.000464158883361278)
          )
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
