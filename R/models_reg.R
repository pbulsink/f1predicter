# Models (regression/classification via xgboost/glmnet)

# 1: Who will get Pole - classification
# 2: Qualifying Position - regression
# 3: Winner - classification
# 4: Podium - classification
# 5: T10 - classification
# 6: Finishing Position - regression

#' Prepare and Split Data for Modeling
#'
#' This helper function preprocesses the data by selecting columns,
#' converting character columns to factors, and then splits the data into
#' training/testing sets and creates cross-validation folds.
#' @param data The input data frame.
#' @param columns A character vector of column names to select. If `NULL`,
#'   all columns in `data` are used.
#' @param prop The proportion of data to be in the training set.
#' @param group The variable to group by for the split.
#' @return A list containing the data split object, training data,
#'   testing data, and cross-validation folds.
prepare_and_split_data <- function(
  data,
  columns = NULL,
  prop = 4 / 5,
  group = "round_id"
) {
  if (!is.null(columns)) {
    processed_data <- dplyr::select(data, dplyr::all_of(columns))
  } else {
    processed_data <- data
  }
  processed_data <- processed_data %>%
    dplyr::mutate_if(is.character, as.factor)

  data_split <- rsample::group_initial_split(
    processed_data,
    prop = prop,
    group = group
  )
  train_data <- rsample::training(data_split)

  list(
    data_split = data_split,
    train_data = train_data,
    test_data = rsample::testing(data_split),
    data_folds = rsample::group_vfold_cv(data = train_data, group = group)
  )
}

#' Report Model Performance Metrics
#'
#' A helper function to generate and print a formatted message with key
#' performance metrics from a `last_fit` object.
#'
#' @param last_fit_object A `last_fit` object from `tune`.
#' @param model_name A character string for the model's name (e.g., "Pole Quali Model").
#' @param metrics A named character vector where names are the metric IDs from
#'   `yardstick` (e.g., "mn_log_loss") and values are the display names (e.g., "log loss").
#' @return Invisibly returns the `last_fit_object`.
report_model_metrics <- function(last_fit_object, model_name, metrics) {
  # Extract the metrics data frame once
  collected_metrics <- tune::collect_metrics(last_fit_object)

  # Build the string parts for each metric
  metric_strings <- purrr::map2_chr(
    names(metrics),
    metrics,
    function(metric_id, display_name) {
      value <- collected_metrics %>%
        dplyr::filter(.data$.metric == metric_id) %>%
        dplyr::pull(.data$.estimate)

      # Return NULL if a metric wasn't found, so it can be filtered out
      if (length(value) == 0) {
        return(NULL)
      }

      paste0(round(value, 4), " ", display_name)
    }
  )

  # Filter out any NULLs if a metric wasn't found
  metric_strings <- purrr::discard(metric_strings, is.null)

  # Combine everything into a final message
  full_message <- paste0(
    model_name,
    " with ",
    paste(metric_strings, collapse = ", "),
    "."
  )

  message(full_message)
  invisible(last_fit_object)
}


# --------------------------- Quali Models ----------------------------

#' Train Qualifying Prediction Models
#'
#' @description
#' This function trains two classification models to predict qualifying results.
#' It can be configured to use data from before ("early") or after ("late")
#' practice sessions have occurred.
#'
#' The function produces:
#' 1. A binary classification model (`quali_pole`) to predict if a driver will
#'    achieve pole position.
#' 2. A multiclass classification model (`quali_pos`) to predict a driver's exact
#'    qualifying position (1-20).
#'
#' @details
#' The function first filters the input data to include seasons from 2018 onwards.
#' Both models are built using the `xgboost` engine. Hyperparameters are tuned
#' via `tune::tune_grid()` with a Latin hypercube grid search.
#'
#' The pole position model is optimized by selecting the best model based on the
#' `mn_log_loss` metric. The qualifying position model is optimized based on the
#' `kap` (Cohen's Kappa) metric.
#'
#' Upon completion, the function prints the final performance metrics of each
#' model (log loss, accuracy, AUC, etc.) to the console.
#'
#' @param data A data frame containing the modeling data.
#' @param use_practice_data A logical value. If `TRUE`, includes practice session
#'   performance metrics as predictors ("late" model). If `FALSE` (default),
#'   it uses data available before practice sessions ("early" model).
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @return A list containing two fitted `workflow` objects: `quali_pole` and `quali_pos`.
train_quali_models <- function(
  data,
  use_practice_data = FALSE,
  engine = "xgboost"
) {
  if (engine == 'xgboost') {
    if (!requireNamespace('xgboost', quietly = TRUE)) {
      cli::cli_abort(
        "Error in f1predicter:::train_quali_models. Package {.code xgboost} needs to be installed"
      )
    }
  } else if (engine == "glmnet") {
    if (!requireNamespace('glmnet', quietly = TRUE)) {
      cli::cli_abort(
        "Error in f1predicter:::train_quali_models. Package {.code glmnet} needs to be installed"
      )
    }
  } else {
    cli::cli_abort(
      "Error in f1predicter:::train_quali_models. Parameter {.param engine} must be either {.code 'xgboost'} or {.code 'glmnet'}."
    )
  }
  data <- data[data$season >= 2018, ]
  p_mod_data <- data # Used later for position model

  # ---- Pole Model Setup ----
  data$pole <- factor(ifelse(data$quali_position == 1, 1, 0), levels = c(1, 0))

  base_pole_cols <- c(
    "quali_position",
    "pole",
    "driver_experience",
    "driver_failure_avg",
    "constructor_grid_avg",
    "constructor_finish_avg",
    "constructor_failure_avg",
    "driver_grid_avg",
    "driver_position_avg",
    "driver_finish_avg",
    "driver_failure_circuit_avg",
    'driver_avg_qgap',
    "constructor_failure_circuit_avg",
    "season",
    "round",
    "round_id",
    "driver_id",
    "constructor_id"
  )

  # check if these are today's practice columns or average practice columns
  practice_cols <- c(
    "driver_practice_optimal_rank_avg",
    "practice_avg_rank",
    "practice_best_rank",
    "practice_optimal_rank"
  )

  pole_cols <- if (use_practice_data) {
    c(base_pole_cols, practice_cols)
  } else {
    base_pole_cols
  }

  pole_splits <- prepare_and_split_data(data, columns = pole_cols)
  train_data_pole <- pole_splits$train_data
  data_folds_pole <- pole_splits$data_folds
  data_split_pole <- pole_splits$data_split

  metrics_multi <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    yardstick::mcc,
    yardstick::mn_log_loss,
    yardstick::roc_auc
  )
  metrics_binary <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::mn_log_loss,
    yardstick::roc_auc
  )
  metrics_reg <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::rsq
  )

  # ---- Pole Model Training ----
  pole_recipe <- recipes::recipe(pole ~ ., data = train_data_pole) %>%
    recipes::update_role(
      "season",
      "round",
      "round_id",
      "driver_id",
      "constructor_id",
      new_role = "ID"
    ) %>%
    recipes::step_rm("quali_position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  if (engine == "xgboost") {
    pole_model_spec <- parsnip::boost_tree(
      trees = 1000,
      tree_depth = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune(),
      mtry = tune::tune(),
      learn_rate = tune::tune(),
      stop_iter = tune::tune()
    ) %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine("xgboost", nthread = 10)

    pole_grid <- dials::grid_latin_hypercube(
      dials::tree_depth(),
      dials::min_n(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      dials::finalize(dials::mtry(), train_data_pole),
      dials::learn_rate(),
      dials::stop_iter(),
      size = 30
    )
  } else if (engine == "glmnet") {
    pole_model_spec <- parsnip::logistic_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")

    pole_grid <- dials::grid_regular(
      dials::penalty(),
      dials::mixture(),
      levels = 5
    )
  } else {
    stop("Invalid engine specified. Choose 'xgboost' or 'glmnet'.")
  }

  pole_wflow <- workflows::workflow() %>%
    workflows::add_model(pole_model_spec) %>%
    workflows::add_recipe(pole_recipe)

  tictoc::tic("Trained Pole Model")
  pole_res <- pole_wflow %>%
    tune::tune_grid(
      resamples = data_folds_pole,
      grid = pole_grid,
      metrics = metrics_binary
    )

  tictoc::toc()

  pole_best <- pole_res %>%
    tune::select_best(metric = "mn_log_loss")

  pole_final <- pole_wflow %>%
    tune::finalize_workflow(pole_best)

  pole_final_fit <- pole_final %>%
    tune::last_fit(data_split_pole, metrics = metrics_binary)

  report_model_metrics(
    pole_final_fit,
    "Pole Quali Model",
    c("mn_log_loss" = "log loss", "accuracy" = "accuracy", "roc_auc" = "auc")
  )

  # ---- Quali Position Model Setup ----
  pos_cols <- pole_cols
  if ("pole" %in% pos_cols) {
    pos_cols <- pos_cols[pos_cols != "pole"]
  }

  # The data for the position model should not be filtered or mutated based on
  # the race result 'position'. We are predicting 'quali_position'.
  pos_data <- p_mod_data %>%
    dplyr::filter(!is.na(.data$quali_position)) %>% # Ensure we have a quali result
    dplyr::select(dplyr::all_of(pos_cols))

  pos_splits <- prepare_and_split_data(pos_data)
  train_data_pos <- pos_splits$train_data
  data_folds_pos <- pos_splits$data_folds
  data_split_pos <- pos_splits$data_split

  # ---- Quali Position Model Training ----
  position_recipe <- recipes::recipe(
    quali_position ~ .,
    data = train_data_pos
  ) %>%
    recipes::update_role(
      "season",
      "round",
      "round_id",
      "driver_id",
      "constructor_id",
      new_role = "ID"
    ) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  if (engine == "xgboost") {
    position_model_spec <- parsnip::boost_tree(
      trees = 1000,
      tree_depth = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune(),
      mtry = tune::tune(),
      learn_rate = tune::tune(),
      stop_iter = tune::tune()
    ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("xgboost", nthread = 10)

    position_grid <- dials::grid_latin_hypercube(
      dials::tree_depth(),
      dials::min_n(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      dials::finalize(dials::mtry(), train_data_pos),
      dials::learn_rate(),
      dials::stop_iter(),
      size = 30
    )
  } else if (engine == "glmnet") {
    position_model_spec <- parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")

    position_grid <- dials::grid_regular(
      dials::penalty(),
      dials::mixture(),
      levels = 5
    )
  }

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_model_spec) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(
      resamples = data_folds_pos,
      grid = position_grid,
      metrics = metrics_reg
    )

  position_best <- position_res %>%
    tune::select_best(metric = "rmse")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best)

  position_final_fit <- position_final %>%
    tune::last_fit(data_split_pos, metrics = metrics_reg)

  report_model_metrics(
    position_final_fit,
    "Quali Position Model",
    c("rmse" = "rmse", "mae" = "mae", "rsq" = "r-squared")
  )

  # ---- Quali Position Classification Model (Ordered Logistic) ----
  tictoc::tic("Trained Position Classification Model (polr)")

  # Use the same data as the regression model, but with a factor outcome and
  # prepped for MASS:polr
  pos_class_data <- pos_data %>%
    dplyr::arrange(.data$quali_position) %>%
    dplyr::mutate(
      quali_position = factor(.data$quali_position, ordered = TRUE)
    ) %>%
    dplyr::arrange(.data$season, .data$round, .data$quali_position)

  pos_class_splits <- prepare_and_split_data(pos_class_data)
  train_data_pos_class <- pos_class_splits$train_data
  data_split_pos_class <- pos_class_splits$data_split

  pos_class_recipe <- recipes::recipe(
    quali_position ~ .,
    data = train_data_pos_class
  ) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Prep the recipe and bake the data
  prepped_recipe <- recipes::prep(
    pos_class_recipe,
    training = train_data_pos_class
  )
  baked_train <- recipes::bake(prepped_recipe, new_data = NULL)
  baked_test <- recipes::bake(
    prepped_recipe,
    new_data = rsample::testing(data_split_pos_class)
  )

  # Fit the model directly using MASS::polr
  # Polr worked better with an explicit formula

  if (use_practice_data) {
    polr_fit <- MASS::polr(
      quali_position ~
        driver_experience +
          driver_failure_avg +
          constructor_grid_avg +
          constructor_finish_avg +
          constructor_failure_avg +
          driver_grid_avg +
          driver_position_avg +
          driver_finish_avg +
          driver_failure_circuit_avg +
          driver_avg_qgap +
          constructor_failure_circuit_avg +
          driver_practice_optimal_rank_avg +
          practice_avg_rank +
          practice_best_rank +
          practice_optimal_rank,
      data = baked_train,
      Hess = TRUE
    )
  } else {
    polr_fit <- MASS::polr(
      quali_position ~
        driver_experience +
          driver_failure_avg +
          constructor_grid_avg +
          constructor_finish_avg +
          constructor_failure_avg +
          driver_grid_avg +
          driver_position_avg +
          driver_finish_avg +
          driver_failure_circuit_avg +
          driver_avg_qgap +
          constructor_failure_circuit_avg,
      data = baked_train,
      Hess = TRUE
    )
  }

  # --- Evaluate the model on the test set ---
  # Get class predictions
  class_preds <- predict(polr_fit, newdata = baked_test, type = "class")
  # Get probability predictions
  prob_preds <- predict(polr_fit, newdata = baked_test, type = "probs")

  # Combine true values and predictions
  test_results <- dplyr::bind_cols(
    baked_test %>% dplyr::select(truth = quali_position),
    .pred_class = class_preds,
    tibble::as_tibble(prob_preds)
  )

  # Calculate metrics
  log_loss_val <- yardstick::mn_log_loss(
    test_results,
    truth = truth,
    colnames(test_results)[
      !(colnames(test_results) %in% c("truth", ".pred_class"))
    ]
  )
  accuracy_val <- yardstick::accuracy(test_results, truth = truth, .pred_class)
  kap_val <- yardstick::kap(test_results, truth = truth, .pred_class)

  # Manually create a metrics object for reporting
  polr_metrics <- tibble::tribble(
    ~.metric,
    ~.estimator,
    ~.estimate,
    "mn_log_loss",
    "standard",
    log_loss_val$.estimate,
    "accuracy",
    "multiclass",
    accuracy_val$.estimate,
    "kap",
    "multiclass",
    kap_val$.estimate
  )

  # Report metrics
  message(glue::glue(
    "Quali Position Ordinal Model (polr) with {round(log_loss_val$.estimate, 4)} log loss, {round(accuracy_val$.estimate, 4)} accuracy, {round(kap_val$.estimate, 4)} kappa."
  ))
  tictoc::toc()

  pos_class_final_fit <- list(
    fit = polr_fit,
    recipe = prepped_recipe,
    metrics = polr_metrics
  )

  # ---- Return ----
  return(list(
    "quali_pole" = pole_final_fit,
    'quali_pos' = position_final_fit,
    'quali_pos_class' = pos_class_final_fit
  ))
}


#' Train Early Qualifying Prediction Models
#'
#' @description
#' This function trains two classification models to predict qualifying results using
#' data available before any practice sessions have occurred for a race weekend. It
#' is a wrapper around `train_quali_models(use_practice_data = FALSE)`.
#'
#' @inherit train_quali_models details
#' @param data A data frame containing the modeling data. Defaults to the output of `clean_data()`.
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @inherit train_quali_models return
#' @export
model_quali_early <- function(
  data = clean_data(),
  engine = "xgboost",
  save_model = TRUE
) {
  models <- train_quali_models(data, use_practice_data = FALSE, engine = engine)
  if (save_model) {
    save_models(model_list = models, model_timing = "early")
  }
  return(models)
}

#' Train Late Qualifying Prediction Models
#'
#' @description
#' This function trains two classification models to predict qualifying results using
#' data available *after* all practice sessions have occurred for a race weekend. This
#' model includes practice performance metrics as predictors. It is a wrapper
#' around `train_quali_models(use_practice_data = TRUE)`.
#'
#' @inherit train_quali_models details
#' @param data A data frame containing the modeling data. Defaults to the output of `clean_data()`.
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @inherit train_quali_models return
#' @export
model_quali_late <- function(
  data = clean_data(),
  engine = "xgboost",
  save_model = TRUE
) {
  models <- train_quali_models(data, use_practice_data = TRUE, engine = engine)
  if (save_model) {
    save_models(model_list = models, model_timing = "late")
  }
  return(models)
}


#' Train a Binary Race Result Model
#'
#' Internal helper function to train a single binary classification model for
#' race results (e.g., win, podium, t10, finish).
#'
#' @param outcome_var The name of the outcome variable (string).
#' @param model_name The display name for the model (string).
#' @param train_data The training data frame.
#' @param data_split The data split object.
#' @param data_folds The cross-validation folds.
#' @param model_spec The parsnip model specification.
#' @param grid The tuning grid.
#' @param other_outcomes A character vector of other outcome variables to remove
#'   from the predictors in the recipe.
#' @return A fitted `workflow` object.
train_binary_result_model <- function(
  outcome_var,
  model_name,
  train_data,
  data_split,
  data_folds,
  model_spec,
  grid,
  other_outcomes
) {
  recipe <- recipes::recipe(
    reformulate(
      colnames(train_data)[
        !(colnames(train_data) %in% c(outcome_var, other_outcomes, "position"))
      ],
      response = outcome_var
    ),
    data = train_data
  ) %>%
    recipes::update_role(
      "season",
      "round",
      "round_id",
      "driver_id",
      "constructor_id",
      new_role = "ID"
    ) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  wflow <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipe)

  metrics_binary <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::mn_log_loss,
    yardstick::roc_auc
  )

  tictoc::tic(paste("Trained", model_name))
  res <- wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = grid,
      metrics = metrics_binary
    )

  best_params <- res %>%
    tune::select_best(metric = "mn_log_loss")
  tictoc::toc()

  final_wflow <- wflow %>%
    tune::finalize_workflow(best_params)

  final_fit <- final_wflow %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  report_model_metrics(
    final_fit,
    model_name,
    c("mn_log_loss" = "log loss", "accuracy" = "accuracy", "roc_auc" = "auc")
  )

  return(final_fit)
}

#' Train Race Result Models
#'
#' @description
#' This function trains a suite of five classification models to predict race
#' results based on the specified scenario.
#'
#' @param data A data frame containing the modeling data.
#' @param scenario A character string specifying the modeling context. One of
#'   "early" (pre-practice), "late" (post-practice), or "after_quali".
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @return A list containing five fitted `workflow` objects.
train_results_models <- function(data, scenario, engine = "xgboost") {
  # ---- Common Data Prep ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data # Keep original for position model

  data$win <- factor(ifelse(data$position == 1, 1, 0), levels = c(1, 0))
  data$podium <- factor(ifelse(data$position <= 3, 1, 0), levels = c(1, 0))
  data$t10 <- factor(ifelse(data$position <= 10, 1, 0), levels = c(1, 0))
  data$finished <- factor(data$finished, levels = c(1, 0))
  # `position` is kept as numeric for regression
  data$round_id <- as.factor(data$round_id)

  # ---- Scenario-specific Columns ----
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
    'driver_avg_qgap',
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
  outcome_cols <- c("win", "podium", "t10", "finished", "position")

  results_cols <- switch(
    scenario,
    "early" = c(base_cols, outcome_cols),
    "late" = c(base_cols, practice_cols, outcome_cols),
    "after_quali" = c(base_cols, practice_cols, quali_perf_cols, outcome_cols)
  )

  # ---- Data Splits ----
  splits <- prepare_and_split_data(data, columns = results_cols)
  train_data <- splits$train_data
  data_split <- splits$data_split
  data_folds <- splits$data_folds

  # ---- Model Specs and Grids ----
  if (engine == "xgboost") {
    class_mod_spec <- parsnip::boost_tree(
      trees = 1000,
      tree_depth = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune(),
      mtry = tune::tune(),
      learn_rate = tune::tune(),
      stop_iter = tune::tune()
    ) %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine("xgboost", nthread = 10)

    reg_mod_spec <- parsnip::boost_tree(
      trees = 1000,
      tree_depth = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune(),
      mtry = tune::tune(),
      learn_rate = tune::tune(),
      stop_iter = tune::tune()
    ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("xgboost", nthread = 10)

    grid <- dials::grid_latin_hypercube(
      dials::tree_depth(),
      dials::min_n(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      dials::finalize(dials::mtry(), train_data),
      dials::learn_rate(),
      dials::stop_iter(),
      size = 30
    )
  } else if (engine == "glmnet") {
    class_mod_spec <- parsnip::logistic_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")

    reg_mod_spec <- parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")

    grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)
  } else {
    stop("Invalid engine specified. Choose 'xgboost' or 'glmnet'.")
  }

  # ---- Train Binary Models ----
  binary_outcomes <- c("win", "podium", "t10", "finished")

  win_final <- train_binary_result_model(
    "win",
    "Win Model",
    train_data,
    data_split,
    data_folds,
    class_mod_spec,
    grid,
    setdiff(binary_outcomes, "win")
  )
  podium_final <- train_binary_result_model(
    "podium",
    "Podium Model",
    train_data,
    data_split,
    data_folds,
    class_mod_spec,
    grid,
    setdiff(binary_outcomes, "podium")
  )
  t10_final <- train_binary_result_model(
    "t10",
    "T10 Model",
    train_data,
    data_split,
    data_folds,
    class_mod_spec,
    grid,
    setdiff(binary_outcomes, "t10")
  )
  finish_final <- train_binary_result_model(
    "finished",
    "Finishing Model",
    train_data,
    data_split,
    data_folds,
    class_mod_spec,
    grid,
    setdiff(binary_outcomes, "finished")
  )

  # ---- Train Position Model (Regression) ----
  pos_cols <- setdiff(results_cols, binary_outcomes)
  pos_data <- p_mod_data %>%
    dplyr::select(dplyr::all_of(pos_cols))

  pos_splits <- prepare_and_split_data(pos_data)
  position_recipe <- recipes::recipe(
    position ~ .,
    data = pos_splits$train_data
  ) %>%
    recipes::update_role(
      "season",
      "round",
      "round_id",
      "driver_id",
      "constructor_id",
      new_role = "ID"
    ) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(reg_mod_spec) %>%
    workflows::add_recipe(position_recipe)

  metrics_reg <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::rsq
  )
  tictoc::tic("Trained Position Model")
  position_res <- tune::tune_grid(
    position_wflow,
    resamples = pos_splits$data_folds,
    grid = grid,
    metrics = metrics_reg
  )
  position_best <- position_res %>%
    tune::select_best(metric = "rmse")
  tictoc::toc(log = T)

  position_final_wflow <- position_wflow %>%
    tune::finalize_workflow(position_best)

  position_final_fit <- position_final_wflow %>%
    tune::last_fit(
      pos_splits$data_split,
      metrics = metrics_reg
    )

  report_model_metrics(
    position_final_fit,
    "Position Model",
    c("rmse" = "rmse", "mae" = "mae", "rsq" = "r-squared")
  )

  # ---- Train Position Model (Ordinal Classification) ----
  tictoc::tic("Trained Position Classification Model (polr)")

  pos_class_data <- pos_data %>%
    dplyr::mutate(position = factor(.data$position, ordered = TRUE))

  pos_class_splits <- prepare_and_split_data(pos_class_data)
  train_data_pos_class <- pos_class_splits$train_data
  test_data_pos_class <- pos_class_splits$test_data

  pos_class_recipe <- recipes::recipe(
    position ~ .,
    data = train_data_pos_class
  ) %>%
    recipes::update_role(
      "season",
      "round",
      "round_id",
      "driver_id",
      "constructor_id",
      new_role = "ID"
    ) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  prepped_recipe <- recipes::prep(
    pos_class_recipe,
    training = train_data_pos_class
  )
  baked_train <- recipes::bake(prepped_recipe, new_data = NULL)
  baked_test <- recipes::bake(prepped_recipe, new_data = test_data_pos_class)

  # Create a formula that excludes other outcome variables
  predictor_vars <- setdiff(
    names(baked_train),
    c("position", "win", "podium", "t10", "finished")
  )
  polr_formula <- reformulate(
    termlabels = predictor_vars,
    response = "position"
  )

  # Fit the model directly using MASS::polr
  polr_fit <- MASS::polr(
    polr_formula,
    data = baked_train,
    Hess = TRUE
  )

  # --- Evaluate the model on the test set ---
  class_preds <- predict(polr_fit, newdata = baked_test, type = "class")
  prob_preds <- predict(polr_fit, newdata = baked_test, type = "probs")

  test_results <- dplyr::bind_cols(
    baked_test %>% dplyr::select(truth = position),
    .pred_class = class_preds,
    tibble::as_tibble(prob_preds)
  )

  # Calculate metrics
  log_loss_val <- yardstick::mn_log_loss(
    test_results,
    truth = truth,
    dplyr::starts_with("..")
  )
  accuracy_val <- yardstick::accuracy(test_results, truth = truth, .pred_class)
  kap_val <- yardstick::kap(test_results, truth = truth, .pred_class)

  # Manually create a metrics object for reporting
  polr_metrics <- tibble::tribble(
    ~.metric,
    ~.estimator,
    ~.estimate,
    "mn_log_loss",
    "standard",
    log_loss_val$.estimate,
    "accuracy",
    "multiclass",
    accuracy_val$.estimate,
    "kap",
    "multiclass",
    kap_val$.estimate
  )

  message(glue::glue(
    "Position Ordinal Model (polr) with {round(log_loss_val$.estimate, 4)} log loss, {round(accuracy_val$.estimate, 4)} accuracy, {round(kap_val$.estimate, 4)} kappa."
  ))
  tictoc::toc()

  position_class_final_fit <- list(
    fit = polr_fit,
    recipe = prepped_recipe,
    metrics = polr_metrics
  )

  return(list(
    "win" = win_final,
    "podium" = podium_final,
    "t10" = t10_final,
    'finish' = finish_final,
    'position' = position_final_fit,
    'position_class' = position_class_final_fit
  ))
}

# --------------------------- Results Models --------------------------
#' Train Race Result Models (Post-Qualifying)
#'
#' @description
#' This function trains a suite of five classification models to predict race
#' results using data available *after* qualifying has completed. This includes
#' historical data, practice session metrics, and final qualifying results.
#'
#' The function produces:
#' 1. `win`: A binary classification model to predict if a driver will win the race.
#' 2. `podium`: A binary classification model to predict if a driver will finish in the top 3.
#' 3. `t10`: A binary classification model to predict if a driver will finish in the top 10.
#' 4. `finish`: A binary classification model to predict if a driver will finish the race (not DNF).
#' 5. `position`: A regression model to predict a driver's exact finishing position.
#'
#' @details
#' The function filters data for seasons from 2018 onwards. All five models are
#' built using the `xgboost` engine, with hyperparameters tuned via a Latin
#' hypercube grid search.
#'
#' The four binary models (`win`, `podium`, `t10`, `finish`) are optimized on
#' the `mn_log_loss` metric. The `position` regression model is optimized
#' based on the `rmse` (Root Mean Squared Error) metric.
#'
#' @param data A data frame containing the modeling data. Defaults to `clean_data()`.
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @return A list containing five fitted `workflow` objects for `win`, `podium`,
#'   `t10`, `finish`, and `position`.
#' @export
model_results_after_quali <- function(
  data = clean_data(),
  engine = "xgboost",
  save_model = TRUE
) {
  models <- train_results_models(
    data,
    scenario = "after_quali",
    engine = engine
  )
  if (save_model) {
    save_models(model_list = models, model_timing = "after_quali")
  }
  return(models)
}

#' Train Race Result Models (Post-Practice)
#'
#' @description
#' This function trains a suite of five classification models to predict race
#' results using data available *after* practice sessions but *before*
#' qualifying.
#'
#' It produces the same models as `model_results_after_quali`, but uses a
#' smaller set of predictors.
#'
#' @inherit model_results_after_quali details
#' @param data A data frame containing the modeling data. Defaults to `clean_data()`.
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @inherit model_results_after_quali return
#' @export
model_results_late <- function(
  data = clean_data(),
  engine = "xgboost",
  save_model = TRUE
) {
  models <- train_results_models(data, scenario = "late", engine = engine)
  if (save_model) {
    save_models(model_list = models, model_timing = "late")
  }
  return(models)
}

#' Train Race Result Models (Pre-Practice)
#'
#' @description
#' This function trains a suite of five classification models to predict race
#' results using data available *before* any practice or qualifying sessions
#' have occurred for a race weekend.
#'
#' It produces the same models as `model_results_after_quali`, but uses a
#' smaller set of predictors.
#'
#' @inherit model_results_after_quali details
#' @param data A data frame containing the modeling data. Defaults to `clean_data()`.
#' @param engine A character string specifying the model engine. One of "xgboost"
#'   (default) or "glmnet".
#' @param save_model A logical value. If `TRUE` (default), the trained models
#'   are automatically butchered and saved to the path specified in
#'   `options('f1predicter.models')`.
#' @inherit model_results_after_quali return
#' @export
model_results_early <- function(
  data = clean_data(),
  engine = "xgboost",
  save_model = TRUE
) {
  models <- train_results_models(data, scenario = "early", engine = engine)
  if (save_model) {
    save_models(model_list = models, model_timing = "early")
  }
  return(models)
}

#' Construct Model File Path
#'
#' Internal helper to construct a standardized file path for saving/loading models.
#' It validates the model type and timing, and checks for the required option.
#'
#' @param model_type The type of model, either "quali" or "results".
#' @param model_timing The timing of the model, one of "early", "late", or "after_quali".
#' @return A full file path string.
#' @noRd
construct_model_path <- function(model_type, model_timing) {
  # 1. Check if the base path option is set
  base_path <- getOption("f1predicter.models")
  if (is.null(base_path)) {
    cli::cli_abort(
      c(
        "Model directory path is not set.",
        "i" = "Please set the path using `options(f1predicter.models = 'path/to/your/models')`."
      )
    )
  }

  # 2. Validate model_type
  if (!model_type %in% c("quali", "results")) {
    cli::cli_abort(
      "{.arg model_type} must be one of 'quali' or 'results', not {.val {model_type}}."
    )
  }

  # 3. Validate model_timing based on model_type
  valid_timings <- if (model_type == "quali") {
    c("early", "late")
  } else {
    # "results"
    c("early", "late", "after_quali")
  }

  if (!model_timing %in% valid_timings) {
    cli::cli_abort(
      c(
        "{.arg model_timing} is invalid for model type {.val {model_type}}.",
        "i" = "Valid timings for '{model_type}' are: {.val {valid_timings}}.",
        "x" = "You provided: {.val {model_timing}}."
      )
    )
  }

  # 4. Construct and return the full path
  file_name <- paste0(model_type, "_", model_timing, "_models.rds")
  file.path(base_path, file_name)
}


#' Save Models
#'
#' This function takes a list of trained `last_fit` model object
#' and saves the resulting list to a standardized file
#' path. The `model_type` ("quali" or "results") is automatically inferred from
#' the names of the models in the list (e.g., "quali_pole", "win").
#'
#' The path is constructed from the base directory set in
#' `getOption("f1predicter.models")` and the specified model timing.
#'
#' @param model_list A named list of `last_fit` objects from `tune`. The names
#'   (e.g., "quali_pole", "win") are used to infer the model type.
#' @param model_timing The timing context for the models, one of `"early"`,
#'   `"late"`, or (for "results" models only) `"after_quali"`.
#' @return Invisibly returns the full `file_path` where models were saved.
#' @export
save_models <- function(model_list, model_timing) {
  # Infer model_type from the names in model_list
  model_names <- names(model_list)
  is_quali <- any(grepl("quali", model_names, fixed = TRUE))
  is_results <- any(
    model_names %in% c("win", "podium", "t10", "finish", "position")
  )

  if (is_quali && is_results) {
    cli::cli_abort(c(
      "Ambiguous model list provided. Contains names for both 'quali' and 'results' models.",
      "i" = "Please provide a list containing only one type of model."
    ))
  } else if (is_quali) {
    model_type <- "quali"
  } else if (is_results) {
    model_type <- "results"
  } else {
    cli::cli_abort(c(
      "Could not automatically determine {.arg model_type} from the names in {.arg model_list}.",
      "i" = "Expected names to contain 'quali' for qualifying models, or be among {.val {c('win', 'podium', 't10', 'finish', 'position')}} for results models.",
      "x" = "Found names: {.val {model_names}}"
    ))
  }

  file_path <- construct_model_path(
    model_type = model_type,
    model_timing = model_timing
  )
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  final_list <- list()
  for (model_name in names(model_list)) {
    model_object <- model_list[[model_name]]

    # Handle the special case for the polr model, which is a list(fit, recipe)
    if (inherits(model_object$fit, "polr")) {
      cli::cli_inform("Saving polr model object for: {.val {model_name}}")
      final_list[[model_name]] <- model_object
      next # Skip to the next model
    }

    # Proceed with butchering for standard last_fit objects
    if (!inherits(model_object, "last_fit")) {
      cli::cli_warn(
        "Object {.val {model_name}} is not a 'last_fit' object or a recognized custom model and will be skipped."
      )
      next
    }

    # Butcher the workflow to reduce size, wrapped in a tryCatch for robustness
    tryCatch(
      {
        # Extract the fitted workflow from the last_fit object
        fitted_wf <- model_object$.workflow[[1]]
        # Butcher the workflow
        butchered_wf <- butcher::butcher(fitted_wf)
        # Replace the original workflow with the butchered one
        model_object$.workflow[[1]] <- butchered_wf

        cli::cli_inform("Butchered workflow for model: {.val {model_name}}")
        final_list[[model_name]] <- model_object
      },
      error = function(e) {
        cli::cli_warn(
          "Failed to butcher model {.val {model_name}}: {e$message}"
        )
      }
    )
  }

  if (length(final_list) > 0) {
    saveRDS(final_list, file = file_path)
    cli::cli_inform(
      "Models successfully butchered and saved to {.path {file_path}}."
    )
  } else {
    cli::cli_warn("No valid models were found to save.")
  }

  invisible(file_path)
}

#' Load Models
#'
#' Loads a list of models from a standardized file path. The path is
#' constructed from the base directory set in `getOption("f1predicter.models")`,
#' the model type, and timing.
#'
#' @param model_type The type of models to load, either `"quali"` or `"results"`.
#' @param model_timing The timing context for the models, one of `"early"`,
#'   `"late"`, or (for "results" models only) `"after_quali"`.
#' @return A list of  model objects, ready for prediction.
#' @export
load_models <- function(model_type, model_timing) {
  file_path <- construct_model_path(model_type, model_timing)

  if (!file.exists(file_path)) {
    cli::cli_abort("Model file not found at {.path {file_path}}.")
  }
  models <- readRDS(file_path)
  cli::cli_inform("Models loaded from {.path {file_path}}.")
  return(models)
}
