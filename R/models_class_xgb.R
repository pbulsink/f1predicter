# Models (classification/xgboost)

# 1: Who will get Pole - Expected position of each person

# 2: Finish Position: - 1st, top 3, top 10 (points), finish

#' @export
model_quali_early_xgb <- function(data = clean_data()) {
  set.seed(1)
  # Model quali early in the week - before practice sessions grid => step_dummy
  data <- data[data$season >= 2018, ]

  message ("configuring data for model")
  # Model results given a grid - predicted or actual
  data$pole <- factor(ifelse(data$quali_position == 1, 1, 0), levels = c(1,0))

  data$round_id <- as.factor(data$round_id)

  p_mod_data <- data  # Used later

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience", "constructor_grid_avg", "driver_grid_avg",
                  "driver_position_avg", 'driver_avg_qgap', "driver_practice_optimal_rank_avg", "pole", "season",
                  "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  metrics_multi <- yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::mcc, yardstick::mn_log_loss,
                                         yardstick::roc_auc)
  metrics_binary <- yardstick::metric_set(yardstick::ppv, yardstick::npv, yardstick::spec, yardstick::sens, yardstick::kap,
                                          yardstick::mn_log_loss, yardstick::roc_auc, yardstick::accuracy)

  metrics_regression <- yardstick::metric_set(yardstick::mae, yardstick::rsq, yardstick::rmse)

  # ---- Pole Model ----
  pole_recipe <- recipes::recipe(pole ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("quali_position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  pole_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(), mtry = tune::tune(),
                                 learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  xgb_grid <- dials::grid_latin_hypercube(dials::tree_depth(), dials::min_n(), dials::loss_reduction(), sample_size = dials::sample_prop(), dials::finalize(dials::mtry(),
                                                                                                                                                            train_data), dials::learn_rate(), dials::stop_iter(), size = 30)

  pole_wflow <- workflows::workflow() %>%
    workflows::add_model(pole_mod) %>%
    workflows::add_recipe(pole_recipe)

  message("Training pole model")
  tictoc::tic("Trained Pole Model")
  pole_res <- pole_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

  pole_best <- pole_res %>%
    tune::select_best("kap")
  tictoc::toc()

  pole_final <- pole_wflow %>%
    tune::finalize_workflow(pole_best) %>%
    parsnip::fit(train_data)
  pole_final_fit <- pole_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Pole Quali Model with ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 4), " npv, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Quali Position Model Classification ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(position = as.factor(.data$position), round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "position", "driver_experience", "constructor_grid_avg", "driver_grid_avg",
                  "driver_position_avg", 'driver_avg_qgap', "driver_practice_optimal_rank_avg", "season",
                  "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note multinomial regression
  position_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  message("Training quali position classification model")
  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_multi)

  position_best <- position_res %>%
    tune::select_best("roc_auc")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best) %>%
    parsnip::fit(train_data)
  position_final_fit <- position_final %>%
    tune::last_fit(data_split, metrics = metrics_multi)

  message("Quali Position Classification Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Quali Regression Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience", "constructor_grid_avg", "driver_grid_avg",
                  "driver_position_avg", 'driver_avg_qgap', "driver_practice_optimal_rank_avg", "season",
                  "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_regress_recipe <- recipes::recipe(quali_position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note linear regression
  position_regress_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_regress_wflow <- workflows::workflow() %>%
    workflows::add_model(position_regress_mod) %>%
    workflows::add_recipe(position_regress_recipe)

  message("Starting quali position model")
  tictoc::tic("Trained Quali Position Model")
  position_regress_res <- position_regress_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_regression)

  position_regress_best <- position_regress_res %>%
    tune::select_best("rmse")
  tictoc::toc(log = T)

  position_regress_final <- position_regress_wflow %>%
    tune::finalize_workflow(position_regress_best) %>%
    parsnip::fit(train_data)
  position_final_regress_fit <- position_regress_final %>%
    tune::last_fit(data_split, metrics = metrics_regression)

  message("Quali Position Model with ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "mae") %>% dplyr::pull(".estimate"), 4), " mae, ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "rsq") %>% dplyr::pull(".estimate"), 4), " rsq, ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "rmse") %>% dplyr::pull(".estimate"), 4), " rmse.")

  return(list("quali_pole" = pole_final, 'quali_pos' = position_final, 'quali_regression_pos' = position_regress_final))
}

#' @export
model_quali_late_xgb <- function(data = clean_data()) {

  set.seed(1)

  # Model quali late in the week - after practices are done. grid => step_dummy
  data <- data[data$season >= 2018, ]

  message ("configuring data for model")
  # Model results given a grid - predicted or actual
  data$pole <- factor(ifelse(data$quali_position == 1, 1, 0), levels = c(1,0))

  data$round_id <- as.factor(data$round_id)

  p_mod_data <- data  # Used later

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience", "driver_failure_avg",
                  "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg", "driver_grid_avg",
                  "driver_position_avg", "driver_finish_avg", "driver_failure_circuit_avg", 'driver_avg_qgap',
                  "constructor_failure_circuit_avg","driver_practice_optimal_rank_avg", "practice_avg_rank",
                  "practice_best_rank", "practice_optimal_rank", "pole", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  metrics_multi <- yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::mcc, yardstick::mn_log_loss,
                                         yardstick::roc_auc)
  metrics_binary <- yardstick::metric_set(yardstick::ppv, yardstick::npv, yardstick::spec, yardstick::sens, yardstick::kap,
                                          yardstick::mn_log_loss, yardstick::roc_auc, yardstick::accuracy)

  metrics_regression <- yardstick::metric_set(yardstick::mae, yardstick::rsq, yardstick::rmse)

  # ---- Pole Model ----
  pole_recipe <- recipes::recipe(pole ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("quali_position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  pole_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(), mtry = tune::tune(),
                                  learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  xgb_grid <- dials::grid_latin_hypercube(dials::tree_depth(), dials::min_n(), dials::loss_reduction(), sample_size = dials::sample_prop(), dials::finalize(dials::mtry(),
                                                                                                                                                            train_data), dials::learn_rate(), dials::stop_iter(), size = 30)

  pole_wflow <- workflows::workflow() %>%
    workflows::add_model(pole_mod) %>%
    workflows::add_recipe(pole_recipe)

  message("Training pole model")
  tictoc::tic("Trained Pole Model")
  pole_res <- pole_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

  pole_best <- pole_res %>%
    tune::select_best("kap")
  tictoc::toc()

  pole_final <- pole_wflow %>%
    tune::finalize_workflow(pole_best) %>%
    parsnip::fit(train_data)
  pole_final_fit <- pole_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Pole Quali Model with ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 4), " npv, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Quali Position Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(position = as.factor(.data$position), round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience",
                  "constructor_grid_avg", "driver_grid_avg", "driver_position_avg", 'driver_avg_qgap',
                  "driver_practice_optimal_rank_avg", "practice_avg_rank",
                  "practice_best_rank", "practice_optimal_rank", "pole", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("pole") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note multinomial regression
  position_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_multi)

  position_best <- position_res %>%
    tune::select_best("kappa")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best) %>%
    parsnip::fit(train_data)
  position_final_fit <- position_final %>%
    tune::last_fit(data_split, metrics = metrics_multi)

  message("Quali Position Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")


  # ---- Quali Regression Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience", "constructor_grid_avg", "driver_grid_avg",
                  "driver_position_avg", 'driver_avg_qgap', "driver_practice_optimal_rank_avg", "season",
                  "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_regress_recipe <- recipes::recipe(quali_position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note linear regression
  position_regress_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                              mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_regress_wflow <- workflows::workflow() %>%
    workflows::add_model(position_regress_mod) %>%
    workflows::add_recipe(position_regress_recipe)

  message("Starting quali position model")
  tictoc::tic("Trained Quali Position Model")
  position_regress_res <- position_regress_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_regression)

  position_regress_best <- position_regress_res %>%
    tune::select_best("rmse")
  tictoc::toc(log = T)

  position_regress_final <- position_regress_wflow %>%
    tune::finalize_workflow(position_regress_best) %>%
    parsnip::fit(train_data)
  position_final_regress_fit <- position_regress_final %>%
    tune::last_fit(data_split, metrics = metrics_regression)

  message("Quali Position Model with ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "mae") %>% dplyr::pull(".estimate"), 4), " mae, ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "rsq") %>% dplyr::pull(".estimate"), 4), " rsq, ",
          round(tune::collect_metrics(position_final_regress_fit) %>% dplyr::filter(.data$.metric == "rmse") %>% dplyr::pull(".estimate"), 4), " rmse.")

  return(list("quali_pole" = pole_final, 'quali_pos' = position_final, 'quali_regression_pos' = position_regress_final))
}

#' @export
model_results_after_quali_xgb <- function(data = clean_data()){
  #As model_results_late - but with quali data
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- factor(ifelse(data$position == 1, 1, 0), levels = c(1,0))
  data$podium <- factor(ifelse(data$position <= 3, 1, 0), levels = c(1,0))
  data$t10 <- factor(ifelse(data$position <= 10, 1, 0), levels = c(1,0))

  data$finished <- factor(data$finished, levels = c(1,0))
  data$position <- as.factor(data$position)
  data$round_id <- as.factor(data$round_id)

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", 'driver_avg_qgap',
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg","driver_practice_optimal_rank_avg",
                  "practice_avg_rank", "practice_best_rank", "practice_optimal_rank", "practice_avg_gap", "practice_best_gap",
                  "q_min_perc", "q_avg_perc", "win", "podium", "t10", "finished", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  metrics_set <- yardstick::metric_set(yardstick::ppv, yardstick::npv, yardstick::spec, yardstick::sens,
                                       yardstick::mn_log_loss, yardstick::roc_auc, yardstick::accuracy)
  metrics_multi <- yardstick::metric_set(yardstick::kap, yardstick::mn_log_loss, yardstick::mcc, yardstick::accuracy, yardstick::roc_auc)

  # ---- Winner Model ----
  win_recipe <- recipes::recipe(win ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  win_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(), mtry = tune::tune(),
                                 learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  xgb_grid <- dials::grid_latin_hypercube(dials::tree_depth(), dials::min_n(), dials::loss_reduction(), sample_size = dials::sample_prop(), dials::finalize(dials::mtry(),
                                          train_data), dials::learn_rate(), dials::stop_iter(), size = 30)

  win_wflow <- workflows::workflow() %>%
    workflows::add_model(win_mod) %>%
    workflows::add_recipe(win_recipe)

  tictoc::tic("Trained Win Model")
  win_res <- win_wflow %>%
    tune::tune_grid(resamples = data_folds,
                    grid = xgb_grid,
                    metrics = metrics_set)

  win_best <- win_res %>%
    tune::select_best("ppv")
  tictoc::toc()

  win_final <- win_wflow %>%
    tune::finalize_workflow(win_best) %>%
    parsnip::fit(train_data)
  win_final_fit <- win_final %>%
    tune::last_fit(data_split, metrics = metrics_set)

  message("Win Model with ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 6), " npv, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Podium Model ----
  podium_recipe <- recipes::recipe(podium ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("win", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  podium_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  podium_wflow <- workflows::workflow() %>%
    workflows::add_model(podium_mod) %>%
    workflows::add_recipe(podium_recipe)

  tictoc::tic("Trained Podium Model")
  podium_res <- podium_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_set)

  podium_best <- podium_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  podium_final <- podium_wflow %>%
    tune::finalize_workflow(podium_best) %>%
    parsnip::fit(train_data)
  podium_final_fit <- podium_final %>%
    tune::last_fit(data_split, metrics = metrics_set)

  message("Podium Model with ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 6), " npv, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- T10 Model ----
  t10_recipe <- recipes::recipe(t10 ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  t10_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  t10_wflow <- workflows::workflow() %>%
    workflows::add_model(t10_mod) %>%
    workflows::add_recipe(t10_recipe)

  tictoc::tic("Trained T10 Model")
  t10_res <- t10_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_set)

  t10_best <- t10_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  t10_final <- t10_wflow %>%
    tune::finalize_workflow(t10_best) %>%
    parsnip::fit(train_data)
  t10_final_fit <- t10_final %>%
    tune::last_fit(data_split, metrics = metrics_set)

  message("T10 Model with ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 6), " npv, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Finish Model ----
  finish_recipe <- recipes::recipe(finished ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "t10", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  finish_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  finish_wflow <- workflows::workflow() %>%
    workflows::add_model(finish_mod) %>%
    workflows::add_recipe(finish_recipe)

  tictoc::tic("Trained Finishing Model")
  finish_res <- finish_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_set)

  finish_best <- finish_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  finish_final <- finish_wflow %>%
    tune::finalize_workflow(finish_best) %>%
    parsnip::fit(train_data)
  finish_final_fit <- finish_final %>%
    tune::last_fit(data_split, metrics = metrics_set)

  message("Finishing Model with ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "ppv") %>% dplyr::pull(".estimate"), 4), " ppv, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "sens") %>% dplyr::pull(".estimate"), 4), " sens, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "spec") %>% dplyr::pull(".estimate"), 4), " spec, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "npv") %>% dplyr::pull(".estimate"), 6), " npv, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Position Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(position = as.factor(.data$position), round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg",
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg",
                  "practice_avg_rank", "practice_best_rank", "practice_optimal_rank",
                  "q_min_perc", "q_avg_perc", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note: multinomial regression
  position_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_multi)

  position_best <- position_res %>%
    tune::select_best("kappa")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best) %>%
    parsnip::fit(train_data)
  position_final_fit <- position_final %>%
    tune::last_fit(data_split, metrics = metrics_multi)

  message("Position Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  return(list("win" = win_final, "podium" = podium_final, "t10" = t10_final, 'finish' = finish_final, 'position' = position_final))

}

#' @export
model_results_late_xgb <- function(data = clean_data()){
  #As model_results_early - but with practice data
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- factor(ifelse(data$position == 1, 1, 0), levels = c(1,0))
  data$podium <- factor(ifelse(data$position <= 3, 1, 0), levels = c(1,0))
  data$t10 <- factor(ifelse(data$position <= 10, 1, 0), levels = c(1,0))

  data$finished <- factor(data$finished, levels = c(1,0))
  data$position <- as.factor(data$position)
  data$round_id <- as.factor(data$round_id)

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", 'driver_avg_qgap',
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg","driver_practice_optimal_rank_avg",
                  "practice_avg_rank", "practice_best_rank", "practice_optimal_rank", "practice_avg_gap", "practice_best_gap",
                  "win", "podium", "t10", "finished", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  metrics_multi <- yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::mcc, yardstick::mn_log_loss,
                                         yardstick::roc_auc)
  metrics_binary <- yardstick::metric_set(yardstick::accuracy, yardstick::mn_log_loss, yardstick::roc_auc)

  # ---- Winner Model ----
  win_recipe <- recipes::recipe(win ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  win_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(), mtry = tune::tune(),
                                 learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  xgb_grid <- dials::grid_latin_hypercube(dials::tree_depth(), dials::min_n(), dials::loss_reduction(), sample_size = dials::sample_prop(), dials::finalize(dials::mtry(),
                                                                                                                                                            train_data), dials::learn_rate(), dials::stop_iter(), size = 30)

  win_wflow <- workflows::workflow() %>%
    workflows::add_model(win_mod) %>%
    workflows::add_recipe(win_recipe)

  tictoc::tic("Trained Win Model")
  win_res <- win_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = yardstick::metric_set(pacc, yardstick::mn_log_loss, yardstick::roc_auc))

  win_best <- win_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  win_final <- win_wflow %>%
    tune::finalize_workflow(win_best) %>%
    parsnip::fit(train_data)
  win_final_fit <- win_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Win Model with ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Podium Model ----
  podium_recipe <- recipes::recipe(podium ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("win", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  podium_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  podium_wflow <- workflows::workflow() %>%
    workflows::add_model(podium_mod) %>%
    workflows::add_recipe(podium_recipe)

  tictoc::tic("Trained Podium Model")
  podium_res <- podium_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  podium_best <- podium_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  podium_final <- podium_wflow %>%
    tune::finalize_workflow(podium_best) %>%
    parsnip::fit(train_data)
  podium_final_fit <- podium_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Podium Model with ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")


  # ---- T10 Model ----
  t10_recipe <- recipes::recipe(t10 ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  t10_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  t10_wflow <- workflows::workflow() %>%
    workflows::add_model(t10_mod) %>%
    workflows::add_recipe(t10_recipe)

  tictoc::tic("Trained T10 Model")
  t10_res <- t10_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  t10_best <- t10_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  t10_final <- t10_wflow %>%
    tune::finalize_workflow(t10_best) %>%
    parsnip::fit(train_data)
  t10_final_fit <- t10_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("T10 Model with ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Finish Model ----
  finish_recipe <- recipes::recipe(finished ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "t10", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  finish_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  finish_wflow <- workflows::workflow() %>%
    workflows::add_model(finish_mod) %>%
    workflows::add_recipe(finish_recipe)

  tictoc::tic("Trained Finishing Model")
  finish_res <- finish_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  finish_best <- finish_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  finish_final <- finish_wflow %>%
    tune::finalize_workflow(finish_best) %>%
    parsnip::fit(train_data)
  finish_final_fit <- finish_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Finishing Model with ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Position Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(position = as.factor(.data$position), round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg",
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg",
                  "practice_avg_rank", "practice_best_rank", "practice_optimal_rank", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note: multinomial regression
  position_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_multi)

  position_best <- position_res %>%
    tune::select_best("kappa")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best) %>%
    parsnip::fit(train_data)
  position_final_fit <- position_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Position Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  return(list("win" = win_final, "podium" = podium_final, "t10" = t10_final, 'finish' = finish_final, 'position' = position_final))

}

#' @export
model_results_early_xgb <- function(data = clean_data()) {
  #Doesn't include practice data - predictions could be with grid predicted (early in week) or actual
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- factor(ifelse(data$position == 1, 1, 0), levels = c(1,0))
  data$podium <- factor(ifelse(data$position <= 3, 1, 0), levels = c(1,0))
  data$t10 <- factor(ifelse(data$position <= 10, 1, 0), levels = c(1,0))

  data$finished <- factor(data$finished, levels = c(1,0))
  data$position <- as.factor(data$position)
  data$round_id <- as.factor(data$round_id)

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", 'driver_avg_qgap',
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg","driver_practice_optimal_rank_avg",
                  "win", "podium", "t10", "finished", "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  metrics_multi <- yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::mcc, yardstick::mn_log_loss,
                                         yardstick::roc_auc)
  metrics_binary <- yardstick::metric_set(yardstick::accuracy, yardstick::mn_log_loss, yardstick::roc_auc)

  # ---- Winner Model ----
  win_recipe <- recipes::recipe(win ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  win_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(), mtry = tune::tune(),
    learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  xgb_grid <- dials::grid_latin_hypercube(dials::tree_depth(), dials::min_n(), dials::loss_reduction(), sample_size = dials::sample_prop(), dials::finalize(dials::mtry(),
    train_data), dials::learn_rate(), dials::stop_iter(), size = 30)

  win_wflow <- workflows::workflow() %>%
    workflows::add_model(win_mod) %>%
    workflows::add_recipe(win_recipe)

  tictoc::tic("Trained Win Model")
  win_res <- win_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = yardstick::metric_set(pacc, yardstick::mn_log_loss, yardstick::roc_auc))

  win_best <- win_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  win_final <- win_wflow %>%
    tune::finalize_workflow(win_best) %>%
    parsnip::fit(train_data)
  win_final_fit <- win_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Win Model with ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(win_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Podium Model ----
  podium_recipe <- recipes::recipe(podium ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("win", "t10", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  podium_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  podium_wflow <- workflows::workflow() %>%
    workflows::add_model(podium_mod) %>%
    workflows::add_recipe(podium_recipe)

  tictoc::tic("Trained Podium Model")
  podium_res <- podium_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  podium_best <- podium_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  podium_final <- podium_wflow %>%
    tune::finalize_workflow(podium_best) %>%
    parsnip::fit(train_data)
  podium_final_fit <- podium_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Podium Model with ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(podium_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")


  # ---- T10 Model ----
  t10_recipe <- recipes::recipe(t10 ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "finished", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  t10_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  t10_wflow <- workflows::workflow() %>%
    workflows::add_model(t10_mod) %>%
    workflows::add_recipe(t10_recipe)

  tictoc::tic("Trained T10 Model")
  t10_res <- t10_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  t10_best <- t10_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  t10_final <- t10_wflow %>%
    tune::finalize_workflow(t10_best) %>%
    parsnip::fit(train_data)
  t10_final_fit <- t10_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("T10 Model with ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(t10_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Finish Model ----
  finish_recipe <- recipes::recipe(finished ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_rm("podium", "win", "t10", "position") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  finish_mod <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  finish_wflow <- workflows::workflow() %>%
    workflows::add_model(finish_mod) %>%
    workflows::add_recipe(finish_recipe)

  tictoc::tic("Trained Finishing Model")
  finish_res <- finish_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = glmnet_grid, metrics = metrics_binary)

  finish_best <- finish_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  finish_final <- finish_wflow %>%
    tune::finalize_workflow(finish_best) %>%
    parsnip::fit(train_data)
  finish_final_fit <- finish_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Finishing Model with ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(finish_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Position Model ----
  data <- p_mod_data %>%
    dplyr::filter(.data$position <= 20) %>%
    dplyr::mutate(position = as.factor(.data$position), round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg",
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg",
                  "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  # Note: multinomial regression
  position_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
    mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", nthread = 4)

  position_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Position Model")
  position_res <- position_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_multi)

  position_best <- position_res %>%
    tune::select_best("kappa")
  tictoc::toc(log = T)

  position_final <- position_wflow %>%
    tune::finalize_workflow(position_best) %>%
    parsnip::fit(train_data)
  position_final_fit <- position_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Position Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  return(list("win" = win_final, "podium" = podium_final, "t10" = t10_final, 'finish' = finish_final, 'position' = position_final))

}
