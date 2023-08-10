# Models

# 1: Who will get Pole - Expected position of each person

# 2: Finish Position: - 1st, top 3, top 10 (points), finish

model_quali_early <- function(data = clean_data()) {
  # Model quali early in the week - after practice sessions grid => step_dummy
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$pole <- ifelse(data$quali_position == 1, 1, 0)
  data$pole <- as.factor(data$pole)

  data$round_id <- as.factor(data$round_id)

  data <- data %>%
    dplyr::select("driver_id", "constructor_id", "quali_position", "driver_experience", "driver_failure_avg",
                  "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg", "driver_grid_avg",
                  "driver_position_avg", "driver_finish_avg", "driver_failure_circuit_avg", 'driver_avg_qgap',
                  "constructor_failure_circuit_avg","driver_practice_optimal_rank_avg", "pole", "season",
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
  metrics_binary <- yardstick::metric_set(yardstick::accuracy, yardstick::mn_log_loss, yardstick::roc_auc)

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

  tictoc::tic("Trained Pole Model")
  pole_res <- pole_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

  pole_best <- pole_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  pole_final <- pole_wflow %>%
    tune::finalize_workflow(pole_best) %>%
    parsnip::fit(train_data)
  pole_final_fit <- pole_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Pole Quali Model with ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Quali Position Model ----
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

  return(list("quali_pole" = pole_final, 'quali_pos' = position_final))
}

model_quali_late <- function(data = clean_data()) {

  # Model quali late in the week - after practices are done. grid => step_dummy
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$pole <- ifelse(data$quali_position == 1, 1, 0)
  data$pole <- as.factor(data$pole)

  data$round_id <- as.factor(data$round_id)

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
  metrics_binary <- yardstick::metric_set(yardstick::accuracy, yardstick::mn_log_loss, yardstick::roc_auc)

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

  tictoc::tic("Trained Pole Model")
  pole_res <- pole_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

  pole_best <- pole_res %>%
    tune::select_best("mn_log_loss")
  tictoc::toc()

  pole_final <- pole_wflow %>%
    tune::finalize_workflow(pole_best) %>%
    parsnip::fit(train_data)
  pole_final_fit <- pole_final %>%
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Pole Quali Model with ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(pole_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  # ---- Quali Position Model ----
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

  return(list("quali_pole" = pole_final, 'quali_pos' = position_final))
}

model_results_after_quali <- function(data = clean_data()){
  #As model_results_early - but with practice data
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- ifelse(data$position == 1, 1, 0)
  data$podium <- ifelse(data$position <= 3, 1, 0)
  data$t10 <- ifelse(data$position <= 10, 1, 0)

  data$win <- as.factor(data$win)
  data$podium <- as.factor(data$podium)
  data$t10 <- as.factor(data$t10)
  data$finished <- as.factor(data$finished)
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
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

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
    tune::last_fit(data_split, metrics = metrics_binary)

  message("Position Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mn_log_loss") %>% dplyr::pull(".estimate"), 4), " log loss, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "accuracy") %>% dplyr::pull(".estimate"), 4), " accuracy, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "kap") %>% dplyr::pull(".estimate"), 4), " kappa, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mcc") %>% dplyr::pull(".estimate"), 4), " mcc, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "roc_auc") %>% dplyr::pull(".estimate"), 4), " auc.")

  return(list("win" = win_final, "podium" = podium_final, "t10" = t10_final, 'finish' = finish_final, 'position' = position_final))

}

model_results_late <- function(data = clean_data()){
  #As model_results_early - but with practice data
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- ifelse(data$position == 1, 1, 0)
  data$podium <- ifelse(data$position <= 3, 1, 0)
  data$t10 <- ifelse(data$position <= 10, 1, 0)

  data$win <- as.factor(data$win)
  data$podium <- as.factor(data$podium)
  data$t10 <- as.factor(data$t10)
  data$finished <- as.factor(data$finished)
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
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

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

model_results_early <- function(data = clean_data()) {
  #Doesn't include practice data - predictions could be with grid predicted (early in week) or actual
  # ---- Common Data ----
  data <- data[data$season >= 2018, ]
  p_mod_data <- data  # Used later
  # Model results given a grid - predicted or actual
  data$win <- ifelse(data$position == 1, 1, 0)
  data$podium <- ifelse(data$position <= 3, 1, 0)
  data$t10 <- ifelse(data$position <= 10, 1, 0)

  data$win <- as.factor(data$win)
  data$podium <- as.factor(data$podium)
  data$t10 <- as.factor(data$t10)
  data$finished <- as.factor(data$finished)
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
    tune::tune_grid(resamples = data_folds, grid = xgb_grid, metrics = metrics_binary)

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

model_quali_gap_early <- function(data = clean_data()) {
  # Model quali early in the week - before practice sessions - use quali gap to sort grid
  data <- data[data$season >= 2018, ]

  # ---- Quali Gap Model ----
  data <- data %>%
    dplyr::mutate(round_id = as.factor(.data$round_id)) %>%
    dplyr::select("driver_id", "constructor_id", "position", "grid", "quali_position", "driver_experience",
                  "driver_failure_avg", "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg",
                  "driver_grid_avg", "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", 'driver_avg_qgap',
                  "driver_failure_circuit_avg", "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg",
                  "season", "round", "round_id") %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::group_initial_split(data, prop = 4/5, group = "round_id")

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  data_folds <- rsample::group_vfold_cv(data = train_data, group = "round_id")

  qgap_recipe <- recipes::recipe(qgap ~ ., data = train_data) %>%
    recipes::update_role("season", "round", "round_id", "driver_id", "constructor_id", new_role = "ID") %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  qgap_mod <- parsnip::boost_tree(trees = 1000, tree_depth = tune::tune(), min_n = tune::tune(), loss_reduction = tune::tune(), sample_size = tune::tune(),
                                      mtry = tune::tune(), learn_rate = tune::tune(), stop_iter = tune::tune()) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost", nthread = 2)

  qgap_wflow <- workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  tictoc::tic("Trained Quali Gap Model")
  qgap_res <- qgap_wflow %>%
    tune::tune_grid(resamples = data_folds, grid = xgb_grid)

  qgap_best <- qgap_res %>%
    tune::select_best("rmse")
  tictoc::toc(log = T)

  qgap_final <- qgap_wflow %>%
    tune::finalize_workflow(qgap_best) %>%
    parsnip::fit(train_data)
  qgap_final_fit <- qgap_final %>%
    tune::last_fit(data_split)

  message("Quali Gap Model with ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "rmse") %>% dplyr::pull(".estimate"), 4), " rmse, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "rsq") %>% dplyr::pull(".estimate"), 4), " rsq, ",
          round(tune::collect_metrics(position_final_fit) %>% dplyr::filter(.data$.metric == "mae") %>% dplyr::pull(".estimate"), 4), " mae.")

  return(list('quali_qgap' = position_final))
}
