# Models

# 1: Who will get Pole
#    - Expected position of each person

# 2: Finish Position:
#    - 1st, top 3, top 10 (points), finish

model_qual_early <- function(driver_list = get_last_drivers(), data = clean_data()){
  # Model quali early in the week - before practices are done
  # grid => step_dummy


}

model_quali_late <- function(driver_list = get_last_drivers(), data = clean_data()){
  # Model quali late in the week - after practice sessions
  # grid => step_dummy

}

model_results <- function(grid, data = clean_data()){
  # ---- Common Data ----
  data <- data[data$season >= 2000,]
  # Model results given a grid - predicted or actual
  data$win <- ifelse(data$position == 1, 1, 0)
  data$podium <- ifelse(data$position <= 3, 1, 0)
  data$t10 <- ifelse(data$position <= 10, 1, 0)

  data$win <- as.factor(data$win)
  data$podium <- as.factor(data$podium)
  data$t10 <- as.factor(data$t10)
  data$finished <- as.factor(data$finished)
  data$position <- as.factor(data$position)

  data<-data %>%
    dplyr::select('driverId', 'constructorId', 'position', 'grid', 'quali_position', 'driver_experience',
                  'driver_failure_avg', 'constructor_failure_avg', 'driver_grid_avg', 'driver_position_avg',
                  'driver_finish_avg', 'win', 'podium', 't10', 'finished', 'season', 'race') %>%
    dplyr::mutate_if(is.character, as.factor)

  # Put 4/5 of the data into the training set
  data_split <- rsample::initial_split(data, prop = 4/5, strata = 'position')

  # Create data frames for the two sets:
  train_data <- rsample::training(data_split)
  test_data  <- rsample::testing(data_split)

  glmnet_grid <- dials::grid_regular(dials::penalty(), dials::mixture(),
                            levels = 5)

  data_folds <- rsample::vfold_cv(train_data)

  # ---- Winner Model ----
  win_recipe <- recipes::recipe(win ~ ., data = train_data) %>%
    recipes::update_role('season', 'race', new_role = "ID") %>%
    recipes::step_rm('podium', 't10', 'finished', 'position') %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  win_mod <- parsnip::logistic_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  win_wflow <-
    workflows::workflow() %>%
    workflows::add_model(win_mod) %>%
    workflows::add_recipe(win_recipe)

  win_res <-
    win_wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = glmnet_grid
    )

  win_best <- win_res %>%
    tune::select_best("accuracy")

  win_final <- win_wflow %>% tune::finalize_workflow(win_best)
  win_final_fit <- win_final %>% tune::last_fit(data_split)

  # ---- Podium Model ----
  podium_recipe <- recipes::recipe(podium ~ ., data = train_data) %>%
    recipes::update_role('season', 'race', new_role = "ID") %>%
    recipes::step_rm('win', 't10', 'finished', 'position') %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  podium_mod <- parsnip::logistic_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  podium_wflow <-
    workflows::workflow() %>%
    workflows::add_model(podium_mod) %>%
    workflows::add_recipe(podium_recipe)

  podium_res <-
    podium_wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = glmnet_grid
    )

  podium_best <- podium_res %>%
    tune::select_best("accuracy")

  podium_final <- podium_wflow %>% tune::finalize_workflow(podium_best)
  podium_final_fit <- podium_final %>% tune::last_fit(data_split)
  # ---- T10 Model ----
  t10_recipe <- recipes::recipe(t10 ~ ., data = train_data) %>%
    recipes::update_role('season', 'race', new_role = "ID") %>%
    recipes::step_rm('podium', 'win', 'finished', 'position') %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  t10_mod <- parsnip::logistic_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  wt10_wflow <-
    workflows::workflow() %>%
    workflows::add_model(t10_mod) %>%
    workflows::add_recipe(t10_recipe)

  t10_res <-
    t10_wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = glmnet_grid
    )

  t10_best <- t10_res %>%
    tune::select_best("accuracy")

  t10_final <- t10_wflow %>% tune::finalize_workflow(t10_best)
  t10_final_fit <- t10_final %>% tune::last_fit(data_split)

  # ---- Finish Model ----
  finish_recipe <- recipes::recipe(t10 ~ ., data = train_data) %>%
    recipes::update_role('season', 'race', new_role = "ID") %>%
    recipes::step_rm('podium', 'win', 't10', 'position') %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  finish_mod <- parsnip::logistic_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  finish_wflow <-
    workflows::workflow() %>%
    workflows::add_model(finish_mod) %>%
    workflows::add_recipe(finish_recipe)

  finish_res <-
    finish_wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = glmnet_grid
    )

  finish_best <- finish_res %>%
    tune::select_best("accuracy")

  finish_final <- finish_wflow %>% tune::finalize_workflow(finish_best)
  finish_final_fit <- finish_final %>% tune::last_fit(data_split)
  # ---- Position Model ----
  position_recipe <- recipes::recipe(position ~ ., data = train_data) %>%
    recipes::update_role('season', 'race', new_role = "ID") %>%
    recipes::step_rm('podium', 't10', 'finished', 'win') %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  #Note multinomial regression
  position_mod <- parsnip::multinom_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  position_wflow <-
    workflows::workflow() %>%
    workflows::add_model(position_mod) %>%
    workflows::add_recipe(position_recipe)

  position_res <-
    position_wflow %>%
    tune::tune_grid(
      resamples = data_folds,
      grid = glmnet_grid
    )

  position_best <- position_res %>%
    tune::select_best("accuracy")

  position_final <- position_wflow %>% tune::finalize_workflow(position_best)
  position_final_fit <- position_final %>% tune::last_fit(data_split)
}
