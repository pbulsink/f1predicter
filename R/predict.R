
generate_new_data<-function(season, race, drivers, historical_data = clean_data()){
  #drivers should be a data.frame or tibble with:
  #driverId, constructorId, (optional any of: quali_position, grid, practice_optimal_rank, practice_best_rank, practice_avg_rank)
  #Any missing race-specific values will be given as current moved averages
  #Any other values will be calculated from historical data, presuming this is next race




  dplyr::select('driverId', 'constructorId', 'grid', 'quali_position', 'driver_experience',
                'driver_failure_avg', 'constructor_grid_avg', 'constructor_finish_avg', 'constructor_failure_avg',
                'driver_grid_avg', 'driver_position_avg', 'driver_finish_avg', 'grid_pos_corr_avg',
                'driver_failure_circuit_avg', 'constructor_failure_circuit_avg', 'driver_practice_optimal_rank_avg',
                'practice_avg_rank', 'practice_best_rank', 'practice_optimal_rank', 'season', 'race', 'raceId')

}


# Predict a winner
predict_winner <- function(new_data = generate_new_data(), win_model){
  preds <- new_data %>%
    dplyr::mutate('win_odd' = win_model %>% predict(new_data, type = 'prob')$.pred_1) %>%
    dplyr::select('driverId', 'race', 'season', 'win_odd')
  return(preds)
}

# Predict a podium
predict_podium <- function(new_data = generate_new_data(), podium_model){
  preds <- new_data %>%
    dplyr::mutate('podium_odd' = podium_model %>% predict(new_data, type = 'prob')$.pred_1) %>%
    dplyr::select('driverId', 'race', 'season', 'podium_odd')
  return(preds)
}

# Predict T10
predict_t10 <- function(new_data = generate_new_data(), t10_model){
  preds <- new_data %>%
    dplyr::mutate('t10_odd' = t10_model %>% predict(new_data, type = 'prob')$.pred_1) %>%
    dplyr::select('driverId', 'race', 'season', 't10_odd')
  return(preds)
}

#Predict Finish
predict_finish <- function(new_data = generate_new_data(), finish_model){
  preds <- new_data %>%
    dplyr::mutate('finish_odd' = finish_model %>% predict(new_data, type = 'prob')$.pred_1) %>%
    dplyr::select('driverId', 'race', 'season', 'finish_odd')
  return(preds)
}

#Predict Position
predict_position <- function(new_data = generate_new_data(), position_model){
  preds <- new_data %>%
    dplyr::select('driverId', 'race', 'season') %>%
    dplyr::mutate('likely_position' = position_model %>% predict(new_data, type = 'class'),
                  'p_odd' = position_model %>% predict(new_data, type = 'class'))
  return(preds)
}

predict_race <- function(new_data = generate_new_data) {

}
