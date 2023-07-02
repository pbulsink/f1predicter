

generate_new_data <- function(season, round, drivers=NULL, historical_data = clean_data()) {
  globals<-list('grid' = 18,
                'position' = 18,
                'driver_finish_avg' = .86,
                'driver_failure_avg' = 0.0555,
                'constructor_failure_avg' = 0.0756,
                'grid_pos_corr' = 0.5284)
  # drivers should be a data.frame or tibble with: driver_id, constructor_id, (optional any of: quali_position, grid, practice_optimal_rank, practice_best_rank,
  # practice_avg_rank) Any missing round-specific values will be given as current moved averages Any other values will be calculated from historical data,
  # presuming this is next round

  stopifnot(nrow(schedule[schedule$season==season & schedule$round == round, ])>0)

  if(is.null(drivers)){
    drivers<-get_last_drivers()
  }

  new_data<-tibble::as_tibble(drivers) %>%
    dplyr::mutate(season = season,
                  round = round,
                  round_id = paste0(season, "-", round),
                  circuit_id = schedule[schedule$season==season & schedule$round == round, ]$circuit_id)

  hd_driver <- historical_data %>%
    dplyr::filter(.data$driver_id %in% new_data$driver_id) %>%
    dplyr::group_by(.data$driver_id) %>%
    dplyr::filter(.data$season == max(.data$season)) %>%
    dplyr::filter(.data$round == max(.data$round)) %>%
    dplyr::ungroup() %>%
    unique()

  hd_constructor <- historical_data %>%
    dplyr::filter(.data$constructor_id %in% new_data$constructor_id) %>%
    dplyr::group_by(.data$constructor_id) %>%
    dplyr::filter(.data$season == max(.data$season)) %>%
    dplyr::filter(.data$round == max(.data$round)) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~mean(.x, na.rm=TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      constructor_grid_avg = tidyr::replace_na(.data$constructor_grid_avg, globals$grid),
      grid = tidyr::replace(.data$grid, globals$grid),
      constructor_finish_avg = tidyr::replace_na(.data$constructor_finish_avg, 1-globals$constructor_failure_avg),
      finished = tidyr::replace_na(.data$finished, 1-globals$constructor_failure_avg),
      constructor_failure_avg = tidyr::replace_na(.data$constructor_failure_avg, globals$constructor_failure_avg),
      constructor_failure_race = tidyr::replace_na(.data$constructor_failure_race, globals$constructor_failure_avg),
      constructor_grid_avg = wmean_two(.data$grid, .data$constructor_grid_avg, 20),
      constructor_failure_avg = wmean_two(.data$constructor_failure_race, .data$constructor_failure_avg, 20),
      constructor_finish_avg = wmean_two(.data$finished, .data$constructor_finish_avg, 20)
    ) %>%
    unique()

  hd_circuit <- historical_data %>%
    dplyr::filter(.data$circuit_id %in% new_data$circuit_id)
  if(nrow(hd_circuit) > 0){
    hd_circuit <- hd_circuit %>%
      dplyr::group_by(.data$circuit_id) %>%
      dplyr::filter(.data$season == max(.data$season)) %>%
      dplyr::filter(.data$round == max(.data$round)) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~mean(.x, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(grid_pos_corr_avg = tidyr::replace_na(.data$grid_pos_corr_avg, globals$grid_pos_corr_avg),
                    grid_pos_corr = tidyr::replace_na(.data$grid_pos_corr, globals$grid_pos_corr_avg),
                    driver_failure_circuit_avg = tidyr::replace_na(.data$driver_failure_circuit_avg, globals$driver_failure_avg),
                    driver_failure_circuit = tidyr::replace_na(.data$driver_failure_circuit, globals$driver_failure_avg),
                    constructor_failure_circuit_avg = tidyr::replace_na(.data$constructor_failure_circuit_avg, globals$constructor_failure_avg),
                    constructor_failure_circuit = tidyr::replace_na(.data$constructor_failure_circuit, globals$constructor_failure_avg),
                    grid_pos_corr_avg = wmean_two(.data$grid_pos_corr, .data$grid_pos_corr_avg, 5),
                    driver_failure_circuit_avg = wmean_two(.data$driver_failure_circuit, .data$driver_failure_circuit_avg, 5),
                    constructor_failure_circuit_avg = wmean_two(.data$constructor_failure_circuit, .data$constructor_failure_circuit_avg, 5)) %>%
      unique()

      new_data$grid_pos_corr_avg + corr
      new_data$driver_failure_circuit_avg
      new_data$constructor_failure_circuit_avg
  } else {
    #Need to make a new version - use defaults
    hd_circuit <- tibble::tibble('circuit_id' = new_data$circuit_id,
                                 'grid_pos_corr_avg' = globals$grid_pos_corr_avg,
                                 'driver_failure_circuit_avg' = globals$driver_failure_avg,
                                 'constructor_failure_circuit_avg' = globals$constructor_failure_avg
    )
  }

  # Fill in missing columns
  if(!('season' %in% colnames(new_data))){
    new_data$season<-season
  }
  if(!('round' %in% colnames(new_data))){
    new_data$round<-round
  }
  if(!('round_id' %in% colnames(new_data))){
    new_data$round_id<-paste0(new_data$season, "-", new_data$round)
  }
  if(!('grid' %in% colnames(new_data))){
    # sort drivers by their average grid for an estimate
    new_data <- newdata %>%
      dplyr::left_bind(new_data, hd_driver[,c('driver_id', 'driver_grid_avg', 'grid'), by = 'driver_id']) %>%
      dplyr::rename('last_grid' = 'grid') %>%
      dplyr::mutate(driver_grid_avg = tidyr::replace_na(.data$driver_grid_avg, globals$grid),
                    last_grid = tidyr::replace_na(.data$grid, globals$grid),
                    driver_grid_avg = wmean_two(.data$driver_grid_avg, .data$last_grid, 10),
                    grid = order(order(.data$driver_grid_avg)))
  } else {
    new_data <- newdata %>%
      dplyr::left_bind(new_data, hd_driver[,c('driver_id', 'driver_grid_avg', 'grid'), by = 'driver_id']) %>%
      dplyr::rename('grid' = 'grid.x', 'last_grid' = 'grid.y') %>%
      dplyr::mutate(driver_grid_avg = tidyr::replace_na(.data$driver_grid_avg, globals$grid),
                    last_grid = tidyr::replace_na(.data$last_grid, globals$grid),
                    driver_grid_avg = wmean_two(.data$driver_grid_avg, .data$last_grid, 10))
  }

  if(!('quali_position' %in% colnames(new_data))){
    new_data$quali_position <- new_data$grid
  }

  #Load driver/constructor/circuit info
  new_data <- new_data %>%
    dplyr::left_join(hd_driver[,c('driver_id', 'driver_experience', 'driver_failure', 'driver_failure_avg',
                                  'position', 'driver_position_avg', 'finished', 'driver_finish_avg')], by = 'driver_id') %>%
    dplyr::mutate(driver_experience = tidyr::replace_na(.driver_experience, -1),
                  driver_experience = .data$driver_experience + 1,
                  driver_failure_avg = tidyr::replace_na(.data$driver_failure_avg, globals$driver_failure_avg),
                  driver_failure = tidyr::replace_na(.data$driver_failure, globals$driver_failure_avg),
                  driver_failure_avg = wmean_two(.data$driver_failure_avg, .data$driver_Failure, 20),
                  position = tidyr::replace_na(.data$position, globals$position),
                  driver_position_avg = tidyr::replace_na(.data$driver_position_avg, globals$position),
                  driver_position_avg = wmean_two(.data$position, .data$driver_position_avg, 10),
                  driver_finish_avg = tidyr::replace_na(.data$driver_finish_avg, globals$driver_finish_avg),
                  finished = tidyr::replace_na(.data$finished, globals$driver_finish_avg, 10),
                  driver_finish_avg = wmean_two(.data$finished, .data$driver_finish_avg, 10)) %>%
    dplyr::left_join(hd_constructor[,c('constructor_id', 'constructor_grid_avg', 'constructor_finish_avg',
                                       'constructor_failure_avg')], by = 'constructor_id') %>%
    dplyr::left_join(hd_circuit[,c('circuit_id', 'grid_pos_corr_avg', 'driver_failure_circuit_avg',
                                   'constructor_failure_circuit_avg')], by = circuit_id)


  #Optionally included practice data
  if(!("driver_practice_optimal_rank_avg" %in% colnames(new_data))){
    new_data$driver_practice_optimal_rank_avg <- NA
  }
  if(!("practice_avg_rank" %in% colnames(new_data))){
    new_data$practice_avg_rank <- NA
  }
  if(!("practice_best_rank" %in% colnames(new_data))){
    new_data$practice_best_rank <- NA
  }
  if(!("practice_optimal_rank" %in% colnames(new_data))){
    new_data$practice_optimal_rank <- NA
  }




  new_data <- new_data %>%
    dplyr::select("driver_id", "constructor_id", "grid", "quali_position", "driver_experience", "driver_failure_avg",
                  "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg", "driver_grid_avg",
                  "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", "driver_failure_circuit_avg",
                  "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg", "practice_avg_rank",
                  "practice_best_rank", "practice_optimal_rank", "season", "round", "round_id") %>%
    unique()

  return(new_data)

}


# Predict a winner
predict_winner <- function(new_data = generate_new_data(), win_model) {
  preds <- new_data %>%
    dplyr::mutate(win_odd = win_model %>%
      stats::predict(new_data, type = "prob")$.pred_1) %>%
    dplyr::select("driver_id", "round", "season", "win_odd")
  return(preds)
}

# Predict a podium
predict_podium <- function(new_data = generate_new_data(), podium_model) {
  preds <- new_data %>%
    dplyr::mutate(podium_odd = podium_model %>%
      stats::predict(new_data, type = "prob")$.pred_1) %>%
    dplyr::select("driver_id", "round", "season", "podium_odd")
  return(preds)
}

# Predict T10
predict_t10 <- function(new_data = generate_new_data(), t10_model) {
  preds <- new_data %>%
    dplyr::mutate(t10_odd = t10_model %>%
      stats::predict(new_data, type = "prob")$.pred_1) %>%
    dplyr::select("driver_id", "round", "season", "t10_odd")
  return(preds)
}

# Predict Finish
predict_finish <- function(new_data = generate_new_data(), finish_model) {
  preds <- new_data %>%
    dplyr::mutate(finish_odd = finish_model %>%
      stats::predict(new_data, type = "prob")$.pred_1) %>%
    dplyr::select("driver_id", "round", "season", "finish_odd")
  return(preds)
}

# Predict Position
predict_position <- function(new_data = generate_new_data(), position_model) {
  preds <- new_data %>%
    dplyr::select("driver_id", "round", "season") %>%
    dplyr::mutate(likely_position = position_model %>%
      stats::predict(new_data, type = "class"), p_odd = position_model %>%
      stats::predict(new_data, type = "class"))
  return(preds)
}

predict_round <- function(new_data = generate_new_data) {

}
