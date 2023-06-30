globals<-list('grid' = 18,
              'position' = 18,
              'driver_finish_avg' = .86,
              'driver_failure_avg' = 0.0555,
              'constructor_failure_avg' = 0.0756)

generate_new_data <- function(season, round, drivers=NULL, historical_data = clean_data()) {
  # drivers should be a data.frame or tibble with: driver_id, constructor_id, (optional any of: quali_position, grid, practice_optimal_rank, practice_best_rank,
  # practice_avg_rank) Any missing round-specific values will be given as current moved averages Any other values will be calculated from historical data,
  # presuming this is next round

  if(is.null(drivers)){
    drivers<-get_last_drivers()
  }
  new_data<-tibble::as_tibble(drivers)

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
    dplyr::ungroup() %>%
    unique()

  if(nrow(hd_driver) != nrow(drivers)){
    # Handle having a new driver around
  }
  if(!(season %in% colnames(new_data))){
    new_data$season<-season
  }
  if(!(round %in% colnames(new_data))){
    new_data$round<-round
  }
  if(!(round_id %in% colnames(new_data))){
    new_data$round_id<-paste0(new_data$season, "-", new_data$round)
  }
  if(!(grid) %in% colnames(new_data)){
    # sort drivers by their average grid for an estimate
    new_data$grid <-
  }
  #if(!)





  dplyr::select("driver_id", "constructor_id", "grid", "quali_position", "driver_experience", "driver_failure_avg",
                "constructor_grid_avg", "constructor_finish_avg", "constructor_failure_avg", "driver_grid_avg",
                "driver_position_avg", "driver_finish_avg", "grid_pos_corr_avg", "driver_failure_circuit_avg",
                "constructor_failure_circuit_avg", "driver_practice_optimal_rank_avg", "practice_avg_rank",
                "practice_best_rank", "practice_optimal_rank", "season", "round", "round_id")

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
