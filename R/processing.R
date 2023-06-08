# Data Processing Functions



#' Clean Data
#'
#' @param input take an input data (or load fresh) and process it for modelling
#'
#' @return a data frame
#' @export
clean_data <- function(input=load_all_data()){
  # Results ----
  rgrid<-input$rgrid
  rg2<-input$rgrid
  rg2$quali_position <- rg2$position
  rg2$driverId <- rg2$qualiResults
  results<-input$results %>%
    dplyr::mutate('pos_change' = .data$grid - .data$position) %>%
    dplyr::group_by(.data$season, .data$driverId) %>%
    dplyr::mutate('points_after' = cumsum(.data$points),
                  'points_before' = .data$points_after - .data$points) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate('driver_experience' = 0:(dplyr::n()-1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$race) %>%
    dplyr::mutate('pos_change_perc' = dplyr::if_else(.data$pos_change >= 0,
                                                     .data$pos_change/.data$grid,
                                                     .data$pos_change/(dplyr::n()-.data$grid)),
                  'grid' = dplyr::if_else(.data$grid != 0, .data$grid, dplyr::n()),
                  'modern_points' = expand_val(c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1), dplyr::n(), 0),
                  'grid_points' = expand_val(c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1), dplyr::n(), 0)[.data$grid],
                  'pos_change_points' = .data$modern_points - .data$grid_points) %>% # should get the points as a vector and pull the nth element
    dplyr::ungroup() %>%
    dplyr::mutate('finished' = dplyr::if_else(grepl("Finished|\\+(\\d+) Lap", .data$status), 1, 0),
                  'driver_failure' = dplyr::if_else(
                    .data$status %in% c('Damage', 'Excluded', 'Illness', 'Injury', 'Collision damage', 'Did not qualify',
                                        'Disqualified', 'Not classified', 'Injured', 'Spun off', 'Accident', 'Collision'),1,0),
                  'constructor_failure' = dplyr::if_else(
                    .data$status %in% c('Gearbox', 'Suspension', 'Fuel pressure', 'Radiator', 'Mechanical', 'Engine',
                                        'Electrical', 'Fuel system', 'Driveshaft', 'Oil line', 'Fuel rig', 'Withdrew',
                                        'Launch control', 'Clutch', 'Hydraulics', 'Brakes', 'Electronics', 'Fuel',
                                        'Out of fuel', 'Oil leak', 'Tyre', 'Transmission', 'Pneumatics', 'Steering',
                                        'Throttle', 'Handling', 'Puncture', 'Rear wing', 'Fire', 'Wheel', 'Overheating',
                                        'Wheel rim', 'Engine fire', 'Retired', 'Tyre puncture', 'Wheel nut', 'Broken wing',
                                        'Heat shield fire', 'Exhaust', 'Technical', 'Water leak', 'Fuel pump', 'Track rod',
                                        'Oil pressure', 'Front wing', 'Water pressure', 'Refuelling', 'Driver Seat',
                                        'Differential', 'Engine misfire', 'Vibrations', 'Alternator', 'ERS', 'Power Unit',
                                        'Turbo', 'Drivetrain', 'Power loss', 'Brake duct', 'Battery', 'Seat', 'Spark plugs',
                                        'Debris', 'Cooling system', 'Water pump', 'Fuel leak', 'Undertray'),1,0)) %>%
    dplyr::left_join(rg2[,c('quali_position', 'driverId', 'season', 'race')], by = c('driverId', 'season', 'race')) %>%
    dplyr::select('driverId', 'constructorId', 'position', 'grid', 'quali_position', 'pos_change', 'pos_change_perc',
                  'pos_change_points', 'fastest_rank', 'fastest_time' = 'time_sec', 'points', 'points_before',
                  'points_after', 'driver_experience', 'season', 'race', 'driver_failure', 'constructor_failure', 'finished')


  # laps ----
  laps <- input$laps %>%
    dplyr::filter(.data$deleted != TRUE) %>%
    dplyr::group_by(.data$season, .data$race, .data$sessionType, .data$driverId) %>%
    dplyr::mutate('best_time' = min(.data$lapTime, na.rm = T),
                  'num_laps' = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$race, .data$sessionType) %>%
    dplyr::mutate('gap_to_best' = .data$best_time - min(.data$best_time, na.rm = T),
                  'perc_to_best' = .data$best_time/min(.data$best_time, na.rm = T),
                  'perc_num_laps' = .data$num_laps/max(.data$num_laps, na.rm = T),
                  'rank' = dplyr::dense_rank(.data$best_time)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'constructorId', 'season', 'race', 'session_type' = 'sessionType', 'best_time', 'rank',
                  'num_laps', 'gap_to_best', 'perc_to_best', 'perc_num_laps')  %>%
    unique()

  # practices ----
  practices <- laps %>%
    dplyr::filter(.data$session_type %in% c('FP1', 'FP2', 'FP3')) %>%
    dplyr::group_by(.data$season, .data$race, .data$driverId) %>%
    dplyr::mutate('practice_avg_rank' = mean(.data$rank, na.rm = T),
                  'practice_best_rank' = min(.data$rank, na.rm = T),
                  'practice_num_laps' = sum(.data$num_laps, na.rm = T),
                  'practice_avg_gap' = mean(.data$gap_to_best, na.rm = T),
                  'practice_best_gap' = min(.data$gap_to_best, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'constructorId', 'season', 'race', 'practice_avg_rank', 'practice_best_rank',
                  'practice_num_laps', 'practice_avg_gap', 'practice_best_gap') %>%
    unique()

  # qualis ----
  qualis<-input$qualis %>%
    dplyr::select('driverId', 'Q1_sec', 'Q2_sec', 'Q3_sec', 'season', 'race') %>%
    dplyr::group_by(.data$season, .data$race) %>%
    dplyr::mutate('Q1_perc' = .data$Q1_sec/min(.data$Q1_sec, na.rm = T),
                  'Q2_perc' = .data$Q2_sec/min(.data$Q2_sec, na.rm = T),
                  'Q3_perc' = .data$Q3_sec/min(.data$Q3_sec, na.rm = T),
                  'Q_min_perc' = NA_real_) %>%
    dplyr::rowwise() %>%
    dplyr::mutate('Q_min_perc' = min(dplyr::c_across(c('Q1_perc','Q2_perc', 'Q3_perc')), na.rm = T),
                  'Q_avg_perc' = mean(dplyr::c_across(c('Q1_perc', 'Q2_perc', 'Q3_perc')), na.rm = T)) %>%
    dplyr::ungroup() %>%
    unique()

  # pitstops ----
  pitstops <- input$pitstops %>%
    dplyr::select('driverId', 'stop', 'duration', 'season', 'race') %>%
    dplyr::group_by(.data$season, .data$race, .data$driverId) %>%
    dplyr::mutate('stops' = dplyr::n()) %>%
    dplyr::ungroup(.data$driverId) %>%
    # Set the fastest stop to 2.0 s to get a better est of team comparisons
    dplyr::mutate('adj_duration' = .data$duration - (min(.data$duration, na.rm = T))+2,
                  'pit_duration_perc' = .data$adj_duration/min(.data$adj_duration, na.rm = T)) %>%
    dplyr::select('driverId', 'pit_stops' = 'stops', 'pit_duration' = 'adj_duration', 'pit_duration_perc', 'season', 'race') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$race, .data$driverId) %>%
    dplyr::mutate('pit_duration_avg' = mean(.data$pit_duration, na.rm=TRUE)) %>%
    dplyr::ungroup(.data$driverId) %>%
    dplyr::mutate('pit_duration_avg_perc' = .data$pit_duration_avg/min(.data$pit_duration)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'pit_duration_avg_perc', 'pit_stops') %>%
    unique() %>%
    dplyr::group_by(.data$season, .data$race) %>%
    dplyr::mutate('pit_num_avg_perc' = .data$pit_stops/mean(.data$pit_stops, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'pit_duration_avg_perc', 'pit_num_avg_perc') %>%
    unique()

  results <- results %>%
    dplyr::left_join(qualis, by = c('race', 'season', 'driverId')) %>%
    dplyr::left_join(practices, by = c('race', 'season', 'driverId')) %>%
    dplyr::left_join(pitstops, by = c('race', 'season', 'driverId')) %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate('driver_pos_change_avg' = lagged_cumwmean_expanded(.data$pos_change),
                  'driver_points_change_avg' = lagged_cumwmean_expanded(.data$pos_change_points),
                  'driver_failure_avg' = lagged_cumwmean_expanded(.data$driver_failure, val = 0.0555),
                  'driver_grid_avg' = lagged_cumwmean_expanded(.data$grid, val = 15),
                  'driver_position_avg' = lagged_cumwmean_expanded(.data$position, val = 15),
                  'driver_finish_avg' = lagged_cumwmean_expanded(.data$finished, val = 0.1311),
                  'const_failure_avg' = 1-.data$driver_finish_avg - .data$driver_failure_avg) %>%
    # Note: constructor failure can be a rolling average of constructor *IF NEEDED*, but driver_finish - driver_failure equals constructor failure
    dplyr::ungroup()


  return(results)
}
