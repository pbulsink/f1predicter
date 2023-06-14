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
    dplyr::arrange('season', 'race', 'position') %>%
    dplyr::filter(!(.data$status == 'Did not qualify' | .data$status == 'Did not prequalify')) %>%
    # Update Team Names:
    dplyr::mutate('constructorId' = dplyr::case_match(.data$constructorId,
                                                      c('tyrrell', 'bar', 'honda', 'brawn') ~ 'mercedes',
                                                      c('benetton', 'renault') ~ 'alpine',
                                                      'lotus_racing' ~ 'caterham',
                                                      'footwork' ~ 'arrows',
                                                      c('jordan', 'midland', 'spyker', 'spyker_mf1', 'force_india', 'racing_point') ~ 'aston_martin',
                                                      c('jaguar', 'stewart') ~ 'red_bull',
                                                      c('minardi', 'toro_rosso') ~ 'alphatauri',
                                                      c('lotus_f1', 'renault') ~ 'alpine',
                                                      'ligier' ~ 'prost',
                                                      c('marussia', 'virgin') ~ 'manor',
                                                      'osella' ~ 'fondmetal',
                                                      'sauber' ~ 'alfa', .default = .data$constructorId)) %>%
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
                                        'Debris', 'Cooling system', 'Water pump', 'Fuel leak', 'Undertray', 'Physical',
                                        'Distributor', 'Chassis', 'Wheel bearing', 'Halfshaft', 'Ignition', 'Injection',
                                        'Safety belt', 'Oil pump', 'Underweight', 'Safety concerns', 'Not restarted',
                                        'Stalled', 'Crankshaft', 'Safety'),1,0)) %>%
    dplyr::left_join(rg2[,c('quali_position', 'driverId', 'season', 'race')], by = c('driverId', 'season', 'race')) %>%
    dplyr::select('driverId', 'constructorId', 'position', 'grid', 'quali_position', 'pos_change', 'pos_change_perc',
                  'pos_change_points', 'fastest_rank', 'fastest_time' = 'time_sec', 'points', 'points_before', 'status',
                  'points_after', 'driver_experience', 'season', 'race', 'driver_failure', 'constructor_failure', 'finished')


  # laps ----
  laps <- input$laps %>%
    dplyr::arrange('season', 'race') %>%
    dplyr::filter(.data$deleted != TRUE) %>%
    dplyr::group_by(.data$season, .data$race, .data$sessionType, .data$driverId) %>%
    dplyr::mutate('best_time' = dplyr::if_else(is.infinite(min(.data$lapTime, na.rm = T)), NA_real_, min(.data$lapTime, na.rm = T)),
                  'num_laps' = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$race, .data$sessionType) %>%
    dplyr::mutate('gap_to_best' = .data$best_time - min(.data$best_time, na.rm = T),
                  'perc_to_best' = .data$best_time/min(.data$best_time, na.rm = T),
                  'perc_num_laps' = .data$num_laps/max(.data$num_laps, na.rm = T),
                  'rank' = dplyr::dense_rank(.data$best_time),
                  'best_s1' = min(.data$sector1Time, na.rm = T),
                  'best_s2' = min(.data$sector2Time, na.rm = T),
                  'best_s3' = min(.data$sector3Time, na.rm = T),
                  'optimal_time' = .data$best_s1 + .data$best_s2 + .data$best_s3,
                  'optimal_rank' = dplyr::dense_rank(.data$optimal_time)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'session_type' = 'sessionType', 'best_time', 'rank',
                  'num_laps', 'gap_to_best', 'perc_to_best', 'perc_num_laps', 'optimal_time', 'optimal_rank')  %>%
    unique()

  # practices ----
  practices <- laps %>%
    dplyr::arrange('season', 'race') %>%
    dplyr::filter(.data$session_type %in% c('FP1', 'FP2', 'FP3')) %>%
    dplyr::group_by(.data$season, .data$race, .data$driverId) %>%
    dplyr::mutate('practice_avg_rank' = mean(.data$rank, na.rm = T),
                  'practice_best_rank' = min(.data$rank, na.rm = T),
                  'practice_num_laps' = sum(.data$num_laps, na.rm = T),
                  'practice_avg_gap' = mean(.data$gap_to_best, na.rm = T),
                  'practice_best_gap' = min(.data$gap_to_best, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'practice_avg_rank', 'practice_best_rank',
                  'practice_num_laps', 'practice_avg_gap', 'practice_best_gap') %>%
    unique()

  # qualis ----
  qualis<-input$qualis %>%
    dplyr::arrange('season', 'race') %>%
    dplyr::select('driverId', 'Q1_sec', 'Q2_sec', 'Q3_sec', 'season', 'race') %>%
    dplyr::group_by(.data$season, .data$race) %>%
    dplyr::mutate('Q1_perc' = .data$Q1_sec/min(.data$Q1_sec, na.rm = T),
                  'Q2_perc' = .data$Q2_sec/min(.data$Q2_sec, na.rm = T),
                  'Q3_perc' = .data$Q3_sec/min(.data$Q3_sec, na.rm = T),
                  'Q_min_perc' = NA_real_) %>%
    dplyr::rowwise() %>%
    dplyr::mutate('Q_min_perc' = dplyr::if_else(is.infinite(min(dplyr::c_across(c('Q1_perc','Q2_perc', 'Q3_perc')), na.rm = T)), NA_real_, min(dplyr::c_across(c('Q1_perc','Q2_perc', 'Q3_perc')))),
                  'Q_avg_perc' = mean(dplyr::c_across(c('Q1_perc', 'Q2_perc', 'Q3_perc')), na.rm = T)) %>%
    dplyr::ungroup() %>%
    unique()

  # pitstops ----
  pitstops <- input$pitstops %>%
    dplyr::arrange('season', 'race') %>%
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
    dplyr::mutate('pit_duration_perc' = .data$pit_duration_avg/min(.data$pit_duration)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'pit_duration_perc', 'pit_stops') %>%
    unique() %>%
    dplyr::group_by(.data$season, .data$race) %>%
    dplyr::mutate('pit_num_perc' = .data$pit_stops/mean(.data$pit_stops, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select('driverId', 'season', 'race', 'pit_duration_perc', 'pit_num_perc') %>%
    unique()

  constructor_results <- results %>%
    dplyr::left_join(pitstops, by = c('race', 'season', 'driverId')) %>%
    dplyr::group_by(.data$season, .data$race, .data$constructorId) %>%
    dplyr::summarise('constructor_best_grid' = min(.data$grid, na.rm = T),
                     'constructor_best_finish' = min(.data$position, na.rm = T),
                     'constructor_failure_race' = mean(.data$constructor_failure, na.rm = T),
                     'constructor_pit_duration_perc' = mean(.data$pit_duration_perc, na.rm = T),
                     'constructor_pit_num_perc' = mean(.data$pit_num_perc)) %>%
    dplyr::mutate('constructor_grid_avg' = as.numeric(slider::slide(.data$constructor_best_grid, s_lagged_cumwmean_expanded, ln = 20, val = 18, .before = 20)),
                  'constructor_finish_avg' = as.numeric(slider::slide(.data$constructor_best_finish, s_lagged_cumwmean_expanded, ln = 20, val = 18, .before = 20)),
                  'constructor_failure_avg' = as.numeric(slider::slide(.data$constructor_failure_race, s_lagged_cumwmean_expanded, ln = 20, val = 0.1512, .before = 20)),
                  'constructor_pit_duration_avg' = as.numeric(slider::slide(.data$constructor_pit_duration_perc, s_lagged_cumwmean_expanded, ln = 20, val = 101, .before = 20)),
                  'constructor_pit_num_avg' = as.numeric(slider::slide(.data$constructor_pit_num_perc, s_lagged_cumwmean_expanded, ln = 20, val = 101, .before = 20))) %>%
    dplyr::select('constructorId', 'season', 'race', 'constructor_grid_avg', 'constructor_finish_avg',
                  'constructor_failure_race', 'constructor_failure_avg', 'constructor_pit_duration_perc',
                  'constructor_pit_num_perc', 'constructor_pit_duration_avg', 'constructor_pit_num_avg') %>%
    unique()

  results <- results %>%
    dplyr::arrange('season', 'race', 'position') %>%
    dplyr::left_join(qualis, by = c('race', 'season', 'driverId')) %>%
    dplyr::left_join(practices, by = c('race', 'season', 'driverId')) %>%
    dplyr::left_join(pitstops, by = c('race', 'season', 'driverId')) %>%
    dplyr::left_join(constructor_results, by = c('race', 'season', 'constructorId')) %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate('driver_pos_change_avg' = as.numeric(slider::slide(.data$pos_change, s_lagged_cumwmean_expanded, ln = 5, val = 0, .before = 5)),
                  'driver_points_change_avg' = as.numeric(slider::slide(.data$pos_change_points, s_lagged_cumwmean_expanded, ln = 5, val = 0, .before = 5)),
                  'driver_failure_avg' = as.numeric(slider::slide(.data$driver_failure, s_lagged_cumwmean_expanded, val = 0.0555, ln = 20, .before = 20)),
                  'driver_grid_avg' = as.numeric(slider::slide(.data$grid, s_lagged_cumwmean_expanded, val = 18, ln = 10, .before = 10)),
                  'driver_position_avg' = as.numeric(slider::slide(.data$position, s_lagged_cumwmean_expanded, val = 18, ln = 10, .before = 10)),
                  'driver_finish_avg' = as.numeric(slider::slide(.data$finished, s_lagged_cumwmean_expanded, val = 0.8689, ln = 10, .before = 10))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate('quali_position' = dplyr::if_else(is.na(.data$quali_position), .data$grid, .data$quali_position))

  return(results)
}
