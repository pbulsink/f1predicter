#' Generate New Data for Prediction
#'
#' @description
#' Creates a feature set for a specific upcoming race for a given set of drivers.
#' This function calculates various driver, constructor, and circuit-specific
#' features based on historical data. It's designed to generate the input data
#' required by the prediction functions (e.g., `predict_winner()`).
#'
#' @details
#' The function takes a season, round, and a list of drivers to generate a
#' predictive dataset. It pulls historical performance data for each driver,
#' their constructor, and the specific circuit from the `historical_data`.
#'
#' If round-specific data like qualifying position or practice ranks are not
#' provided in the `drivers` data frame, the function will estimate or impute
#' them based on historical averages or sensible defaults. For example, if `grid`
#' is missing, it will be estimated based on the driver's historical average
#' grid position.
#'
#' The features generated include driver experience, failure rates, average
#' finishing positions, constructor averages, and circuit-specific performance
#' metrics.
#'
#' @param season A numeric value for the season year (e.g., 2023).
#' @param round A numeric value for the round number of the season.
#' @param drivers A data frame or tibble containing the drivers for the race.
#'   It must include `driver_id` and `constructor_id`. Optionally, it can
#'   include `quali_position`, `grid`, and practice-related ranks. If `NULL`
#'   (the default), it will use the drivers from the most recent race.
#' @param historical_data A data frame of historical race data, typically the
#'   output of `clean_data()`. This is used to calculate the feature values.
#' @return A tibble where each row corresponds to a driver for the specified
#'   race, and columns are the features required for the modeling functions.
#' @export
generate_new_data <- function(
  season,
  round,
  drivers = NULL,
  historical_data = clean_data()
) {
  # drivers should be a data.frame or tibble with: driver_id, constructor_id, (optional any of: quali_position, grid, practice_optimal_rank, practice_best_rank,
  # practice_avg_rank) Any missing round-specific values will be given as current moved averages Any other values will be calculated from historical data,
  # presuming this is next round

  schedule <- f1predicter::schedule

  stopifnot(
    nrow(schedule[schedule$season == season & schedule$round == round, ]) > 0
  )

  if (is.null(drivers)) {
    drivers <- historical_data[historical_data$round_id == tail(historical_data$round_id, 1), c('driver_id', 'constructor_id')]
  }

  new_data <- tibble::as_tibble(drivers) %>%
    dplyr::mutate(
      season = season,
      round = round,
      round_id = paste0(season, "-", round),
      circuit_id = schedule[
        schedule$season == season & schedule$round == round,
      ]$circuit_id
    )

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
    dplyr::summarise(dplyr::across(
      dplyr::where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      constructor_grid_avg = tidyr::replace_na(
        .data$constructor_grid_avg,
        default_params$grid
      ),
      grid = tidyr::replace_na(.data$grid, default_params$grid),
      constructor_finish_avg = tidyr::replace_na(
        .data$constructor_finish_avg,
        1 - default_params$constructor_failure_avg * 2
      ),
      finished = tidyr::replace_na(
        .data$finished,
        1 - default_params$constructor_failure_avg
      ),
      constructor_failure_avg = tidyr::replace_na(
        .data$constructor_failure_avg,
        default_params$constructor_failure_avg * 2
      ),
      constructor_failure_race = tidyr::replace_na(
        .data$constructor_failure_race,
        default_params$constructor_failure_avg * 2
      ),
      constructor_grid_avg = wmean_two(
        .data$grid,
        .data$constructor_grid_avg,
        20
      ),
      constructor_failure_avg = wmean_two(
        .data$constructor_failure_race,
        .data$constructor_failure_avg,
        20
      ),
      constructor_finish_avg = wmean_two(
        .data$finished,
        .data$constructor_finish_avg,
        20
      )
    ) %>%
    unique()

  hd_circuit <- historical_data %>%
    dplyr::filter(.data$circuit_id %in% new_data$circuit_id)
  if (nrow(hd_circuit) > 0) {
    hd_circuit <- hd_circuit %>%
      dplyr::group_by(.data$circuit_id) %>%
      dplyr::filter(.data$season == max(.data$season)) %>%
      dplyr::filter(.data$round == max(.data$round)) %>%
      dplyr::summarise(dplyr::across(
        dplyr::where(is.numeric),
        ~ mean(.x, na.rm = TRUE)
      )) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        grid_pos_corr_avg = tidyr::replace_na(
          .data$grid_pos_corr_avg,
          default_params$grid_pos_corr_avg
        ),
        grid_pos_corr = tidyr::replace_na(
          .data$grid_pos_corr,
          default_params$grid_pos_corr_avg
        ),
        driver_failure_circuit_avg = tidyr::replace_na(
          .data$driver_failure_circuit_avg,
          default_params$driver_failure_avg
        ),
        driver_failure_circuit = tidyr::replace_na(
          .data$driver_failure_circuit,
          default_params$driver_failure_avg
        ),
        constructor_failure_circuit_avg = tidyr::replace_na(
          .data$constructor_failure_circuit_avg,
          default_params$constructor_failure_avg
        ),
        constructor_failure_circuit = tidyr::replace_na(
          .data$constructor_failure_circuit,
          default_params$constructor_failure_avg
        ),
        grid_pos_corr_avg = wmean_two(
          .data$grid_pos_corr,
          .data$grid_pos_corr_avg,
          5
        ),
        driver_failure_circuit_avg = wmean_two(
          .data$driver_failure_circuit,
          .data$driver_failure_circuit_avg,
          5
        ),
        constructor_failure_circuit_avg = wmean_two(
          .data$constructor_failure_circuit,
          .data$constructor_failure_circuit_avg,
          5
        )
      ) %>%
      unique()
  } else {
    #Need to make a new version - use defaults
    hd_circuit <- tibble::tibble(
      'circuit_id' = new_data$circuit_id,
      'grid_pos_corr_avg' = default_params$grid_pos_corr_avg,
      'driver_failure_circuit_avg' = default_params$driver_failure_avg,
      'constructor_failure_circuit_avg' = default_params$constructor_failure_avg
    )
  }

  # Fill in missing columns
  if (!('grid' %in% colnames(new_data))) {
    # sort drivers by their average grid for an estimate
    new_data <- new_data %>%
      dplyr::left_join(
        hd_driver[, c('driver_id', 'driver_grid_avg', 'grid')],
        by = 'driver_id'
      ) %>%
      dplyr::rename('last_grid' = .data$grid) %>%
      dplyr::mutate(
        driver_grid_avg = tidyr::replace_na(
          .data$driver_grid_avg,
          default_params$grid
        ),
        last_grid = tidyr::replace_na(.data$last_grid, default_params$grid),
        driver_grid_avg = wmean_two(.data$last_grid, .data$driver_grid_avg, 10),
        grid = order(order(.data$driver_grid_avg))
      )
  } else {
    new_data <- new_data %>%
      dplyr::left_join(
        hd_driver[, c('driver_id', 'driver_grid_avg', 'grid')],
        by = 'driver_id'
      ) %>%
      dplyr::rename('grid' = 'grid.x', 'last_grid' = 'grid.y') %>%
      dplyr::mutate(
        driver_grid_avg = tidyr::replace_na(
          .data$driver_grid_avg,
          default_params$grid
        ),
        last_grid = tidyr::replace_na(.data$last_grid, default_params$grid),
        driver_grid_avg = wmean_two(.data$last_grid, .data$driver_grid_avg, 10)
      )
  }

  if (!('quali_position' %in% colnames(new_data))) {
    new_data$quali_position <- new_data$grid
  }

  #Load driver/constructor/circuit info
  new_data <- new_data %>%
    dplyr::left_join(
      hd_driver[, c(
        'driver_id',
        'driver_experience',
        'driver_failure',
        'driver_failure_avg',
        'position',
        'driver_position_avg',
        'finished',
        'driver_finish_avg',
        'driver_avg_qgap'
      )],
      by = 'driver_id'
    ) %>%
    dplyr::mutate(
      driver_experience = tidyr::replace_na(.data$driver_experience, -1),
      driver_experience = .data$driver_experience + 1,
      driver_failure_avg = tidyr::replace_na(
        .data$driver_failure_avg,
        default_params$driver_failure_avg
      ),
      driver_failure = tidyr::replace_na(
        .data$driver_failure,
        default_params$driver_failure_avg
      ),
      driver_failure_avg = wmean_two(
        .data$driver_failure,
        .data$driver_failure_avg,
        20
      ),
      position = tidyr::replace_na(.data$position, default_params$position),
      driver_position_avg = tidyr::replace_na(
        .data$driver_position_avg,
        default_params$position
      ),
      driver_position_avg = wmean_two(
        .data$position,
        .data$driver_position_avg,
        10
      ),
      driver_finish_avg = tidyr::replace_na(
        .data$driver_finish_avg,
        default_params$driver_finish_avg
      ),
      finished = tidyr::replace_na(
        .data$finished,
        default_params$driver_finish_avg
      ),
      driver_finish_avg = wmean_two(.data$finished, .data$driver_finish_avg, 10)
    ) %>%
    dplyr::mutate(
      driver_avg_qgap = tidyr::replace_na(
        .data$driver_avg_qgap,
        default_params$qgap
      )
    ) %>%
    dplyr::left_join(
      hd_constructor[, c(
        'constructor_id',
        'constructor_grid_avg',
        'constructor_finish_avg',
        'constructor_failure_avg'
      )],
      by = 'constructor_id'
    ) %>%
    dplyr::left_join(
      hd_circuit[, c(
        'circuit_id',
        'grid_pos_corr_avg',
        'driver_failure_circuit_avg',
        'constructor_failure_circuit_avg'
      )],
      by = 'circuit_id'
    ) %>%
    dplyr::mutate(
      constructor_grid_avg = tidyr::replace_na(.data$constructor_grid_avg, default_params$grid)
    )

  #Optionally included practice data
  if (!("driver_practice_optimal_rank_avg" %in% colnames(new_data))) {
    new_data$driver_practice_optimal_rank_avg <- NA
  }
  if (!("practice_avg_rank" %in% colnames(new_data))) {
    new_data$practice_avg_rank <- NA
  }
  if (!("practice_best_rank" %in% colnames(new_data))) {
    new_data$practice_best_rank <- NA
  }
  if (!("practice_optimal_rank" %in% colnames(new_data))) {
    new_data$practice_optimal_rank <- NA
  }

  new_data <- new_data %>%
    dplyr::select(
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
      "driver_avg_qgap",
      "grid_pos_corr_avg",
      "driver_failure_circuit_avg",
      "constructor_failure_circuit_avg",
      "driver_practice_optimal_rank_avg",
      "practice_avg_rank",
      "practice_best_rank",
      "practice_optimal_rank",
      "season",
      "round",
      "round_id"
    ) %>%
    unique() %>%
    dplyr::mutate(
      round_id = as.factor(.data$round_id),
      driver_id = as.factor(.data$driver_id),
      constructor_id = as.factor(.data$constructor_id)
    )

  return(new_data)
}

#' Generate Prediction Data for the Next Race
#'
#' @description
#' A wrapper around `generate_new_data()` that automatically finds the next
#' upcoming race from the `f1predicter::schedule` data and generates the
#' corresponding feature set.
#'
#' @details
#' This function identifies the next race by finding the first entry in the
#' `f1predicter::schedule` with a date greater than or equal to the current
#' system date. It then extracts the `season` and `round` for that race and
#' passes them to `generate_new_data()`.
#'
#' A message is printed to the console indicating which race (`season`, `round`,
#' and `race_name`) the data is being generated for.
#'
#' @param ... Additional arguments to be passed to `generate_new_data()`, such
#'   as `drivers` or `historical_data`.
#'
#' @return A tibble where each row corresponds to a driver for the next
#'   upcoming race, and columns are the features required for the modeling
#'   functions.
#' @seealso [generate_new_data()]
#' @export
generate_next_race_data <- function(...) {
  schedule <- f1predicter::schedule %>%
    dplyr::mutate(date = as.Date(.data$date))

  next_race <- schedule %>%
    dplyr::filter(.data$date >= Sys.Date()) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::slice(1)

  if (nrow(next_race) == 0) {
    cli::cli_abort("Could not find an upcoming race in the schedule.")
  }

  cli::cli_inform(
    "Generating data for the next race: {next_race$season} Round {next_race$round} - {next_race$race_name}"
  )

  generate_new_data(
    season = as.numeric(next_race$season),
    round = as.numeric(next_race$round),
    ...
  )
}


#' Predict Pole Position
#'
#' @description
#' Predicts the probability of each driver achieving pole position for an upcoming
#' race.
#'
#' @details
#' This function takes a `workflow` object (trained for pole position prediction)
#' and a data frame of features for the upcoming race. The user can control
#' whether to make an "early" (pre-practice) or "late" (post-practice)
#' prediction by passing the appropriate model object from `model_quali_early()`
#' or `model_quali_late()`.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param quali_pole_model A `workflow` object for predicting pole position,
#'   such as `model_quali_early()$quali_pole`.
#' @return A tibble with `driver_id`, `round`, `season`, and `pole_odd` (the
#'   predicted probability of getting pole position).
#' @export
predict_quali_pole <- function(
  new_data = generate_next_race_data(),
  quali_pole_model
) {
  preds <- new_data %>%
    dplyr::mutate(
      pole_odd = (tune::extract_workflow(quali_pole_model) %>%
        stats::predict(new_data, type = "prob"))$.pred_1
    ) %>%
    dplyr::mutate(
      pole_odd = normalize_vector(.data$pole_odd)
    ) %>%
    dplyr::select("driver_id", "round", "season", "pole_odd") %>%
    dplyr::arrange(-.data$pole_odd)
  return(preds)
}

#' Predict Qualifying Position
#'
#' @description
#' Predicts the likely qualifying position for each driver in an upcoming race.
#'
#' @details
#' This function takes a `workflow` object (trained for qualifying position
#' prediction) and a data frame of features for the upcoming race. The user can
#' control whether to make an "early" (pre-practice) or "late" (post-practice)
#' prediction by passing the appropriate model object from `model_quali_early()`
#' or `model_quali_late()`.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param quali_pos_model A `workflow` object for predicting qualifying position,
#'   such as `model_quali_early()$quali_pos`.
#' @return A tibble with `driver_id`, `round`, `season`, and
#'   `likely_quali_position`.
#' @export
predict_quali_pos <- function(new_data = generate_next_race_data(), quali_pos_model) {

  preds <- new_data %>%
    dplyr::mutate(
      likely_quali_position = (tune::extract_workflow(quali_pos_model) %>%
                    stats::predict(new_data, type = "numeric"))$.pred
    ) %>%
    dplyr::select("driver_id", "round", "season", "likely_quali_position") %>%
    dplyr::arrange(-.data$likely_quali_position)
  return(preds)
}

#' Predict Qualifying Results for a Round
#'
#' @description
#' A wrapper function to predict both pole position probability and likely
#' qualifying position for a given round.
#'
#' @details
#' This function combines the outputs of `predict_quali_pole()` and
#' `predict_quali_pos()` into a single tibble.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param quali_pole_model A `workflow` object for predicting pole position.
#' @param quali_pos_model A `workflow` object for predicting qualifying position.
#' @return A tibble with predictions for pole probability and qualifying position
#'   for each driver.
#' @export
predict_quali_round <- function(
  new_data = generate_next_race_data(),
  quali_pole_model,
  quali_pos_model
) {
  pole_preds <- predict_quali_pole(new_data, quali_pole_model)
  pos_preds <- predict_quali_pos(new_data, quali_pos_model)

  all_preds <- pole_preds %>%
    dplyr::left_join(pos_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::arrange(-.data$pole_odd)
  return(all_preds)
}

#' Predict Race Winner
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param win_model A `workflow` object for predicting the winner.
#' @return A tibble with `driver_id`, `round`, `season`, and `win_odd`.
#' @keywords internal
predict_winner <- function(new_data = generate_next_race_data(), win_model) {
  preds <- new_data %>%
    dplyr::mutate(
      win_odd = tune::extract_workflow(win_model) %>%
        stats::predict(new_data, type = "prob")$.pred_1
    ) %>%
    dplyr::select("driver_id", "round", "season", "win_odd")
  return(preds)
}

#' Predict Podium Finish
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param podium_model A `workflow` object for predicting a podium finish.
#' @return A tibble with `driver_id`, `round`, `season`, and `podium_odd`.
#' @keywords internal
predict_podium <- function(new_data = generate_next_race_data(), podium_model) {
  preds <- new_data %>%
    dplyr::mutate(
      podium_odd = tune::extract_workflow(podium_model) %>%
        stats::predict(new_data, type = "prob")$.pred_1
    ) %>%
    dplyr::select("driver_id", "round", "season", "podium_odd")
  return(preds)
}

#' Predict Top 10 Finish
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param t10_model A `workflow` object for predicting a top 10 finish.
#' @return A tibble with `driver_id`, `round`, `season`, and `t10_odd`.
#' @keywords internal
predict_t10 <- function(new_data = generate_next_race_data(), t10_model) {
  preds <- new_data %>%
    dplyr::mutate(
      t10_odd = tune::extract_workflow(t10_model) %>%
        stats::predict(new_data, type = "prob")$.pred_1
    ) %>%
    dplyr::select("driver_id", "round", "season", "t10_odd")
  return(preds)
}

#' Predict Race Finish (Not DNF)
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param finish_model A `workflow` object for predicting finishing the race.
#' @return A tibble with `driver_id`, `round`, `season`, and `finish_odd`.
#' @keywords internal
predict_finish <- function(new_data = generate_next_race_data(), finish_model) {
  preds <- new_data %>%
    dplyr::mutate(
      finish_odd = tune::extract_workflow(finish_model) %>%
        stats::predict(new_data, type = "prob")$.pred_1
    ) %>%
    dplyr::select("driver_id", "round", "season", "finish_odd")
  return(preds)
}

#' Predict Finishing Position
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param position_model A `workflow` object for predicting the finishing position.
#' @return A tibble with `driver_id`, `round`, `season`, and `likely_position`.
#' @keywords internal
predict_position <- function(new_data = generate_next_race_data(), position_model) {
  position_preds <- stats::predict(tune::extract_workflow(position_model), new_data, type = "numeric")

  preds <- new_data %>%
    dplyr::select("driver_id", "round", "season") %>%
    dplyr::bind_cols(position_preds) %>%
    dplyr::rename(likely_position = .data$.pred)
  return(preds)
}

#' Predict Race Results for a Round
#'
#' @description
#' A wrapper function to predict all race outcomes for a given round.
#'
#' @details
#' This function combines the outputs of `predict_winner()`, `predict_podium()`,
#' `predict_t10()`, `predict_finish()`, and `predict_position()` into a single
#' tibble. It requires the full suite of models trained by one of the
#' `model_results_*()` functions.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param win_model A `workflow` object for predicting the winner.
#' @param podium_model A `workflow` object for predicting a podium finish.
#' @param t10_model A `workflow` object for predicting a top 10 finish.
#' @param finish_model A `workflow` object for predicting finishing the race.
#' @param position_model A `workflow` object for predicting the finishing position.
#' @return A tibble with predictions for all race outcomes for each driver,
#'   including win/podium/t10/finish odds and the likely finishing position.
#' @export
predict_round <- function(
  new_data = generate_next_race_data(),
  win_model,
  podium_model,
  t10_model,
  finish_model,
  position_model
) {
  win_preds <- predict_winner(new_data, win_model)
  podium_preds <- predict_podium(new_data, podium_model)
  t10_preds <- predict_t10(new_data, t10_model)
  finish_preds <- predict_finish(new_data, finish_model)
  position_preds <- predict_position(new_data, position_model)

  all_preds <- win_preds %>%
    dplyr::left_join(podium_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(t10_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(finish_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(position_preds, by = c("driver_id", "round", "season"))
  return(all_preds)
}
