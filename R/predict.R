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
#' @param penalties A named vector of `driver_id = penalty_positions` to be
#'   applied (in order of application).
#'   For example `c('hamilton' = 5, 'max_verstappen' = 10)`.
#' @return A tibble where each row corresponds to a driver for the specified
#'   race, and columns are the features required for the modeling functions.
#' @export
generate_new_data <- function(
  season,
  round,
  drivers = NULL,
  penalties = NULL,
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
    drivers <- historical_data[
      historical_data$round_id == utils::tail(historical_data$round_id, 1),
      c('driver_id', 'constructor_id')
    ]
  }

  cli::cli_inform("Building next race data tibbles")
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

  #Load driver/constructor/circuit info
  new_data <- new_data %>%
    dplyr::left_join(
      hd_driver[, c(
        'driver_id',
        'driver_experience',
        'driver_failure',
        'driver_failure_avg',
        'position',
        'grid',
        "quali_position",
        'driver_position_avg',
        'finished',
        'driver_finish_avg',
        'driver_avg_qgap',
        'driver_grid_avg',
        'driver_practice_optimal_rank_avg'
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
      constructor_grid_avg = tidyr::replace_na(
        .data$constructor_grid_avg,
        default_params$grid
      )
    )

  # TODO Refactor processing code to subfunctions to do the same laps calculations for practice there as here
  # Try to load laps and quali for the round to get up-to-date data
  cli::cli_inform("Checking for mid-weekend data")
  laps <- tryCatch(
    get_laps(season = season, round = round),
    error = function(e) NULL
  )

  if (!is.null(laps) && nrow(laps) > 0) {
    cli::cli_inform(
      "Found lap data for {season} round {round}. Calculating practice stats."
    )
    practice_results <- laps %>%
      dplyr::mutate(season = season, round = round) %>%
      add_drivers_to_laps(season = season) %>%
      process_lap_times() %>%
      summarize_practice_laps() %>%
      dplyr::select(-c('season', 'round'))
    # d_ids <- f1dataR::load_drivers(season = season) %>%
    #   dplyr::select("driver_id", "code")
    # practice_laps <- laps %>%
    #   dplyr::filter(.data$session_type %in% c("FP1", "FP2", "FP3"))

    # practice_results <- practice_laps %>%
    #   dplyr::left_join(d_ids, by = c(driver = "code")) %>%
    # dplyr::group_by(.data$driver_id, .data$session_type) %>%
    # dplyr::summarise(
    #   best_lap_time = min(.data$lap_time, na.rm = TRUE),
    #   optimal_lap_time = min(.data$sector1time, na.rm = TRUE) +
    #     min(.data$sector2time, na.rm = TRUE) +
    #     min(.data$sector3time, na.rm = TRUE),
    #   .groups = "drop_last"
    # ) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(.data$session_type) %>%
    # dplyr::mutate(
    #   best_rank = rank(.data$best_lap_time, ties.method = "min"),
    #   optimal_rank = rank(.data$optimal_lap_time, ties.method = "min")
    # ) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(.data$driver_id) %>%
    # dplyr::summarise(
    #   practice_best_rank = min(.data$best_rank, na.rm = TRUE),
    #   practice_avg_rank = mean(.data$best_rank, na.rm = TRUE),
    #   practice_optimal_rank = min(.data$optimal_rank, na.rm = TRUE)
    # )

    new_data <- new_data %>%
      dplyr::left_join(practice_results, by = "driver_id")
  } else {
    new_data$driver_practice_optimal_rank_avg <- nrow(new_data) * 3 / 4
    new_data$practice_avg_rank <- round(nrow(new_data) * 3 / 4)
    new_data$practice_best_rank <- round(nrow(new_data) / 2)
    new_data$practice_optimal_rank <- round(nrow(new_data) / 2)
    new_data$practice_avg_gap <- 1.5
    new_data$practice_best_gap <- 1
  }

  quali <- tryCatch(
    f1dataR::load_quali(season = season, round = round),
    error = function(e) NULL
  )

  if (!is.null(quali) && nrow(quali) > 0) {
    cli::cli_inform("Found qualifying data for {season} round {round}.")
    quali$round <- round
    quali$season <- season
    quali_results <- process_quali_times(quali)
    quali_results <- quali_results %>%
      dplyr::mutate(
        'quali_position' = dplyr::row_number()
      ) %>%
      dplyr::select(-"driver_avg_qgap")

    # If quali_position already exists, remove it before joining
    if ("quali_position" %in% names(new_data)) {
      new_data$quali_position <- NULL
    }
    if ("grid" %in% names(new_data)) {
      new_data$grid <- NULL
    }
    new_data <- new_data %>%
      dplyr::left_join(quali_results) %>%
      dplyr::mutate(
        grid = .data$quali_position,
        driver_avg_qgap = 0.8*driver_avg_qgap + 0.2*qgap
      )

  } else {
    # sort drivers by their average grid for an estimate
    new_data <- new_data %>%
      dplyr::rename('last_grid' = 'grid') %>%
      dplyr::mutate(
        driver_grid_avg = tidyr::replace_na(
          .data$driver_grid_avg,
          default_params$grid
        ),
        last_grid = tidyr::replace_na(.data$last_grid, default_params$grid),
        driver_grid_avg = wmean_two(.data$last_grid, .data$driver_grid_avg, 10),
        grid = order(order(.data$driver_grid_avg)),
        driver_avg_qgap = 1,
        quali_position = .data$grid,
        q_min_perc = 1.02,
        q_avg_perc = 1.02,
      )
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
      "practice_avg_gap",
      "practice_best_gap",
      "q_min_perc",
      "q_avg_perc",
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

  if (!is.null(penalties)) {
    for (p in seq_along(penalties)) {
      new_data <- apply_grid_penalty(
        new_data,
        names(penalties)[p],
        penalties[[p]]
      )
    }
  }

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

#' Apply a Grid Penalty to a Driver
#'
#' @description
#' Adjusts the starting grid by applying a penalty to a specific driver. The
#' penalized driver is moved down the grid, and other drivers are moved up to
#' fill the vacated spot.
#'
#' @details
#' The function uses the `quali_position` column as the initial grid order.
#' It calculates the new position for the penalized driver and re-orders the
#' grid accordingly. The penalty is capped at the last position on the grid
#' (e.g., a 5-place penalty for a driver in 19th out of 20 results in a 20th
#' place start).
#'
#' @param race_data A data frame containing race participants. Must include
#'   `driver_id`, `quali_position` and `grid`.
#' @param driver_id The character string ID of the driver to penalize.
#' @param penalty A positive integer representing the number of grid places
#'   for the penalty.
#'
#' @return A tibble identical to `race_data` but with the `grid` column
#'   updated to reflect the penalty.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assume `after_quali_data` is a data frame with quali results
#'   # Apply a 5-place penalty to 'max_verstappen'
#'   new_grid_data <- apply_grid_penalty(
#'     race_data = generate_next_race_data(),
#'     driver_id = "max_verstappen",
#'     penalty = 5
#'   )
#' }
apply_grid_penalty <- function(
  race_data = generate_next_race_data(),
  driver_id,
  penalty
) {
  # --- Input Validation ---
  stopifnot(
    is.data.frame(race_data),
    "driver_id" %in% names(race_data),
    "quali_position" %in% names(race_data),
    "grid" %in% names(race_data)
  )
  if (!driver_id %in% race_data$driver_id) {
    cli::cli_abort("Driver {.val {driver_id}} not found in the provided data.")
  }
  if (!is.numeric(penalty) || penalty <= 0) {
    cli::cli_abort("{.arg penalty} must be a positive number.")
  }

  # --- Apply Penalty ---
  # Establish the pre-penalty grid order based on qualifying
  sorted_drivers <- race_data %>%
    dplyr::arrange(.data$grid) %>%
    dplyr::mutate(driver_id = as.character(.data$driver_id))
  driver_order <- as.character(sorted_drivers$driver_id)

  original_pos <- which(driver_order == driver_id)
  target_pos <- min(original_pos + penalty, length(driver_order))

  # Re-order the drivers
  driver_order <- append(
    driver_order[-original_pos],
    driver_id,
    after = target_pos - 1
  )

  # Create the new grid mapping and join it back to the original data
  new_grid_df <- tibble::tibble(
    driver_id = driver_order,
    grid = seq_along(driver_order)
  )

  race_data <- race_data %>%
    dplyr::select(-dplyr::any_of("grid")) %>%
    dplyr::left_join(new_grid_df, by = "driver_id")

  return(race_data)
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
#'   such as `model_quali_early()$quali_pole`. Can also be a `model_stack` ensemble object
#' @return A tibble with `driver_id`, `round`, `season`, and `pole_odd` (the
#'   predicted probability of getting pole position).
#' @export
predict_quali_pole <- function(
  new_data = generate_next_race_data(),
  quali_pole_model
) {
  pred_call <- if (inherits(quali_pole_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(quali_pole_model, new_data, type = "prob")
  } else {
    stats::predict(
      tune::extract_workflow(quali_pole_model),
      new_data,
      type = "prob"
    )
  }

  preds <- new_data %>%
    dplyr::mutate(pole_odd = pred_call$.pred_1) %>%
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
#'   such as `model_quali_early()$quali_pos`. Can also be a `model_stack` ensemble object.
#' @param is_ensemble A logical indicating if the provided model is a `stacks`
#'   ensemble. Defaults to `FALSE`.
#' @return A tibble with `driver_id`, `round`, `season`, and
#'   `likely_quali_position`.
#' @export
predict_quali_pos <- function(
  new_data = generate_next_race_data(),
  quali_pos_model,
  is_ensemble = FALSE
) {
  pred_call <- if (inherits(quali_pos_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(quali_pos_model, new_data, type = "numeric")
  } else {
    stats::predict(
      tune::extract_workflow(quali_pos_model),
      new_data,
      type = "numeric"
    )
  }

  preds <- new_data %>%
    dplyr::mutate(likely_quali_position = pred_call$.pred) %>%
    dplyr::select("driver_id", "round", "season", "likely_quali_position") %>%
    dplyr::arrange(.data$likely_quali_position)
  return(preds)
}

#' Predict Qualifying Position (Classification)
#'
#' @description
#' Predicts the likely qualifying position for each driver using an ordered
#' classification model.
#'
#' @details
#' This function takes a `workflow` object (trained for qualifying position
#' prediction using a classification method) and a data frame of features for
#' the upcoming race. The user can control whether to make an "early"
#' (pre-practice) or "late" (post-practice) prediction by passing the
#' appropriate model object from `model_quali_early()` or `model_quali_late()`.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param quali_pos_class_model A `workflow` object for predicting qualifying position,
#'   such as `model_quali_early()$quali_pos_class`.
#' @return A tibble with `driver_id`, `round`, `season`, and
#'   `likely_quali_position_class`.
#' @export
predict_quali_pos_class <- function(
  new_data = generate_next_race_data(),
  quali_pos_class_model
) {
  # The polr model is not a workflow, so it needs special handling.
  # It's a list containing the fit and the prepped recipe.
  baked_new_data <- recipes::bake(
    quali_pos_class_model$recipe,
    new_data = new_data
  )

  preds <- new_data %>%
    dplyr::mutate(
      .pred = stats::predict(
        quali_pos_class_model$fit,
        newdata = baked_new_data,
        type = "class"
      ),
      .probs = stats::predict(
        quali_pos_class_model$fit,
        newdata = baked_new_data,
        type = "probs"
      )
    ) %>%
    # The prediction is a factor, convert to numeric for sorting/comparison
    dplyr::mutate(
      likely_quali_position_class = as.numeric(as.character(.data$.pred))
    ) %>%
    dplyr::select(
      "driver_id",
      "round",
      "season",
      "likely_quali_position_class",
      ".probs"
    ) %>%
    dplyr::arrange(.data$likely_quali_position_class)
  return(preds)
}

#' Predict Qualifying Results for a Round
#'
#' @description
#' A wrapper function to predict pole position probability and likely qualifying
#' position for a given round using multiple models.
#'
#' @details
#' This function combines the outputs of `predict_quali_pole()`,
#' `predict_quali_pos()`, and `predict_quali_pos_class()` into a single tibble.
#' It takes the list of fitted models returned by `model_quali_early()` or
#' `model_quali_late()`.
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param quali_models A list of fitted `workflow` objects for qualifying prediction.
#'   If `NULL` (default), the function will attempt to load the "early"
#'   qualifying models using `load_models()`. Otherwise, it should be a list as
#'   returned by `model_quali_early()` or `model_quali_late()`.
#' @param engine The model engine to use. Can be an individual engine like
#'   `"ranger"` or `"glmnet"`, or `"ensemble"` (default) to load stacked models.
#' @return A tibble with predictions for pole probability and qualifying position
#'   (from both regression and classification models) for each driver.
#' @export
predict_quali_round <- function(
  new_data = generate_next_race_data(),
  quali_models = NULL,
  engine = "ensemble" # TODO: change to autodetect engine
) {
  # If quali_models is NULL or a character string, load the appropriate models
  if (is.null(quali_models) || is.character(quali_models)) {
    if (is.character(quali_models)) {
      valid_timings <- c("early", "late")
      if (!quali_models %in% valid_timings) {
        cli::cli_abort(
          "{.arg quali_models} must be one of {.val {valid_timings}} when provided as a string."
        )
      }
      model_timing <- quali_models
    } else {
      # is.null(quali_models), so auto-detect
      model_timing <- if (any(grepl("practice", names(new_data)))) {
        "late"
      } else {
        "early"
      }
    }
    cli::cli_inform(
      "Loading '{model_timing}' qualifying models for engine {.val {engine}} from disk."
    )
    quali_models <- load_models(
      model_type = "quali",
      model_timing = model_timing,
      engine = engine
    )
  }

  # Check if all required models are in the list
  required_models <- c("quali_pole", "quali_pos", "quali_pos_class")
  if (!all(required_models %in% names(quali_models))) {
    cli::cli_abort(
      "The {.arg quali_models} list must contain the following models: {.val {required_models}}"
    )
  }

  pole_preds <- predict_quali_pole(new_data, quali_models$quali_pole)
  pos_preds <- predict_quali_pos(new_data, quali_models$quali_pos)

  # For ensemble models, the ordinal classification model (polr) was trained
  # using the predictions from the other ensemble models as features. We need
  # to replicate that here by adding those predictions to the new_data frame.
  data_for_class_model <- if (engine == "ensemble") {
    cli::cli_inform(
      "Adding ensemble predictions as features for the ordinal model."
    )
    # Get the raw probability for pole and numeric prediction for position
    pole_ensemble_preds <- stats::predict(
      quali_models$quali_pole,
      new_data,
      type = "prob"
    )
    pos_ensemble_preds <- stats::predict(
      quali_models$quali_pos,
      new_data,
      type = "numeric"
    )

    # Add them as new columns with the names expected by the polr model's recipe
    new_data %>%
      dplyr::mutate(
        ensemble_pole_pred = pole_ensemble_preds$.pred_1,
        ensemble_pos_pred = pos_ensemble_preds$.pred
      )
  } else {
    # For single-engine models, no extra features are needed
    new_data
  }

  pos_class_preds <- predict_quali_pos_class(
    data_for_class_model,
    quali_models$quali_pos_class
  )

  all_preds <- pole_preds %>%
    dplyr::left_join(pos_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(
      pos_class_preds,
      by = c("driver_id", "round", "season")
    ) %>%
    dplyr::arrange(-.data$pole_odd)
  return(all_preds)
}

#' Predict Race Winner
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param win_model A `workflow` object for predicting the winner.
#' @return A tibble with `driver_id`, `round`, `season`, and `win_odd`.
#' @keywords internal
predict_winner <- function(
  new_data = generate_next_race_data(),
  win_model
) {
  pred_call <- if (inherits(win_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(win_model, new_data, type = "prob")
  } else {
    stats::predict(tune::extract_workflow(win_model), new_data, type = "prob")
  }

  preds <- new_data %>%
    dplyr::mutate(
      win_odd = pred_call$.pred_1
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
predict_podium <- function(
  new_data = generate_next_race_data(),
  podium_model
) {
  pred_call <- if (inherits(podium_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(podium_model, new_data, type = "prob")
  } else {
    stats::predict(
      tune::extract_workflow(podium_model),
      new_data,
      type = "prob"
    )
  }

  preds <- new_data %>%
    dplyr::mutate(
      podium_odd = pred_call$.pred_1
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
predict_t10 <- function(
  new_data = generate_next_race_data(),
  t10_model
) {
  pred_call <- if (inherits(t10_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(t10_model, new_data, type = "prob")
  } else {
    stats::predict(tune::extract_workflow(t10_model), new_data, type = "prob")
  }

  preds <- new_data %>%
    dplyr::mutate(
      t10_odd = pred_call$.pred_1
    ) %>%
    dplyr::select("driver_id", "round", "season", "t10_odd")
  return(preds)
}

#' Predict Finishing Position
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param position_model A `workflow` object for predicting the finishing position.
#' @return A tibble with `driver_id`, `round`, `season`, and `likely_position`.
#' @keywords internal
predict_position <- function(
  new_data = generate_next_race_data(),
  position_model
) {
  position_preds <- if (inherits(position_model, "model_stack")) {
    if (!requireNamespace("stacks", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg stacks} must be installed to predict with an ensemble model."
      )
    }
    stats::predict(position_model, new_data, type = "numeric")
  } else {
    stats::predict(
      tune::extract_workflow(position_model),
      new_data,
      type = "numeric"
    )
  }
  preds <- new_data %>%
    dplyr::select("driver_id", "round", "season") %>%
    dplyr::bind_cols(position_preds) %>%
    dplyr::rename(likely_position = .data$.pred)
  return(preds)
}

#' Predict Finishing Position (Classification)
#'
#' @param new_data A data frame of new data, typically from `generate_new_data()`.
#' @param position_class_model A list containing the `polr` fit and recipe.
#' @return A tibble with `driver_id`, `round`, `season`, and `likely_position_class`.
#' @keywords internal
predict_position_class <- function(
  new_data = generate_next_race_data(),
  position_class_model
) {
  # The polr model is not a workflow, so it needs special handling.
  # It's a list containing the fit and the prepped recipe.
  baked_new_data <- recipes::bake(
    position_class_model$recipe,
    new_data = new_data
  )

  preds <- new_data %>%
    dplyr::mutate(
      .pred = stats::predict(
        position_class_model$fit,
        newdata = baked_new_data,
        type = "class"
      ),
      .probs = stats::predict(
        position_class_model$fit,
        newdata = baked_new_data,
        type = "probs"
      )
    ) %>%
    # The prediction is a factor, convert to numeric for sorting/comparison
    dplyr::mutate(
      likely_position_class = as.numeric(as.character(.data$.pred))
    ) %>%
    dplyr::select(
      "driver_id",
      "round",
      "season",
      "likely_position_class",
      ".probs"
    ) %>%
    dplyr::arrange(.data$likely_position_class)
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
#' @param results_models A list of fitted `workflow` objects for race results prediction.
#'   If `NULL` (default), the function will attempt to load the "early" results
#'   models using `load_models()`. Otherwise, it should be a list as returned by
#'   a `model_results_*()` function, containing `win`, `podium`, `t10`, and `position`.
#' @param engine The model engine to use if loading models from disk. Defaults
#'   to `"ranger"`. Can also be `"ensemble"`.
#' @return A tibble with predictions for all race outcomes for each driver,
#'   including win/podium/t10 odds and the likely finishing position.
#' @export
predict_round <- function(
  new_data = generate_next_race_data(),
  results_models = NULL,
  engine = "ensemble"
) {
  # If results_models is NULL or a character string, load the appropriate models
  if (is.null(results_models) || is.character(results_models)) {
    if (is.character(results_models)) {
      valid_timings <- c("early", "late", "after_quali")
      if (!results_models %in% valid_timings) {
        cli::cli_abort(
          "{.arg results_models} must be one of {.val {valid_timings}} when provided as a string."
        )
      }
      model_timing <- results_models
    } else {
      # is.null(results_models), so auto-detect
      model_timing <- if (any(grepl("q_.*_perc", names(new_data)))) {
        "after_quali"
      } else if (any(grepl("practice", names(new_data)))) {
        "late"
      } else {
        "early"
      }
    }
    cli::cli_inform(
      "Loading '{model_timing}' results models for engine {.val {engine}} from disk."
    )
    results_models <- load_models(
      model_type = "results",
      model_timing = model_timing,
      engine = engine
    )
  }

  # Check if all required models are in the list
  required_models <- c(
    "win",
    "podium",
    "t10",
    "position",
    "position_class"
  )
  if (!all(required_models %in% names(results_models))) {
    cli::cli_abort(
      "The {.arg results_models} list must contain: {.val {required_models}}"
    )
  }

  win_preds <- predict_winner(new_data, results_models$win)
  podium_preds <- predict_podium(new_data, results_models$podium)
  t10_preds <- predict_t10(new_data, results_models$t10)
  position_preds <- predict_position(new_data, results_models$position)

  # For ensemble models, the ordinal classification model (polr) was trained
  # using the predictions from the other ensemble models as features. We need
  # to replicate that here by adding those predictions to the new_data frame.
  data_for_class_model <- if (engine == "ensemble") {
    cli::cli_inform(
      "Adding ensemble predictions as features for the ordinal model."
    )
    # Get the raw probability for win and numeric prediction for position
    win_ensemble_preds <- stats::predict(
      results_models$win,
      new_data,
      type = "prob"
    )
    pos_ensemble_preds <- stats::predict(
      results_models$position,
      new_data,
      type = "numeric"
    )

    # Add them as new columns with the names expected by the polr model's recipe
    new_data %>%
      dplyr::mutate(
        ensemble_win_pred = win_ensemble_preds$.pred_1,
        ensemble_pos_pred = pos_ensemble_preds$.pred
      )
  } else {
    # For single-engine models, no extra features are needed
    new_data
  }

  position_class_preds <- predict_position_class(
    data_for_class_model,
    results_models$position_class
  )

  all_preds <- win_preds %>%
    dplyr::left_join(podium_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(t10_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(position_preds, by = c("driver_id", "round", "season")) %>%
    dplyr::left_join(
      position_class_preds,
      by = c("driver_id", "round", "season")
    )
  return(all_preds)
}
