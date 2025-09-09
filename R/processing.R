#' default parameters
#' Just a list of default parameters
#' @keywords internal
default_params <- list(
  'grid' = 16.9,
  'position' = 14.9,
  'driver_finish_avg' = 1 - (0.09253282 + 0.1461158 / 2),
  'driver_failure_avg' = 0.09253282,
  'constructor_failure_avg' = 0.1461158 / 2,
  'grid_pos_corr_avg' = 0.5682137,
  'constructor_pit_duration_perc' = 1.50,
  'quali_avg_perc' = 1.014262,
  'fastest_pit' = 2.5,
  'pos_change' = 1.56621,
  'qgap' = 1.5,
  'constructor_pit_num_perc' = 1
)


# Data Processing Functions

#' Process Raw Race Results Data
#'
#' @param results Raw results data frame.
#' @param rgrid Raw qualifying grid data frame.
#' @return A processed data frame of race results.
#' @noRd
process_results_data <- function(input) {
  # ---- 1. Process Data for Joining ----
  # Prepare qualifying results to be joined with race results later.
  rg2 <- input$rgrid
  rg2$quali_position <- rg2$position
  rg2$driver_id <- rg2$quali_results

  # ---- 2. Process Race Results ----
  processed_results <- input$results %>% # Start with the main race results data
    dplyr::arrange(.data$season, .data$round, .data$position) %>% # Ensure chronological order
    dplyr::filter(
      !(.data$status == "Did not qualify" |
        .data$status == "Did not prequalify")
    ) %>%
    # Update Team Names:
    dplyr::mutate(
      constructor_id = dplyr::case_match(
        .data$constructor_id,
        c("tyrrell", "bar", "honda", "brawn") ~ "mercedes",
        c("benetton", "renault") ~ "alpine",
        "lotus_racing" ~ "caterham",
        "footwork" ~ "arrows",
        c(
          "jordan",
          "midland",
          "spyker",
          "spyker_mf1",
          "force_india",
          "racing_point"
        ) ~
          "aston_martin",
        c("jaguar", "stewart") ~ "red_bull",
        c("minardi", "toro_rosso") ~ "alphatauri",
        c("lotus_f1", "renault") ~ "alpine",
        "ligier" ~ "prost",
        c("marussia", "virgin") ~ "manor",
        "osella" ~ "fondmetal",
        c("sauber", "kick", "stake", "audi") ~ "alfa",
        .default = .data$constructor_id
      )
    ) %>%
    # Calculate positions gained or lost during the race.
    dplyr::mutate(pos_change = .data$grid - .data$position) %>%
    # Calculate cumulative points for a driver within a season.
    dplyr::group_by(.data$season, .data$driver_id) %>%
    dplyr::mutate(
      points_after = cumsum(.data$points),
      points_before = .data$points_after - .data$points
    ) %>%
    dplyr::ungroup() %>%
    # Calculate a driver's career experience in terms of races competed.
    dplyr::group_by(.data$driver_id) %>%
    dplyr::mutate(driver_experience = 0:(dplyr::n() - 1)) %>%
    dplyr::ungroup() %>% # Calculate race-specific metrics.
    dplyr::group_by(.data$season, .data$round) %>%
    dplyr::mutate(
      pos_change_perc = dplyr::if_else(
        .data$pos_change >= 0,
        .data$pos_change / .data$grid,
        .data$pos_change / (dplyr::n() - .data$grid)
      ),
      weighted_passes = (.data$grid - .data$position) / .data$grid,
      # Handle cases where grid position is 0 (e.g., pit lane start).
      grid = dplyr::if_else(.data$grid != 0, .data$grid, dplyr::n()),
      modern_points = expand_val(
        c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1),
        dplyr::n(),
        0
      ),
      grid_points = expand_val(
        c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1),
        dplyr::n(),
        0
      )[.data$grid],
      pos_change_points = .data$modern_points - .data$grid_points
    ) %>%
    dplyr::ungroup() %>%
    # Create binary flags based on the 'status' column.
    dplyr::mutate(
      finished = dplyr::if_else(
        grepl("Finished|\\+(\\d+) Lap|Lapped", .data$status),
        1,
        0
      ),
      driver_failure = dplyr::if_else(
        .data$status %in%
          c(
            "Damage",
            "Excluded",
            "Illness",
            "Injury",
            "Collision damage",
            "Did not qualify",
            "Disqualified",
            "Did not start",
            "Not classified",
            "Injured",
            "Spun off",
            "Accident",
            "Collision"
          ),
        1,
        0
      ),
      constructor_failure = dplyr::if_else(
        .data$status %in%
          c(
            "Gearbox",
            "Suspension",
            "Fuel pressure",
            "Radiator",
            "Mechanical",
            "Engine",
            "Electrical",
            "Fuel system",
            "Driveshaft",
            "Oil line",
            "Fuel rig",
            "Withdrew",
            "Launch control",
            "Clutch",
            "Hydraulics",
            "Brakes",
            "Electronics",
            "Fuel",
            "Out of fuel",
            "Oil leak",
            "Tyre",
            "Transmission",
            "Pneumatics",
            "Steering",
            "Throttle",
            "Handling",
            "Puncture",
            "Rear wing",
            "Fire",
            "Wheel",
            "Overheating",
            "Wheel rim",
            "Engine fire",
            "Retired",
            "Tyre puncture",
            "Wheel nut",
            "Broken wing",
            "Heat shield fire",
            "Exhaust",
            "Technical",
            "Water leak",
            "Fuel pump",
            "Track rod",
            "Oil pressure",
            "Front wing",
            "Water pressure",
            "Refuelling",
            "Driver Seat",
            "Differential",
            "Engine misfire",
            "Vibrations",
            "Alternator",
            "ERS",
            "Power Unit",
            "Turbo",
            "Drivetrain",
            "Power loss",
            "Brake duct",
            "Battery",
            "Seat",
            "Spark plugs",
            "Debris",
            "Cooling system",
            "Water pump",
            "Fuel leak",
            "Undertray",
            "Physical",
            "Distributor",
            "Chassis",
            "Wheel bearing",
            "Halfshaft",
            "Ignition",
            "Injection",
            "Safety belt",
            "Oil pump",
            "Underweight",
            "Safety concerns",
            "Not restarted",
            "Stalled",
            "Crankshaft",
            "Safety"
          ),
        1,
        0
      )
    ) %>% # Join the qualifying position from the prepared `rg2` data.
    dplyr::left_join(
      rg2[, c("quali_position", "driver_id", "season", "round")],
      by = c("driver_id", "season", "round")
    ) %>%
    # Select and rename the final columns for this section.
    dplyr::select(
      "driver_id",
      "constructor_id",
      "position",
      "grid",
      "quali_position",
      "pos_change",
      "pos_change_perc",
      "weighted_passes",
      "pos_change_points",
      "fastest_rank",
      fastest_time = "time_sec",
      "points",
      "points_before",
      "status",
      "points_after",
      "driver_experience",
      "season",
      "round",
      "driver_failure",
      "constructor_failure",
      "finished"
    ) %>%
    janitor::clean_names()

  return(processed_results)
}

#' Process Raw Lap Time Data
#'
#' @param laps Raw laps data frame.
#' @return A processed data frame of lap times with ranks and gaps.
#' @noRd
process_lap_times <- function(laps) {
  # ---- 3. Process Lap Times ----
  processed_laps <- laps %>%
    dplyr::arrange(.data$season, .data$round) %>%
    dplyr::filter(.data$deleted != TRUE) %>%
    # For each driver in each session, find their best lap time and total laps.
    dplyr::group_by(
      .data$season,
      .data$round,
      .data$session_type,
      .data$driver_id
    ) %>%
    dplyr::mutate(
      best_time = dplyr::if_else(
        is.infinite(min(.data$lap_time, na.rm = T)),
        NA_real_,
        min(.data$lap_time, na.rm = T)
      ),
      num_laps = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    suppressWarnings() %>% # For each session, find the overall best time and most laps.
    dplyr::group_by(.data$season, .data$round, .data$session_type) %>%
    dplyr::mutate(
      'session_best' = min(.data$best_time, na.rm = TRUE),
      'session_most_laps' = max(.data$num_laps, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    # Calculate metrics relative to the session bests (e.g., gap, percentage).
    # Also calculate an "optimal time" by summing a driver's best sector times.
    dplyr::group_by(
      .data$season,
      .data$round,
      .data$session_type,
      .data$driver_id
    ) %>%
    dplyr::mutate(
      gap_to_best = .data$best_time - .data$session_best,
      perc_to_best = .data$best_time / .data$session_best,
      perc_num_laps = .data$num_laps / .data$session_most_laps,
      best_s1 = suppressWarnings(min(.data$sector1time, na.rm = T)),
      best_s2 = suppressWarnings(min(.data$sector2time, na.rm = T)),
      best_s3 = suppressWarnings(min(.data$sector3time, na.rm = T)),
      optimal_time = .data$best_s1 + .data$best_s2 + .data$best_s3,
      optimal_time = dplyr::if_else(
        is.infinite(.data$optimal_time),
        NA,
        .data$optimal_time
      )
    ) %>%
    dplyr::ungroup() %>%
    # Select the relevant columns.
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "session_type",
      "best_time",
      "num_laps",
      "gap_to_best",
      "perc_to_best",
      "perc_num_laps",
      "optimal_time"
    ) %>%
    unique() %>%
    dplyr::group_by(.data$season, .data$round, .data$session_type) %>% # Rank drivers within each session.
    dplyr::mutate(
      optimal_time = tidyr::replace_na(
        .data$optimal_time,
        max(.data$optimal_time, na.rm = TRUE) + 0.5
      ),
      best_time = tidyr::replace_na(
        .data$best_time,
        max(.data$best_time, na.rm = TRUE) + 0.5
      ),
      rank = dplyr::dense_rank(.data$best_time),
      optimal_rank = dplyr::dense_rank(.data$optimal_time)
    ) %>%
    dplyr::mutate(
      rank = tidyr::replace_na(.data$rank, dplyr::n()),
      optimal_rank = tidyr::replace_na(.data$optimal_rank, dplyr::n())
    ) %>%
    dplyr::ungroup() %>%
    janitor::clean_names()
  return(processed_laps)
}

#' Summarize Practice Lap Data
#'
#' @param processed_laps A data frame of processed lap times from `process_lap_times`.
#' @return A data frame summarizing performance across practice sessions.
#' @noRd
summarize_practice_laps <- function(processed_laps) {
  # ---- 4. Summarize Practice Session Data ----
  practices <- processed_laps %>%
    dplyr::arrange(.data$season, .data$round) %>%
    # Filter for practice sessions only.
    dplyr::filter(.data$session_type %in% c("FP1", "FP2", "FP3")) %>%
    dplyr::group_by(.data$season, .data$round, .data$driver_id) %>%
    # Summarize performance across all practice sessions for a race weekend.
    dplyr::mutate(
      practice_avg_rank = suppressWarnings(mean(.data$rank, na.rm = T)),
      practice_best_rank = suppressWarnings(min(.data$rank, na.rm = T)),
      practice_num_laps = suppressWarnings(sum(.data$num_laps, na.rm = T)),
      practice_avg_gap = suppressWarnings(mean(.data$gap_to_best, na.rm = T)),
      practice_best_gap = suppressWarnings(min(.data$gap_to_best, na.rm = T)),
      practice_optimal_rank = suppressWarnings(mean(
        .data$optimal_rank,
        na.rm = T
      ))
    ) %>%
    # Handle infinite values that can result from sessions with no times
    dplyr::mutate(
      practice_avg_rank = dplyr::if_else(
        is.infinite(.data$practice_avg_rank),
        NA,
        .data$practice_avg_rank
      ),
      practice_best_rank = dplyr::if_else(
        is.infinite(.data$practice_best_rank),
        NA,
        .data$practice_best_rank
      ),
      practice_avg_gap = dplyr::if_else(
        is.infinite(.data$practice_avg_gap),
        NA,
        .data$practice_avg_gap
      ),
      practice_best_gap = dplyr::if_else(
        is.infinite(.data$practice_best_gap),
        NA,
        .data$practice_best_gap
      ),
      practice_optimal_rank = dplyr::if_else(
        is.infinite(.data$practice_optimal_rank),
        NA,
        .data$practice_optimal_rank
      )
    ) %>% # Impute remaining NA values with reasonable defaults (e.g., last rank).
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$round) %>%
    dplyr::mutate(
      practice_avg_rank = tidyr::replace_na(
        .data$practice_avg_rank,
        dplyr::n()
      ),
      practice_best_rank = tidyr::replace_na(
        .data$practice_best_rank,
        dplyr::n()
      ),
      practice_num_laps = tidyr::replace_na(.data$practice_num_laps, 0),
      practice_avg_gap = tidyr::replace_na(
        .data$practice_avg_gap,
        max(c(.data$practice_avg_gap, 1.5), na.rm = TRUE) + 0.1
      ),
      practice_best_gap = tidyr::replace_na(
        .data$practice_best_gap,
        max(c(.data$practice_avg_gap, 1.0), na.rm = TRUE) + 0.1
      ),
      practice_optimal_rank = tidyr::replace_na(
        .data$practice_optimal_rank,
        dplyr::n()
      )
    ) %>%
    dplyr::ungroup() %>%
    # Select final columns.
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "practice_avg_rank",
      "practice_best_rank",
      "practice_num_laps",
      "practice_avg_gap",
      "practice_best_gap",
      "practice_optimal_rank"
    ) %>%
    unique() %>%
    janitor::clean_names()
  return(practices)
}

#' Process Raw Qualifying Time Data
#'
#' @param qualis Raw qualis data frame.
#' @return A processed data frame of qualifying times with gaps and rolling averages.
#' @noRd
process_quali_times <- function(qualis) {
  # ---- 5. Process Qualifying Session Data ----
  processed_qualis <- qualis %>%
    dplyr::arrange(.data$season, .data$round) %>%
    # Select raw qualifying times.
    dplyr::select(
      "driver_id",
      "q1_sec",
      "q2_sec",
      "q3_sec",
      "season",
      "round"
    ) %>%
    # Calculate each driver's time as a percentage of the session's fastest time.
    dplyr::group_by(.data$season, .data$round) %>%
    dplyr::mutate(
      q1_perc = .data$q1_sec / min(.data$q1_sec, na.rm = T),
      q2_perc = .data$q2_sec / min(.data$q2_sec, na.rm = T),
      q3_perc = .data$q3_sec / min(.data$q3_sec, na.rm = T)
    ) %>%
    dplyr::mutate(
      # Find the best percentage gap (q_min_perc) across Q1, Q2, Q3.
      q1_perc = tidyr::replace_na(.data$q1_perc, 1.07),
      q_min_perc = pmin(.data$q1_perc, .data$q2_perc, .data$q3_perc, na.rm = T)
    ) %>%
    # Calculate the time gap to the fastest driver in each session.
    dplyr::mutate(
      q_min_perc = tidyr::replace_na(.data$q_min_perc, 1.07),
      q1gap = .data$q1_sec - min(.data$q1_sec, na.rm = T),
      q2gap = .data$q2_sec - min(.data$q2_sec, na.rm = T),
      q3gap = .data$q3_sec - min(.data$q3_sec, na.rm = T),
      qgap = dplyr::case_when(
        !is.na(.data$q3_sec) ~ .data$q3gap,
        !is.na(.data$q2_sec) ~ .data$q2gap,
        !is.na(.data$q1_sec) ~ .data$q1gap,
        TRUE ~ max(q1gap, na.rm = T) + 0.1
      ),
      qgap = dplyr::if_else(
        .data$qgap > 5,
        max(.data$qgap[.data$qgap < 5], na.rm = T) + 0.1,
        .data$qgap
      )
    ) %>%
    dplyr::mutate(
      qgap = tidyr::replace_na(.data$qgap, max(.data$qgap, na.rm = T) + 0.1)
    ) %>%
    # Calculate the average percentage gap across all quali sessions attended.
    dplyr::rowwise() %>%
    dplyr::mutate(
      q_avg_perc = mean(
        dplyr::c_across(c("q1_perc", "q2_perc", "q3_perc")),
        na.rm = T
      ),
      q_avg_perc = tidyr::replace_na(
        .data$q_avg_perc,
        default_params$quali_avg_perc
      )
    ) %>%
    dplyr::ungroup() %>%
    # Calculate a driver's rolling average qualifying gap over the last 5 races.
    dplyr::group_by(.data$driver_id) %>%
    dplyr::mutate(
      driver_avg_qgap = as.numeric(slider::slide(
        .data$qgap,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$qgap,
        .before = 5
      ))
    ) %>%
    dplyr::ungroup() %>%
    suppressWarnings() %>%
    unique() %>%
    janitor::clean_names()
  return(processed_qualis)
}

#' Process Raw Pit Stop Data
#'
#' @param pitstops Raw pitstops data frame.
#' @return A processed data frame of pit stop performance.
#' @noRd
process_pit_stops <- function(pitstops) {
  # ---- 6. Process Pit Stop Data ----
  processed_pitstops <- pitstops %>%
    dplyr::arrange(.data$season, .data$round) %>%
    dplyr::select("driver_id", "stop", "duration", "season", "round") %>%
    # For each driver in a race, count their total number of stops.
    dplyr::group_by(.data$season, .data$round, .data$driver_id) %>%
    dplyr::mutate(stops = dplyr::n()) %>%
    dplyr::ungroup(.data$driver_id) %>%
    # Adjust pit stop durations relative to the fastest stop in the race.
    # TODO: Unless a fastest stop time can be found online
    dplyr::mutate(
      stops = tidyr::replace_na(.data$stops, 0),
      adj_duration = .data$duration -
        (min(.data$duration, na.rm = T)) +
        default_params$fastest_pit,
      pit_duration_perc = .data$adj_duration /
        min(.data$adj_duration, na.rm = T)
    ) %>%
    # Select and rename columns.
    dplyr::select(
      "driver_id",
      pit_stops = "stops",
      pit_duration = "adj_duration",
      "pit_duration_perc",
      "season",
      "round"
    ) %>%
    # Calculate average pit duration for drivers with multiple stops.
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season, .data$round, .data$driver_id) %>%
    dplyr::mutate(pit_duration_avg = mean(.data$pit_duration, na.rm = TRUE)) %>%
    dplyr::ungroup(.data$driver_id) %>%
    dplyr::mutate(
      pit_duration_perc = .data$pit_duration_avg /
        min(.data$pit_duration, na.rm = TRUE)
    ) %>%
    # Select final columns for this section.
    dplyr::ungroup() %>%
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "pit_duration_perc",
      "pit_stops"
    ) %>%
    unique() %>%
    # Calculate the number of stops as a percentage of the race average.
    dplyr::group_by(.data$season, .data$round) %>%
    dplyr::mutate(
      pit_num_perc = .data$pit_stops / mean(.data$pit_stops, na.rm = T)
    ) %>%
    dplyr::mutate(
      pit_num_perc = tidyr::replace_na(.data$pit_num_perc, 0),
      pit_duration_perc = tidyr::replace_na(.data$pit_duration_perc, 2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "pit_duration_perc",
      "pit_num_perc"
    ) %>%
    unique() %>%
    janitor::clean_names()
  return(processed_pitstops)
}

#' Create Constructor-Level Features
#'
#' @param results Processed results data from `process_results_data`.
#' @param pitstops Processed pitstop data from `process_pit_stops`.
#' @return A data frame with historical constructor-level features.
#' @noRd
create_constructor_features <- function(results, pitstops) {
  # ---- 7. Create Constructor-Level Features ----
  constructor_results <- results %>%
    dplyr::left_join(pitstops, by = c("round", "season", "driver_id")) %>%
    # For each constructor in each race, summarize key performance metrics.
    dplyr::group_by(.data$season, .data$round, .data$constructor_id) %>% # Summarize by constructor for each race
    dplyr::summarise(
      constructor_best_grid = min(.data$grid, na.rm = T),
      constructor_best_finish = min(.data$position, na.rm = T),
      constructor_failure_race = mean(.data$constructor_failure, na.rm = T),
      constructor_pit_duration_perc = mean(.data$pit_duration_perc, na.rm = T),
      constructor_pit_num_perc = mean(.data$pit_num_perc)
    ) %>%
    dplyr::mutate(
      constructor_best_grid = tidyr::replace_na(.data$constructor_best_grid, 19)
    ) %>%
    dplyr::ungroup() %>%
    # Calculate rolling historical averages for constructor performance.
    dplyr::group_by(.data$constructor_id) %>%
    dplyr::mutate(
      constructor_grid_avg = as.numeric(slider::slide(
        .data$constructor_best_grid,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$grid,
        .before = 10
      )),
      constructor_finish_avg = as.numeric(slider::slide(
        .data$constructor_best_finish,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$position,
        .before = 10
      )),
      constructor_failure_avg = as.numeric(slider::slide(
        .data$constructor_failure_race,
        s_lagged_cumwmean_expanded,
        ln = 10,
        val = default_params$constructor_failure_avg * 2,
        .before = 20
      )),
      constructor_pit_duration_avg = as.numeric(slider::slide(
        .data$constructor_pit_duration_perc,
        s_lagged_cumwmean_expanded,
        ln = 8,
        val = default_params$constructor_pit_duration_perc,
        .before = 20
      )),
      constructor_pit_num_avg = as.numeric(slider::slide(
        .data$constructor_pit_num_perc,
        s_lagged_cumwmean_expanded,
        ln = 8,
        val = default_params$constructor_pit_num_perc,
        .before = 20
      ))
    ) %>%
    dplyr::ungroup() %>%
    # Select final constructor feature columns.
    dplyr::select(
      "constructor_id",
      "season",
      "round",
      "constructor_grid_avg",
      "constructor_finish_avg",
      "constructor_failure_race",
      "constructor_failure_avg",
      "constructor_pit_duration_perc",
      "constructor_pit_num_perc",
      "constructor_pit_duration_avg",
      "constructor_pit_num_avg"
    ) %>%
    unique() %>%
    janitor::clean_names()
  return(constructor_results)
}

#' Create Circuit-Specific Features
#'
#' @param results A data frame containing processed results with failure flags.
#' @return A data frame with historical circuit-specific features.
#' @noRd
create_circuit_features <- function(results) {
  # This function calculates rolling historical averages for each circuit.
  circuit_summary <- results %>%
    dplyr::select(
      "round",
      "season",
      "circuit_id",
      "driver_failure",
      "constructor_failure",
      "grid",
      "position"
    ) %>%
    dplyr::group_by(.data$round, .data$season, .data$circuit_id) %>%
    dplyr::summarise(
      driver_failure_circuit = mean(.data$driver_failure, na.rm = T),
      constructor_failure_circuit = mean(.data$constructor_failure, na.rm = T),
      grid_pos_corr = stats::cor(.data$grid, .data$position),
      .groups = "drop"
    )

  circuit_features <- circuit_summary %>%
    dplyr::group_by(.data$circuit_id) %>%
    dplyr::mutate(
      driver_failure_circuit_avg = as.numeric(slider::slide(
        .data$driver_failure_circuit,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$driver_failure_avg,
        .before = 5
      )),
      constructor_failure_circuit_avg = as.numeric(slider::slide(
        .data$constructor_failure_circuit,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$constructor_failure_avg * 2,
        .before = 5
      )),
      grid_pos_corr_avg = as.numeric(slider::slide(
        .data$grid_pos_corr,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = default_params$grid_pos_corr_avg,
        .before = 5
      ))
    ) %>%
    dplyr::ungroup()

  return(circuit_features)
}

#' Combine and Finalize All Features
#'
#' @param results Processed results data.
#' @param qualis Processed quali data.
#' @param practices Processed practice data.
#' @param pitstops Processed pitstop data.
#' @param constructor_results Processed constructor data.
#' @param schedule Raw schedule data.
#' @return A final, cleaned data frame ready for modeling.
#' @noRd
combine_and_finalize_features <- function(
  results,
  qualis,
  practices,
  pitstops,
  constructor_results,
  schedule
) {
  # ---- 9. Combine All Data and Create Final Features ----
  results <- results %>%
    dplyr::arrange(.data$season, .data$round, .data$position) %>%
    # Join all the previously created data frames.
    dplyr::left_join(qualis, by = c("round", "season", "driver_id")) %>%
    dplyr::left_join(practices, by = c("round", "season", "driver_id")) %>%
    dplyr::left_join(pitstops, by = c("round", "season", "driver_id")) %>%
    dplyr::left_join(
      constructor_results,
      by = c("round", "season", "constructor_id")
    )

  # Process and join schedule data
  schedule_clean <- schedule %>%
    dplyr::select("circuit_id", "season", "round") %>%
    dplyr::mutate(
      "season" = as.integer(.data$season),
      "round" = as.integer(.data$round)
    )
  results <- results %>%
    dplyr::left_join(schedule_clean, by = c("round", "season")) %>%
    # Create final driver-specific rolling average features.
    dplyr::group_by(.data$driver_id) %>%
    dplyr::mutate(
      driver_pos_change_avg = as.numeric(slider::slide(
        .data$pos_change,
        s_lagged_cumwmean_expanded,
        ln = 8,
        val = default_params$pos_change,
        .before = 5
      )),
      driver_weighted_pass_avg = as.numeric(slider::slide(
        .data$weighted_passes,
        s_lagged_cumwmean_expanded,
        ln = 8,
        val = 0,
        .before = 5
      )),
      driver_points_change_avg = as.numeric(slider::slide(
        .data$pos_change_points,
        s_lagged_cumwmean_expanded,
        ln = 5,
        val = 0,
        .before = 5
      )),
      driver_failure = tidyr::replace_na(.data$driver_failure, 0),
      driver_failure_avg = as.numeric(slider::slide(
        .data$driver_failure,
        s_lagged_cumwmean_expanded,
        val = default_params$driver_failure_avg,
        ln = 10,
        .before = 20
      )),
      driver_grid_avg = as.numeric(slider::slide(
        .data$grid,
        s_lagged_cumwmean_expanded,
        val = default_params$grid,
        ln = 8,
        .before = 10
      )),
      driver_position_avg = as.numeric(slider::slide(
        .data$position,
        s_lagged_cumwmean_expanded,
        val = default_params$position,
        ln = 8,
        .before = 10
      )),
      driver_finish_avg = as.numeric(slider::slide(
        .data$finished,
        s_lagged_cumwmean_expanded,
        val = default_params$driver_finish_avg,
        ln = 8,
        .before = 10
      )),
      practice_optimal_rank = tidyr::replace_na(
        .data$practice_optimal_rank,
        default_params$position
      ),
      driver_practice_optimal_rank_avg = as.numeric(
        slider::slide(
          .data$practice_optimal_rank,
          s_lagged_cumwmean_expanded,
          val = default_params$position,
          ln = 8,
          .before = 10
        )
      ),
      driver_avg_qgap = tidyr::replace_na(
        .data$driver_avg_qgap,
        mean(.data$driver_avg_qgap, na.rm = TRUE)
      ),
      driver_avg_qgap = tidyr::replace_na(
        .data$driver_avg_qgap,
        default_params$qgap
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      quali_position = dplyr::if_else(
        is.na(.data$quali_position),
        .data$grid,
        .data$quali_position
      )
    ) %>%
    janitor::clean_names()

  # ---- 10. Join Circuit Features and Final Imputation ----
  circuit_features <- create_circuit_features(results)

  final_data <- results %>%
    dplyr::left_join(
      circuit_features,
      by = c("round", "season", "circuit_id")
    ) %>%
    dplyr::mutate(round_id = paste0(.data$season, "-", .data$round)) %>%
    # Final NA imputation for remaining columns.
    dplyr::group_by(.data$season, .data$round) %>%
    dplyr::mutate(
      q_min_perc = tidyr::replace_na(
        .data$q_min_perc,
        mean(.data$q_min_perc, na.rm = TRUE)
      ),
      q_avg_perc = tidyr::replace_na(
        .data$q_avg_perc,
        mean(.data$q_avg_perc, na.rm = TRUE)
      ),
      practice_avg_rank = tidyr::replace_na(
        .data$practice_avg_rank,
        dplyr::n()
      ),
      practice_best_rank = tidyr::replace_na(
        .data$practice_best_rank,
        dplyr::n()
      ),
      practice_best_rank = dplyr::if_else(
        is.infinite(.data$practice_best_rank),
        dplyr::n(),
        .data$practice_best_rank
      ),
      practice_avg_gap = tidyr::replace_na(
        .data$practice_avg_gap,
        mean(.data$practice_avg_gap, na.rm = TRUE)
      ),
      practice_best_gap = dplyr::if_else(
        is.infinite(.data$practice_best_gap),
        NA_real_,
        .data$practice_best_gap
      ),
      practice_best_gap = tidyr::replace_na(
        .data$practice_best_gap,
        mean(.data$practice_best_gap, na.rm = TRUE)
      ),
      pit_duration_perc = tidyr::replace_na(
        .data$pit_duration_perc,
        mean(.data$pit_duration_perc, na.rm = TRUE)
      ),
      pit_num_perc = tidyr::replace_na(
        .data$pit_num_perc,
        0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$season, .data$round) %>%
    janitor::clean_names()

  return(final_data)
}


#' Clean Data
#'
#' This function serves as a wrapper to load all raw data and process it through
#' a series of cleaning and feature engineering steps, preparing it for modeling.
#'
#' @param input A list of raw data frames, typically from `load_all_data()`.
#' @return A single, cleaned data frame.
#' @importFrom rlang .data
#' @export
clean_data <- function(input = load_all_data()) {
  nseasons <- length(unique(input$results$season))
  cli::cli_inform(
    "Loaded {nseasons} seasons of data, now processing"
  )
  # 1. Process individual data sources
  results_processed <- process_results_data(input)
  laps_processed <- process_lap_times(input$laps)
  practices_summarized <- summarize_practice_laps(laps_processed)
  qualis_processed <- process_quali_times(input$qualis)
  pitstops_processed <- process_pit_stops(input$pitstops)
  constructor_features <- create_constructor_features(
    results_processed,
    pitstops_processed
  )

  # 2. Combine all processed data and finalize features
  final_data <- combine_and_finalize_features(
    results = results_processed,
    qualis = qualis_processed,
    practices = practices_summarized,
    pitstops = pitstops_processed,
    constructor_results = constructor_features,
    schedule = f1predicter::schedule
  )
  cli::cli_inform("Returning {nrow(final_data)} rows of data.")
  return(final_data)
}
