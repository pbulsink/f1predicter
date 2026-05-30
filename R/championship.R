# Championship odds simulation using Monte Carlo methods.
# Inspired by https://github.com/jeppeolesen/f1-championship-odds

# --- Points Systems ---

#' F1 GP Points System
#'
#' Returns the standard F1 Grand Prix points awarded to positions 1-10.
#'
#' @return A named numeric vector of points per finishing position.
#' @noRd
gp_points_system <- function() {
  c(
    "1" = 25, "2" = 18, "3" = 15, "4" = 12, "5" = 10,
    "6" = 8, "7" = 6, "8" = 4, "9" = 2, "10" = 1
  )
}

#' F1 Sprint Points System
#'
#' Returns the F1 sprint race points awarded to positions 1-8.
#'
#' @return A named numeric vector of points per finishing position.
#' @noRd
sprint_points_system <- function() {
  c(
    "1" = 8, "2" = 7, "3" = 6, "4" = 5,
    "5" = 4, "6" = 3, "7" = 2, "8" = 1
  )
}

#' Get Points for a Finishing Position
#'
#' @param position Integer finishing position.
#' @param type Character, either "race" or "sprint".
#' @return Numeric points awarded (0 if outside points-scoring positions or DNF).
#' @noRd
get_points_for_position <- function(position, type = "race") {
  if (is.na(position) || position < 1) return(0)
  pts <- if (type == "sprint") sprint_points_system() else gp_points_system()
  pos_char <- as.character(as.integer(position))
  if (pos_char %in% names(pts)) pts[[pos_char]] else 0
}


# --- Current Standings ---

#' Get Current Championship Standings
#'
#' @description
#' Retrieves the current driver championship standings from `f1dataR`.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#'
#' @return A tibble with columns `driver_id`, `points`, and `position`.
#' @export
#' @examples
#' \dontrun{
#' standings <- get_current_standings(2025)
#' }
get_current_standings <- function(season = as.numeric(f1dataR::get_current_season())) {
  standings <- tryCatch(
    f1dataR::load_standings(season = season, round = "last", type = "driver"),
    error = function(e) {
      cli::cli_abort(
        "Could not load championship standings for {season}: {e$message}"
      )
    }
  )

  standings %>%
    dplyr::transmute(
      driver_id = .data$driver_id,
      points = as.numeric(.data$points),
      position = as.numeric(.data$position)
    ) %>%
    dplyr::arrange(.data$position)
}


# --- Remaining Schedule ---

#' Get Remaining Races in the Season
#'
#' @description
#' Returns the remaining race rounds (and which have sprints) for the current
#' season from the package schedule data.
#'
#' @param season Numeric season year.
#' @param after_round Numeric round number. Returns races after this round.
#'   If `NULL` (default), uses the current date to determine remaining races.
#'
#' @return A tibble with columns `round`, `race_name`, `date`, `has_sprint`.
#' @export
#' @examples
#' \dontrun{
#' remaining <- get_remaining_schedule(2025)
#' }
get_remaining_schedule <- function(
    season = as.numeric(f1dataR::get_current_season()),
    after_round = NULL
) {
  schedule <- f1predicter::schedule %>%
    dplyr::filter(.data$season == !!season) %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      has_sprint = !is.na(.data$sprint_date)
    )

  if (is.null(after_round)) {
    schedule <- schedule %>%
      dplyr::filter(.data$date >= Sys.Date())
  } else {
    schedule <- schedule %>%
      dplyr::filter(.data$round > after_round)
  }

  schedule %>%
    dplyr::select("round", "race_name", "date", "has_sprint") %>%
    dplyr::arrange(.data$round)
}


# --- Performance Metrics ---

#' Calculate Driver Performance Metrics
#'
#' @description
#' Calculates performance metrics for each driver based on recent race history,
#' including average finishing position, standard deviation (consistency), and
#' DNF rate. Uses a weighted approach: 70% weight on last 5 races, 30% on full
#' season, inspired by the Jeppe Olesen approach.
#'
#' @param season Numeric season year.
#' @param historical_data Historical race data from `clean_data()`. If `NULL`,
#'   uses the package's internal data loading.
#'
#' @return A tibble with columns `driver_id`, `avg_position`, `position_sd`,
#'   `dnf_rate`, `recent_avg_position`, `weighted_avg_position`.
#' @noRd
calculate_driver_performance <- function(
    season,
    historical_data = NULL
) {
  if (is.null(historical_data)) {
    historical_data <- clean_data()
  }

  season_data <- historical_data %>%
    dplyr::filter(.data$season == !!season)

  if (nrow(season_data) == 0) {
    cli::cli_abort("No historical data found for season {season}.")
  }

  # Calculate metrics per driver
  driver_metrics <- season_data %>%
    dplyr::group_by(.data$driver_id) %>%
    dplyr::arrange(.data$round) %>%
    dplyr::mutate(
      dnf = as.numeric(!.data$finished | is.na(.data$position) | .data$position > 20)
    ) %>%
    dplyr::summarise(
      n_races = dplyr::n(),
      avg_position = mean(.data$position, na.rm = TRUE),
      position_sd = stats::sd(.data$position, na.rm = TRUE),
      dnf_rate = mean(.data$dnf, na.rm = TRUE),
      # Recent performance: last 5 races
      recent_avg_position = mean(
        utils::tail(.data$position[!is.na(.data$position)], 5),
        na.rm = TRUE
      ),
      recent_sd = stats::sd(
        utils::tail(.data$position[!is.na(.data$position)], 5),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Handle edge cases
      position_sd = tidyr::replace_na(.data$position_sd, 5),
      recent_sd = tidyr::replace_na(.data$recent_sd, 5),
      # Weighted average: 70% recent, 30% full season
      weighted_avg_position = 0.7 * .data$recent_avg_position +
        0.3 * .data$avg_position
    )

  driver_metrics
}


# --- Monte Carlo Simulation ---

#' Simulate Championship Odds
#'
#' @description
#' Uses Monte Carlo simulation to predict the probability of each driver winning
#' the championship based on current standings and performance metrics. This is
#' inspired by the approach in
#' \url{https://github.com/jeppeolesen/f1-championship-odds}.
#'
#' @details
#' The simulation works as follows:
#' \enumerate{
#'   \item Starts with current championship points.
#'   \item For each remaining race (and sprint, if applicable):
#'     \itemize{
#'       \item Applies a DNF probability for each driver (50% lower for sprints).
#'       \item Predicts finishing position based on weighted performance average
#'         plus random noise from the driver's consistency (standard deviation).
#'       \item Awards points based on session type.
#'     }
#'   \item Determines the championship winner.
#'   \item Repeats for `n_simulations` iterations.
#'   \item Returns win probabilities as the fraction of simulations won.
#' }
#'
#' Only drivers in mathematical contention are simulated. A driver is in
#' contention if the maximum possible points they can earn in remaining races
#' is enough to catch the current leader.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#' @param standings A tibble of current standings from `get_current_standings()`.
#'   If `NULL`, loaded automatically.
#' @param remaining A tibble of remaining schedule from `get_remaining_schedule()`.
#'   If `NULL`, loaded automatically.
#' @param n_simulations Integer number of Monte Carlo simulations to run.
#'   Defaults to 10000.
#' @param historical_data Historical race data. If `NULL`, uses `clean_data()`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{driver_id}{Driver identifier}
#'     \item{current_points}{Current championship points}
#'     \item{win_probability}{Probability of winning the championship (0 to 1)}
#'     \item{avg_final_points}{Average total points at season end across simulations}
#'     \item{avg_final_position}{Average final championship position across simulations}
#'     \item{in_contention}{Logical, whether the driver is mathematically in contention}
#'     \item{season}{The season year}
#'   }
#' @export
#' @examples
#' \dontrun{
#' odds <- simulate_championship_odds(season = 2025, n_simulations = 1000)
#' }
simulate_championship_odds <- function(
    season = as.numeric(f1dataR::get_current_season()),
    standings = NULL,
    remaining = NULL,
    n_simulations = 10000L,
    historical_data = NULL
) {
  # --- Input Validation ---
  stopifnot(is.numeric(season), length(season) == 1)

  stopifnot(is.numeric(n_simulations), n_simulations >= 1)
  n_simulations <- as.integer(n_simulations)

  # --- Load Data ---
  if (is.null(standings)) {
    cli::cli_inform("Loading current championship standings for {season}...")
    standings <- get_current_standings(season)
  }

  if (is.null(remaining)) {
    remaining <- get_remaining_schedule(season)
  }

  if (nrow(remaining) == 0) {
    cli::cli_inform("No remaining races. Returning current standings as final.")
    return(
      standings %>%
        dplyr::mutate(
          current_points = .data$points,
          win_probability = dplyr::if_else(.data$position == 1, 1, 0),
          avg_final_points = .data$points,
          avg_final_position = as.numeric(.data$position),
          in_contention = .data$position == 1,
          season = !!season
        ) %>%
        dplyr::select(
          "driver_id", "current_points", "win_probability",
          "avg_final_points", "avg_final_position", "in_contention", "season"
        )
    )
  }

  # --- Performance Metrics ---
  cli::cli_inform("Calculating driver performance metrics...")
  performance <- calculate_driver_performance(season, historical_data)

  # Merge standings with performance
  sim_data <- standings %>%
    dplyr::left_join(performance, by = "driver_id") %>%
    dplyr::mutate(
      # Defaults for drivers with no performance data
      weighted_avg_position = tidyr::replace_na(.data$weighted_avg_position, 15),
      position_sd = tidyr::replace_na(.data$position_sd, 5),
      dnf_rate = tidyr::replace_na(.data$dnf_rate, 0.1)
    )

  # --- Determine Mathematical Contention ---
  n_remaining_races <- nrow(remaining)
  n_remaining_sprints <- sum(remaining$has_sprint)
  max_race_points <- max(gp_points_system())
  max_sprint_points <- max(sprint_points_system())
  max_possible_remaining <- n_remaining_races * max_race_points +
    n_remaining_sprints * max_sprint_points

  leader_points <- max(sim_data$points)
  sim_data <- sim_data %>%
    dplyr::mutate(
      in_contention = (.data$points + max_possible_remaining) >= leader_points
    )

  contenders <- sim_data %>% dplyr::filter(.data$in_contention)

  if (nrow(contenders) == 0) {
    cli::cli_abort("No drivers in contention. This shouldn't happen.")
  }

  cli::cli_inform(
    "Simulating {n_simulations} seasons for {nrow(contenders)} contenders across {n_remaining_races} races and {n_remaining_sprints} sprints..."
  )

  # --- Run Simulations ---
  n_drivers <- nrow(contenders)
  win_counts <- stats::setNames(integer(n_drivers), contenders$driver_id)
  total_points_matrix <- matrix(
    0, nrow = n_simulations, ncol = n_drivers,
    dimnames = list(NULL, contenders$driver_id)
  )

  for (sim in seq_len(n_simulations)) {
    sim_points <- contenders$points

    for (race_idx in seq_len(nrow(remaining))) {
      race <- remaining[race_idx, ]

      # --- Sprint Simulation ---
      if (race$has_sprint) {
        sprint_positions <- simulate_race_positions(
          contenders$weighted_avg_position,
          contenders$position_sd,
          contenders$dnf_rate * 0.5, # 50% lower DNF rate for sprints
          n_drivers
        )
        for (d in seq_len(n_drivers)) {
          sim_points[d] <- sim_points[d] +
            get_points_for_position(sprint_positions[d], "sprint")
        }
      }

      # --- Race Simulation ---
      race_positions <- simulate_race_positions(
        contenders$weighted_avg_position,
        contenders$position_sd,
        contenders$dnf_rate,
        n_drivers
      )
      for (d in seq_len(n_drivers)) {
        sim_points[d] <- sim_points[d] +
          get_points_for_position(race_positions[d], "race")
      }
    }

    total_points_matrix[sim, ] <- sim_points
    winner_idx <- which.max(sim_points)
    win_counts[winner_idx] <- win_counts[winner_idx] + 1L
  }

  # --- Compile Results ---
  results <- tibble::tibble(
    driver_id = contenders$driver_id,
    current_points = contenders$points,
    win_probability = as.numeric(win_counts) / n_simulations,
    avg_final_points = colMeans(total_points_matrix),
    avg_final_position = NA_real_,
    in_contention = TRUE,
    season = season
  )

  # Calculate average final position from simulations
  for (sim in seq_len(n_simulations)) {
    ranks <- rank(-total_points_matrix[sim, ], ties.method = "min")
    results$avg_final_position <- results$avg_final_position + ranks
  }
  # Replace initial NA with 0 before accumulating
  results$avg_final_position <- 0
  for (sim in seq_len(n_simulations)) {
    ranks <- rank(-total_points_matrix[sim, ], ties.method = "min")
    results$avg_final_position <- results$avg_final_position + ranks
  }
  results$avg_final_position <- results$avg_final_position / n_simulations


  # Add non-contenders
  non_contenders <- sim_data %>%
    dplyr::filter(!.data$in_contention) %>%
    dplyr::transmute(
      driver_id = .data$driver_id,
      current_points = .data$points,
      win_probability = 0,
      avg_final_points = .data$points,
      avg_final_position = NA_real_,
      in_contention = FALSE,
      season = !!season
    )

  results <- dplyr::bind_rows(results, non_contenders) %>%
    dplyr::arrange(dplyr::desc(.data$win_probability), dplyr::desc(.data$current_points))

  results
}


#' Simulate Race Finishing Positions
#'
#' @description
#' Simulates finishing positions for a set of drivers based on their
#' performance metrics. Uses the weighted average position plus random noise
#' from a normal distribution with the driver's standard deviation. Applies
#' DNF probability.
#'
#' @param avg_positions Numeric vector of weighted average positions.
#' @param position_sds Numeric vector of position standard deviations.
#' @param dnf_rates Numeric vector of DNF probabilities.
#' @param n_drivers Integer number of drivers.
#'
#' @return An integer vector of finishing positions (NA for DNF).
#' @noRd
simulate_race_positions <- function(
    avg_positions,
    position_sds,
    dnf_rates,
    n_drivers
) {
  # Simulate raw performance scores (lower is better)
  raw_scores <- stats::rnorm(n_drivers, mean = avg_positions, sd = position_sds)

  # Apply DNF
  dnf <- stats::runif(n_drivers) < dnf_rates
  raw_scores[dnf] <- Inf

  # Convert to positions
  positions <- rep(NA_integer_, n_drivers)
  finishing_order <- order(raw_scores)

  pos <- 1L

  for (idx in finishing_order) {
    if (dnf[idx]) {
      positions[idx] <- NA_integer_
    } else {
      positions[idx] <- pos
      pos <- pos + 1L
    }
  }

  positions
}


# --- Social Media Formatting ---

#' Format Championship Odds for a Skeet
#'
#' @description
#' Formats the championship simulation results into a thread suitable for
#' posting on Bluesky, following the existing social post patterns.
#'
#' @param odds A tibble from `simulate_championship_odds()`.
#'
#' @return A list of lists suitable for `post_skeet_predictions()`.
#' @noRd
format_championship_skeet <- function(odds) {
  current_season <- odds$season[1]

  odds_formatted <- odds %>%
    dplyr::filter(.data$in_contention) %>%
    dplyr::mutate(
      driver_name = get_driver_name(current_season, .data$driver_id)
    )

  # Top contenders for championship win
  top_contenders <- odds_formatted %>%
    dplyr::arrange(dplyr::desc(.data$win_probability)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{dplyr::row_number()}. {.data$driver_name}: {scales::percent(.data$win_probability, 0.1)} ({round(.data$current_points)} pts)"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  n_remaining <- get_remaining_schedule(current_season)
  n_races <- nrow(n_remaining)
  n_sprints <- sum(n_remaining$has_sprint)

  tags <- c("F1", "F1Championship", "F1Predictions")

  skeet1_body <- glue::glue(
    "#F1 {current_season} Championship Win Probabilities \\U0001F3C6",
    "",
    "{n_races} races and {n_sprints} sprints remaining",
    "",
    "{top_contenders}",
    .sep = "\n"
  )

  # Additional context skeet
  leader <- odds_formatted %>%
    dplyr::arrange(dplyr::desc(.data$win_probability)) %>%
    dplyr::slice(1)

  skeet2_body <- glue::glue(
    "\\U0001F4CA Simulation details:",
    "Based on 10,000 Monte Carlo simulations",
    "Performance: 70% recent (last 5) / 30% season",
    "Accounts for DNFs and sprint races",
    "",
    "Leader {leader$driver_name} avg projected total: {round(leader$avg_final_points, 1)} pts",
    "\n#F1 #F1Championship",
    .sep = "\n"
  )

  return(list(
    list(text = skeet1_body, tags = tags),
    list(text = skeet2_body)
  ))
}


#' Post Championship Predictions to Bluesky
#'
#' @description
#' A wrapper function that simulates championship odds and posts them to Bluesky.
#'
#' @param odds A tibble from `simulate_championship_odds()`. If `NULL`,
#'   simulations are run automatically.
#' @param season Numeric season year (used only when `odds` is `NULL`).
#' @param n_simulations Number of simulations (used only when `odds` is `NULL`).
#'
#' @return Invisibly returns the Bluesky API response.
#' @export
#' @examples
#' \dontrun{
#' odds <- simulate_championship_odds(2025)
#' post_championship_predictions(odds)
#' }
post_championship_predictions <- function(
    odds = NULL,
    season = as.numeric(f1dataR::get_current_season()),
    n_simulations = 10000L
) {
  if (is.null(odds)) {
    odds <- simulate_championship_odds(
      season = season,
      n_simulations = n_simulations
    )
  }
  skeet_thread <- format_championship_skeet(odds)
  post_skeet_predictions(skeets = skeet_thread)
}
