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
    "1" = 25,
    "2" = 18,
    "3" = 15,
    "4" = 12,
    "5" = 10,
    "6" = 8,
    "7" = 6,
    "8" = 4,
    "9" = 2,
    "10" = 1
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
    "1" = 8,
    "2" = 7,
    "3" = 6,
    "4" = 5,
    "5" = 4,
    "6" = 3,
    "7" = 2,
    "8" = 1
  )
}

#' Get Points for a Finishing Position
#'
#' @param position Integer finishing position.
#' @param type Character, either "race" or "sprint".
#' @return Numeric points awarded (0 if outside points-scoring positions or DNF).
#' @noRd
get_points_for_position <- function(position, type = "race") {
  if (is.na(position) || position < 1) {
    return(0)
  }
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
get_current_standings <- function(
  season = as.numeric(f1dataR::get_current_season())
) {
  standings <- tryCatch(
    f1dataR::load_standings(season = season, round = "last", type = "driver"),
    error = function(e) {
      cli::cli_abort(
        "Could not load championship standings for {season}: {e$message}"
      )
    }
  )

  standings |>
    dplyr::transmute(
      driver_id = .data$driver_id,
      points = as.numeric(.data$points),
      position = as.numeric(.data$position)
    ) |>
    dplyr::arrange(.data$position)
}


.validate_chart_season <- function(season, call = rlang::caller_env()) {
  if (!is.numeric(season) || length(season) != 1 || is.na(season)) {
    cli::cli_abort(
      "{.arg season} must be a single numeric value.",
      call = call
    )
  }
}

.fallback_chart_label <- function(ids) {
  labels <- stringr::str_replace_all(ids, "_", " ")
  labels <- stringr::str_to_title(labels)
  fallback_codes <- toupper(stringr::str_sub(gsub("[^A-Za-z]", "", labels), 1, 3))
  ifelse(fallback_codes == "", labels, fallback_codes)
}

.completed_rounds <- function(season) {
  season_schedule <- f1predicter::schedule |>
    dplyr::filter(.data$season == !!season) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::arrange(.data$round)

  completed <- season_schedule |>
    dplyr::filter(.data$date < Sys.Date()) |>
    dplyr::pull(.data$round)

  unique(completed)
}

.load_round_standings_history <- function(
  season,
  type = c("driver", "constructor")
) {
  .validate_chart_season(season)
  type <- rlang::arg_match(type)

  rounds <- .completed_rounds(season)
  if (length(rounds) == 0) {
    cli::cli_abort(
      "No completed rounds found for season {season}."
    )
  }

  standings_history <- purrr::map_dfr(rounds, function(round) {
    standings <- tryCatch(
      f1dataR::load_standings(season = season, round = round, type = type),
      error = function(e) {
        cli::cli_abort(
          "Could not load {type} standings for season {season}, round {round}: {e$message}"
        )
      }
    )

    if (type == "driver") {
      standings |>
        dplyr::transmute(
          driver_id = .data$driver_id,
          constructor_id = .data$constructor_id,
          points = as.numeric(.data$points),
          position = as.numeric(.data$position),
          round = round,
          season = season
        )
    } else {
      standings |>
        dplyr::transmute(
          constructor_id = .data$constructor_id,
          points = as.numeric(.data$points),
          position = as.numeric(.data$position),
          round = round,
          season = season
        )
    }
  })

  race_lookup <- f1predicter::schedule |>
    dplyr::filter(.data$season == !!season) |>
    dplyr::transmute(
      season = as.numeric(.data$season),
      round = as.integer(.data$round),
      race_name = .data$race_name
    ) |>
    dplyr::distinct(.data$season, .data$round, .data$race_name)

  standings_history |>
    dplyr::left_join(race_lookup, by = c("season", "round")) |>
    dplyr::relocate(.data$race_name, .after = .data$round)
}

.safe_driver_colour <- function(driver_id, season, round) {
  tryCatch(
    f1dataR::get_driver_colour(
      driver = driver_id,
      season = season,
      round = round
    ),
    error = function(e) {
      NA_character_
    }
  )
}

.safe_constructor_colour <- function(constructor_id, season, round) {
  tryCatch(
    f1dataR::get_team_colour(
      team = constructor_id,
      season = season,
      round = round
    ),
    error = function(e) {
      NA_character_
    }
  )
}

.complete_chart_colors <- function(data, id_col) {
  missing <- is.na(data$color) | data$color == ""
  if (!any(missing)) {
    return(data)
  }

  missing_ids <- unique(data[[id_col]][missing])
  fallback <- grDevices::hcl.colors(length(missing_ids), palette = "Dark 3")
  names(fallback) <- missing_ids
  data$color[missing] <- fallback[data[[id_col]][missing]]
  data
}

.get_driver_chart_metadata <- function(season, round) {
  drivers <- tryCatch(
    f1dataR::load_drivers(season = season),
    error = function(e) {
      NULL
    }
  )

  if (is.null(drivers)) {
    return(tibble::tibble(
      driver_id = character(0),
      label = character(0),
      color = character(0)
    ))
  }

  drivers |>
    dplyr::transmute(
      driver_id = .data$driver_id,
      label = dplyr::if_else(
        is.na(.data$code) | .data$code == "",
        .fallback_chart_label(.data$driver_id),
        .data$code
      ),
      color = vapply(
        .data$driver_id,
        .safe_driver_colour,
        FUN.VALUE = character(1),
        season = season,
        round = round
      )
    ) |>
    .complete_chart_colors(id_col = "driver_id")
}

.get_constructor_chart_metadata <- function(season, round) {
  constructors <- tryCatch(
    f1dataR::load_constructors(),
    error = function(e) {
      NULL
    }
  )

  if (is.null(constructors)) {
    return(tibble::tibble(
      constructor_id = character(0),
      label = character(0),
      color = character(0)
    ))
  }

  constructors |>
    dplyr::transmute(
      constructor_id = .data$constructor_id,
      label = .data$name,
      color = vapply(
        .data$constructor_id,
        .safe_constructor_colour,
        FUN.VALUE = character(1),
        season = season,
        round = round
      )
    ) |>
    .complete_chart_colors(id_col = "constructor_id")
}

.build_championship_points_history <- function(
  season,
  type = c("driver", "constructor")
) {
  type <- rlang::arg_match(type)
  history <- .load_round_standings_history(season = season, type = type)
  round <- max(history$round, na.rm = TRUE)

  if (type == "driver") {
    metadata <- .get_driver_chart_metadata(season = season, round = round)
    history |>
      dplyr::left_join(metadata, by = "driver_id") |>
      dplyr::arrange(.data$round, .data$position)
  } else {
    metadata <- .get_constructor_chart_metadata(season = season, round = round)
    history |>
      dplyr::left_join(metadata, by = "constructor_id") |>
      dplyr::arrange(.data$round, .data$position)
  }
}

.format_historical_results <- function(results, season, round) {
  if (is.null(results) || nrow(results) == 0) {
    return(tibble::tibble(
      driver_id = character(0),
      round = integer(0),
      position = numeric(0),
      finished = logical(0),
      driver_failure = numeric(0),
      constructor_failure = numeric(0),
      constructor_failure_race = numeric(0),
      season = numeric(0)
    ))
  }

  results |>
    dplyr::transmute(
      driver_id = .data$driver_id,
      round = as.integer(round),
      position = as.numeric(.data$position),
      finished = .data$status == "Finished",
      driver_failure = as.numeric(.data$status != "Finished"),
      constructor_failure = 0,
      constructor_failure_race = 0,
      season = as.numeric(season)
    )
}

.load_championship_historical_data <- function(season) {
  purrr::map_dfr(c(season - 1, season), function(history_season) {
    rounds <- .completed_rounds(history_season)
    purrr::map_dfr(rounds, function(round) {
      race_results <- tryCatch(
        f1dataR::load_results(season = history_season, round = round),
        error = function(e) NULL
      )
      sprint_results <- tryCatch(
        suppressMessages(f1dataR::load_sprint(
          season = history_season,
          round = round
        )),
        error = function(e) NULL
      )

      dplyr::bind_rows(
        .format_historical_results(race_results, history_season, round),
        .format_historical_results(sprint_results, history_season, round)
      )
    })
  })
}

.build_driver_championship_odds_history <- function(
  season,
  n_simulations = 10000L,
  historical_data = NULL,
  ...
) {
  .validate_chart_season(season)

  if (is.null(historical_data)) {
    historical_data <- tryCatch(
      clean_data(cache_processed = TRUE),
      error = function(e) {
        .load_championship_historical_data(season)
      }
    )
  }

  standings_history <- .load_round_standings_history(
    season = season,
    type = "driver"
  )
  completed_rounds <- sort(unique(standings_history$round))
  metadata <- .get_driver_chart_metadata(
    season = season,
    round = max(completed_rounds)
  )

  purrr::map_dfr(completed_rounds, function(round) {
    round_standings <- standings_history |>
      dplyr::filter(.data$round == !!round)

    round_history <- historical_data |>
      dplyr::filter(.data$season < !!season | .data$round <= !!round)

    simulate_championship_odds(
      season = season,
      standings = round_standings |>
        dplyr::select("driver_id", "points", "position"),
      remaining = get_remaining_schedule(season = season, after_round = round),
      n_simulations = n_simulations,
      historical_data = round_history,
      ...
    ) |>
      dplyr::select(
        "driver_id",
        "current_points",
        "win_probability",
        "avg_final_points",
        "avg_final_position",
        "in_contention",
        "season"
      ) |>
      dplyr::mutate(
        round = round,
        race_name = round_standings$race_name[1]
      )
  }) |>
    dplyr::left_join(metadata, by = "driver_id") |>
    dplyr::arrange(.data$round, dplyr::desc(.data$win_probability))
}

.plot_championship_history <- function(
  history,
  id_col,
  value_col,
  title,
  ylab,
  percent = FALSE
) {
  final_points <- history |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::filter(.data$round == max(.data$round)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data[[value_col]])

  x_values <- sort(unique(history$round))
  y_values <- history[[value_col]]
  y_max <- max(y_values, na.rm = TRUE)
  y_limit <- c(0, y_max * 1.05)
  if (percent) {
    y_limit[2] <- max(y_limit[2], 0.05)
  }

  graphics::plot(
    NA,
    xlim = c(min(x_values), max(x_values) + 0.8),
    ylim = y_limit,
    xlab = "Round",
    ylab = ylab,
    xaxt = "n",
    yaxt = "n",
    xaxs = "i",
    yaxs = "i",
    main = title
  )
  x_ticks <- x_values
  y_ticks <- pretty(y_limit)
  graphics::abline(v = x_ticks, col = "grey85", lwd = 1)
  graphics::abline(h = y_ticks, col = "grey85", lwd = 1)
  graphics::axis(1, at = x_ticks, labels = x_ticks)
  if (percent) {
    graphics::axis(
      2,
      at = y_ticks,
      labels = sprintf("%d%%", round(y_ticks * 100))
    )
  } else {
    graphics::axis(2, at = y_ticks, labels = y_ticks)
  }

  old_xpd <- graphics::par("xpd")
  on.exit(graphics::par(xpd = old_xpd), add = TRUE)
  graphics::par(xpd = NA)

  for (id in final_points[[id_col]]) {
    driver_history <- history |>
      dplyr::filter(.data[[id_col]] == !!id) |>
      dplyr::arrange(.data$round)

    graphics::lines(
      driver_history$round,
      driver_history[[value_col]],
      col = driver_history$color[1],
      lwd = 2
    )
  }

  label_prefix <- final_points$label
  label_suffix <- if (percent) {
    sprintf("%d", round(final_points[[value_col]] * 100))
  } else {
    round(final_points[[value_col]])
  }
  labels <- paste(label_prefix, label_suffix)

  graphics::points(
    final_points$round,
    final_points[[value_col]],
    col = final_points$color,
    pch = 16,
    cex = 1.4
  )
  graphics::text(
    x = final_points$round + 0.12,
    y = final_points[[value_col]],
    labels = labels,
    col = final_points$color,
    pos = 4
  )

  invisible(history)
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
  schedule <- f1predicter::schedule |>
    dplyr::filter(.data$season == !!season) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      has_sprint = !is.na(.data$sprint_date)
    )

  if (is.null(after_round)) {
    schedule <- schedule |>
      dplyr::filter(.data$date >= Sys.Date())
  } else {
    schedule <- schedule |>
      dplyr::filter(.data$round > after_round)
  }

  schedule |>
    dplyr::select("round", "race_name", "date", "has_sprint") |>
    dplyr::arrange(.data$round)
}


# --- Performance Metrics ---

#' Calculate Driver Performance Metrics
#'
#' @description
#' Calculates performance metrics for each driver based on recent race history,
#' including average finishing position, standard deviation (consistency), and
#' DNF rate. Uses a weighted approach combining recent form, full season
#' average, and previous season performance, inspired by the Jeppe Olesen
#' approach.
#'
#' For early-season calculations (fewer than `n_recent_races` races completed),
#' data from the previous season is included with a sliding scale: race 1 uses
#' 4/5 previous season + 1/5 current, race 2 uses 3/5 + 2/5, etc. Once 5+
#' races are completed, only current season data is used for the recent and
#' season components, but `weight_prev_season` still anchors to the previous
#' season's average.
#'
#' @param season Numeric season year.
#' @param historical_data Historical race data from `clean_data()`. If `NULL`,
#'   uses the package's internal data loading.
#' @param weight_recent Numeric weight for last `n_recent_races` performance
#'   (default 0.50).
#' @param weight_season Numeric weight for full current season average
#'   (default 0.40).
#' @param weight_prev_season Numeric weight for previous season performance
#'   (default 0.1).
#' @param n_recent_races Integer number of recent races to use for the
#'   recent-form component (default 5).
#'
#' @return A tibble with columns `driver_id`, `avg_position`, `position_sd`,
#'   `dnf_rate`.
#' @noRd
calculate_driver_performance <- function(
  season,
  historical_data = NULL,
  weight_recent = 0.5,
  weight_season = 0.4,
  weight_prev_season = 0.1,
  n_recent_races = 5L
) {
  if (is.null(historical_data)) {
    historical_data <- clean_data()
  }

  season_data <- historical_data |>
    dplyr::filter(.data$season == !!season)

  prev_season_data <- historical_data |>
    dplyr::filter(.data$season == !!(season - 1))

  if (nrow(season_data) == 0 && nrow(prev_season_data) == 0) {
    cli::cli_abort(
      "No historical data found for season {season} or {season - 1} in {.fn calculate_driver_performance}."
    )
  }

  # Determine how many races have been completed this season
  n_completed <- if (nrow(season_data) > 0) {
    max(season_data$round, na.rm = TRUE)
  } else {
    0L
  }

  # Calculate current season metrics (may be empty)
  current_metrics <- NULL
  current_season <- NULL
  if (nrow(season_data) > 0) {
    current_metrics <- calculate_season_metrics(
      season_data,
      n_recent = n_recent_races
    )
    current_season <- calculate_season_metrics(
      season_data,
      n_recent = length(unique(season_data$round)) # Use full current season for anchor
    )
  }

  # Calculate previous season metrics (for blending and prev-season weight)
  prev_metrics <- NULL
  if (nrow(prev_season_data) > 0) {
    prev_metrics <- calculate_season_metrics(
      prev_season_data,
      n_recent = n_recent_races
    )
    prev_season <- calculate_season_metrics(
      prev_season_data,
      n_recent = length(unique(prev_season_data$round)) # Use full previous season for anchor
    )
  }

  # For early season (< n_recent_races races), blend with previous season
  if (n_completed < n_recent_races && !is.null(prev_metrics)) {
    # Sliding scale: current season weight increases with races completed
    current_weight <- n_completed / n_recent_races
    prev_weight <- 1 - current_weight

    if (is.null(current_metrics)) {
      # No current season data at all (pre-season), use previous season only
      driver_metrics <- prev_metrics
    } else {
      # Blend metrics from both seasons
      all_drivers <- unique(c(
        current_metrics$driver_id,
        prev_metrics$driver_id
      ))
      driver_metrics <- tibble::tibble(driver_id = all_drivers)

      driver_metrics <- driver_metrics |>
        dplyr::left_join(
          current_metrics |>
            dplyr::select(
              "driver_id",
              curr_avg = "avg_position",
              curr_sd = "position_sd",
              curr_dnf = "dnf_rate",
              "n_races"
            ),
          by = "driver_id"
        ) |>
        dplyr::left_join(
          prev_metrics |>
            dplyr::select(
              "driver_id",
              prev_avg = "avg_position",
              prev_sd = "position_sd",
              prev_dnf = "dnf_rate",
            ),
          by = "driver_id"
        ) |>
        dplyr::mutate(
          n_races = tidyr::replace_na(.data$n_races, 0L),
          avg_position = dplyr::case_when(
            !is.na(.data$curr_avg) & !is.na(.data$prev_avg) ~
              current_weight * .data$curr_avg + prev_weight * .data$prev_avg,
            !is.na(.data$curr_avg) ~ .data$curr_avg,
            TRUE ~ .data$prev_avg
          ),
          position_sd = dplyr::case_when(
            !is.na(.data$curr_sd) & !is.na(.data$prev_sd) ~
              current_weight * .data$curr_sd + prev_weight * .data$prev_sd,
            !is.na(.data$curr_sd) ~ .data$curr_sd,
            TRUE ~ .data$prev_sd
          ),
          dnf_rate = dplyr::case_when(
            !is.na(.data$curr_dnf) & !is.na(.data$prev_dnf) ~
              current_weight * .data$curr_dnf + prev_weight * .data$prev_dnf,
            !is.na(.data$curr_dnf) ~ .data$curr_dnf,
            TRUE ~ .data$prev_dnf
          )
        ) |>
        dplyr::select(
          "driver_id",
          "n_races",
          "avg_position",
          "position_sd",
          "dnf_rate",
        )
    }
    current_season <- driver_metrics
  } else {
    # n_recent_races+ races completed: use current season only
    driver_metrics <- current_metrics
  }

  # --- Compute weighted_avg_position with 3-way blend ---
  # Determine previous season avg for the prev-season anchor component
  prev_season_avg <- if (!is.null(prev_metrics)) {
    prev_season |>
      dplyr::select(
        "driver_id",
        prev_season_avg = "avg_position",
        prev_season_sd = "position_sd",
        prev_season_dnf = "dnf_rate"
      )
  } else {
    NULL
  }

  # Determine current season avg for the season anchor component
  curr_season_avg <- if (!is.null(current_season)) {
    current_season |>
      dplyr::select(
        "driver_id",
        curr_season_avg = "avg_position",
        curr_season_sd = "position_sd",
        curr_season_dnf = "dnf_rate"
      )
  } else {
    NULL
  }

  if (!is.null(prev_season_avg)) {
    driver_metrics <- driver_metrics |>
      dplyr::left_join(prev_season_avg, by = "driver_id") |>
      dplyr::left_join(curr_season_avg, by = "driver_id") |>
      tidyr::replace_na(list(
        prev_season_avg = 15,
        prev_season_sd = 5,
        prev_season_dnf = 0.1,
        curr_season_avg = 15,
        curr_season_sd = 5,
        curr_season_dnf = 0.1
      )) |>
      dplyr::mutate(
        avg_position = weight_recent *
          .data$avg_position +
          weight_season * .data$curr_season_avg +
          weight_prev_season * .data$prev_season_avg,
        position_sd = weight_recent *
          .data$position_sd +
          weight_season * .data$curr_season_sd +
          weight_prev_season * .data$prev_season_sd,
        dnf_rate = weight_recent *
          .data$dnf_rate +
          weight_season * .data$curr_season_dnf +
          weight_prev_season * .data$prev_season_dnf
      )
  }

  driver_metrics |>
    dplyr::select(
      "driver_id",
      "avg_position",
      "position_sd",
      "dnf_rate",
    )
}


#' Calculate Metrics for a Single Season of Data
#'
#' @description
#' Computes per-driver performance metrics from a single season's race results,
#' including average finishing position, consistency (standard deviation), DNF
#' rate, and recent form (last N races). These metrics feed into the weighted
#' performance model used by the Monte Carlo championship simulation.
#'
#' @param data A tibble of race results for a single season with columns
#'   `driver_id`, `round`, `position`, `finished`.
#' @param n_recent Integer number of recent races for the recent-form average
#'   (default 5).
#' @return A tibble with per-driver performance metrics.
#' @noRd
calculate_season_metrics <- function(data, n_recent = 5L) {
  grid_size <- max(data$position, na.rm = TRUE)
  if (is.infinite(grid_size) || is.na(grid_size)) {
    grid_size <- 22
  }

  data <- data |>
    dplyr::group_by(.data$driver_id) |>
    dplyr::arrange(.data$round) |>
    dplyr::mutate(
      # consider both driver and constructor failures for DNF rate, since both can cause a DNF
      # also take a small penalty for teammate car failure
      # but a driver can't dnf more than once per race
      dnf = .data$driver_failure +
        .data$constructor_failure +
        .data$constructor_failure_race,
      dnf = ifelse(.data$dnf > 1, 1, .data$dnf)
    ) |>
    dplyr::summarise(
      n_races = dplyr::n(),
      avg_position = mean(
        utils::tail(.data$position[!is.na(.data$position)], !!n_recent),
        na.rm = TRUE
      ),
      position_sd = stats::sd(
        utils::tail(.data$position[!is.na(.data$position)], !!n_recent),
        na.rm = TRUE
      ),
      dnf_rate = mean(
        utils::tail(.data$dnf, !!n_recent),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    # Correct for tail truncation bias. Observed SD is compressed near P1 and
    # the back of the grid because finishing positions are bounded.
    dplyr::mutate(
      position_sd = tidyr::replace_na(.data$position_sd, 5),
      # Heuristic multiplier: inflates SD by ~1.65x at boundaries to better estimate latent performance.
      dist_to_tail = pmin(
        .data$avg_position - 1,
        grid_size - .data$avg_position
      ),
      position_sd = pmax(
        .data$position_sd * (1 + 0.65 * exp(-pmax(dist_to_tail, 0) / 3)),
        1.5
      )
    ) |>
    dplyr::select(-"dist_to_tail")
  return(data)
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
#'       \item Simulates all drivers in the standings (full grid).
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
#' All drivers are simulated in each race to produce realistic position
#' assignments. Mathematical contention is tracked but does not exclude
#' drivers from the simulation.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#' @param standings A tibble of current standings from `get_current_standings()`.
#'   If `NULL`, loaded automatically.
#' @param remaining A tibble of remaining schedule from `get_remaining_schedule()`.
#'   If `NULL`, loaded automatically.
#' @param n_simulations Integer number of Monte Carlo simulations to run.
#'   Defaults to 10000.
#' @param historical_data Historical race data. If `NULL`, uses `clean_data()`.
#' @param ... Additional parameters to be passed to calculate_driver_performance(), such as
#'   weights for the performance metrics. See `calculate_driver_performance()` for details.
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
  historical_data = NULL,
  ...
) {
  # --- Input Validation ---
  if (!is.numeric(season) || length(season) != 1) {
    cli::cli_abort(
      "{.arg season} must be a single numeric value in {.fn simulate_championship_odds}."
    )
  }

  if (!is.numeric(n_simulations) || n_simulations < 1) {
    cli::cli_abort(
      "{.arg n_simulations} must be a positive number in {.fn simulate_championship_odds}."
    )
  }
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
      standings |>
        dplyr::mutate(
          current_points = .data$points,
          win_probability = dplyr::if_else(.data$position == 1, 1, 0),
          avg_final_points = .data$points,
          avg_final_position = as.numeric(.data$position),
          in_contention = .data$position == 1,
          season = season
        ) |>
        dplyr::select(
          "driver_id",
          "current_points",
          "win_probability",
          "avg_final_points",
          "avg_final_position",
          "in_contention",
          "season"
        )
    )
  }

  # --- Performance Metrics ---
  cli::cli_inform("Calculating driver performance metrics...")
  performance <- calculate_driver_performance(
    season,
    historical_data,
    ... = ...
  )

  # Merge standings with performance
  sim_data <- standings |>
    dplyr::left_join(performance, by = "driver_id") |>
    dplyr::mutate(
      # Defaults for drivers with no performance data
      avg_position = tidyr::replace_na(.data$avg_position, 15),
      position_sd = tidyr::replace_na(.data$position_sd, 5),
      dnf_rate = tidyr::replace_na(.data$dnf_rate, 0.1)
    )

  # --- Determine Mathematical Contention ---
  n_remaining_races <- nrow(remaining)
  n_remaining_sprints <- sum(remaining$has_sprint)
  max_race_points <- max(gp_points_system())
  max_sprint_points <- max(sprint_points_system())
  max_possible_remaining <- n_remaining_races *
    max_race_points +
    n_remaining_sprints * max_sprint_points

  leader_points <- max(sim_data$points)
  sim_data <- sim_data |>
    dplyr::mutate(
      in_contention = (.data$points + max_possible_remaining) >= leader_points
    )

  n_contenders <- sum(sim_data$in_contention)

  if (n_contenders == 0) {
    cli::cli_abort(
      "No drivers in mathematical contention in {.fn simulate_championship_odds}. This shouldn't happen."
    )
  }

  # --- Run Simulations (all drivers participate in each race) ---
  n_drivers <- nrow(sim_data)

  win_counts <- stats::setNames(integer(n_drivers), sim_data$driver_id)
  total_points_matrix <- matrix(
    0,
    nrow = n_simulations,
    ncol = n_drivers,
    dimnames = list(NULL, sim_data$driver_id)
  )

  pb <- cli::cli_progress_bar(
    total = n_simulations,
    format = "Simulating Season Championships {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total})"
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  for (sim in seq_len(n_simulations)) {
    cli::cli_progress_update(id = pb)
    sim_points <- sim_data$points

    for (race_idx in seq_len(nrow(remaining))) {
      race <- remaining[race_idx, ]

      # --- Sprint Simulation ---
      if (race$has_sprint) {
        sprint_positions <- simulate_race_positions(
          sim_data$avg_position,
          sim_data$position_sd,
          sim_data$dnf_rate * 0.5, # 50% lower DNF rate for sprints
          n_drivers
        )
        for (d in seq_len(n_drivers)) {
          sim_points[d] <- sim_points[d] +
            get_points_for_position(sprint_positions[d], "sprint")
        }
      }

      # --- Race Simulation ---
      race_positions <- simulate_race_positions(
        sim_data$avg_position,
        sim_data$position_sd,
        sim_data$dnf_rate,
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
    driver_id = sim_data$driver_id,
    current_points = sim_data$points,
    win_probability = as.numeric(win_counts) / n_simulations,
    avg_final_points = colMeans(total_points_matrix),
    avg_final_position = NA_real_,
    in_contention = sim_data$in_contention,
    season = season
  )

  # Calculate average final position from simulations
  results$avg_final_position <- 0
  for (sim in seq_len(n_simulations)) {
    ranks <- rank(-total_points_matrix[sim, ], ties.method = "min")
    results$avg_final_position <- results$avg_final_position + ranks
  }
  results$avg_final_position <- results$avg_final_position / n_simulations

  results <- results |>
    dplyr::arrange(
      dplyr::desc(.data$win_probability),
      dplyr::desc(.data$current_points),
      .data$avg_final_position
    )

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
  # Simulate raw performance scores (lower is better).
  raw_scores <- stats::rnorm(
    n_drivers,
    mean = avg_positions,
    sd = position_sds
  )

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


# --- Championship Charts ---

#' Chart Driver Championship Odds by Round
#'
#' @description
#' Plots simulated driver championship win probabilities after each completed
#' race weekend in the selected season.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#' @param n_simulations Integer number of Monte Carlo simulations to run for
#'   each completed round. Defaults to 10000.
#' @param historical_data Historical race data from `clean_data()`. If `NULL`,
#'   uses `clean_data()`.
#' @param ... Additional parameters passed to `simulate_championship_odds()`.
#'
#' @return Invisibly returns the plotted odds history tibble.
#' @export
#' @examples
#' \dontrun{
#' chart_driver_championship_odds(2025, n_simulations = 1000)
#' }
chart_driver_championship_odds <- function(
  season = as.numeric(f1dataR::get_current_season()),
  n_simulations = 10000L,
  historical_data = NULL,
  ...
) {
  history <- .build_driver_championship_odds_history(
    season = season,
    n_simulations = n_simulations,
    historical_data = historical_data,
    ...
  )

  .plot_championship_history(
    history = history,
    id_col = "driver_id",
    value_col = "win_probability",
    title = glue::glue("F1 Drivers Championship Odds {season}"),
    ylab = "Win probability",
    percent = TRUE
  )
}

#' Chart Driver Championship Points by Round
#'
#' @description
#' Plots driver championship points after each completed race weekend in the
#' selected season.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#'
#' @return Invisibly returns the plotted points history tibble.
#' @export
#' @examples
#' \dontrun{
#' chart_driver_championship_points(2025)
#' }
chart_driver_championship_points <- function(
  season = as.numeric(f1dataR::get_current_season())
) {
  history <- .build_championship_points_history(
    season = season,
    type = "driver"
  )

  .plot_championship_history(
    history = history,
    id_col = "driver_id",
    value_col = "points",
    title = glue::glue("F1 Drivers Championship {season}"),
    ylab = "Points"
  )
}

#' Chart Constructor Championship Points by Round
#'
#' @description
#' Plots constructor championship points after each completed race weekend in
#' the selected season.
#'
#' @param season Numeric season year. Defaults to `f1dataR::get_current_season()`.
#'
#' @return Invisibly returns the plotted points history tibble.
#' @export
#' @examples
#' \dontrun{
#' chart_constructor_championship_points(2025)
#' }
chart_constructor_championship_points <- function(
  season = as.numeric(f1dataR::get_current_season())
) {
  history <- .build_championship_points_history(
    season = season,
    type = "constructor"
  )

  .plot_championship_history(
    history = history,
    id_col = "constructor_id",
    value_col = "points",
    title = glue::glue("F1 Constructors Championship {season}"),
    ylab = "Points"
  )
}


# --- Social Media Formatting ---

#' Format Championship Odds for a Skeet
#'
#' @description
#' Formats the championship simulation results into a thread suitable for
#' posting on Bluesky, following the existing social post patterns.
#'
#' @param odds A tibble from `simulate_championship_odds()`.
#' @param n_simulations Integer number of simulations that were run (for display).
#'   Defaults to 10000.
#'
#' @return A list of lists suitable for `post_skeet_predictions()`.
#' @noRd
format_championship_skeet <- function(odds, n_simulations = 10000L) {
  current_season <- odds$season[1]

  odds_formatted <- odds |>
    dplyr::filter(.data$in_contention) |>
    dplyr::mutate(
      driver_name = get_driver_name(current_season, .data$driver_id)
    )

  # Top contenders for championship win
  top_contenders <- odds_formatted |>
    dplyr::arrange(dplyr::desc(.data$win_probability)) |>
    dplyr::slice_head(n = 5) |>
    dplyr::mutate(
      text = glue::glue(
        "{dplyr::row_number()}. {.data$driver_name}: {scales::percent(.data$win_probability, 0.1)} ({round(.data$current_points)} pts)"
      )
    ) |>
    dplyr::pull(.data$text) |>
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
  leader <- odds_formatted |>
    dplyr::arrange(dplyr::desc(.data$win_probability)) |>
    dplyr::slice(1)

  skeet2_body <- glue::glue(
    "\\U0001F4CA Simulation details:",
    "Based on {scales::comma(n_simulations)} Monte Carlo simulations",
    "Performance: 65% recent (last 5) / 30% season / 5% prev season",
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
# nocov start
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
  skeet_thread <- format_championship_skeet(odds, n_simulations = n_simulations)
  post_skeet_predictions(skeets = skeet_thread)
}
# nocov end
