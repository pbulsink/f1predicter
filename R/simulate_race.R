# Monte Carlo race simulation for a single F1 race weekend.
# The mean finishing position for each driver is provided by the ML prediction
# pipeline (via `.predict_position()`); the SD and DNF rate are computed from
# historical data using the same empirical approach as the championship
# simulation (`calculate_driver_performance()`).

#' Default Simulation Parameters
#'
#' Returns the default set of parameters used during Monte Carlo race
#' simulation. Pass a modified copy to `simulate_race()` to tune behaviour
#' without touching source code.
#'
#' @return A named list of numeric parameters:
#'   \describe{
#'     \item{n_simulations}{Number of Monte Carlo simulations (default 10000).}
#'     \item{circuit_sd_scale}{Logical flag. When `TRUE` (the default), each
#'       driver's `position_sd` is multiplied by `(2 - grid_pos_corr_avg)`,
#'       so low-overtaking circuits preserve grid order (multiplier near 1)
#'       while high-overtaking circuits widen the distribution (multiplier up
#'       to 2). Set to `FALSE` to disable circuit-based SD scaling.}
#'     \item{sprint_mean_weight}{Weight given to the sprint finishing position
#'       when updating the ML mean estimate. Between 0 and 1 (default 0.15).}
#'     \item{sprint_dnf_scale}{Fraction of the full-race DNF rate used for
#'       sprint simulations (default 0.5).}
#'     \item{wet_sd_multiplier}{Factor by which each driver's `position_sd` is
#'       scaled when weather is `"wet"` (default 1.5).}
#'     \item{default_position_sd}{Fallback SD when no historical data are
#'       available (default 5).}
#'     \item{default_dnf_rate}{Fallback DNF probability when no historical data
#'       are available (default 0.1).}
#'     \item{quali_default_position_sd}{Fallback qualifying position SD when no
#'       historical data are available (default 3).}
#'     \item{quali_qgap_sd_weight}{Scaling weight for `driver_avg_qgap` when
#'       adjusting per-driver qualifying SD. Larger gap → wider SD (default
#'       0.15).}
#'     \item{quali_practice_weight}{Weight given to the practice rank when
#'       blending with the ML qualifying mean. Between 0 and 1 (default 0.10).}
#'     \item{quali_wet_sd_multiplier}{Factor by which each driver's qualifying
#'       SD is scaled when weather is `"wet"` (default 1.3).}
#'   }
#' @export
#'
#' @examples
#' params <- simulation_params()
#' params$n_simulations
simulation_params <- function() {
  list(
    n_simulations = 10000L,
    circuit_sd_scale = TRUE,
    sprint_mean_weight = 0.15,
    sprint_dnf_scale = 0.5,
    wet_sd_multiplier = 1.5,
    default_position_sd = 5,
    default_dnf_rate = 0.1,
    # --- Qualifying simulation parameters ---
    quali_default_position_sd = 3,
    # Weight of the qgap (average qualifying gap to fastest, in seconds) when
    # adjusting the per-driver position SD. Larger qgap → worse quali pace →
    # wider SD. Applied as: sd * (1 + quali_qgap_sd_weight * driver_avg_qgap).
    quali_qgap_sd_weight = 0.15,
    # Weight of practice rank (lower rank = faster) when adjusting the ML mean
    # qualifying position. Applied as: mean * (1 - quali_practice_weight) +
    # practice_rank * quali_practice_weight. Between 0 and 1 (default 0.10).
    quali_practice_weight = 0.10,
    # Wet-weather SD multiplier for qualifying (default 1.3, slightly less than
    # race because wet quali is shorter and pace gaps compress less than a race).
    quali_wet_sd_multiplier = 1.3
  )
}


#' Compute Per-Driver Race Simulation Metrics
#'
#' @description
#' Derives per-driver `position_sd` and `dnf_rate` for the upcoming race using
#' the same empirical approach as `calculate_driver_performance()` in
#' `championship.R`. The mean (`avg_position`) is *not* returned here; it comes
#' from the ML model via `.predict_position()`.
#'
#' @param season Numeric season year.
#' @param circuit_id Character circuit identifier, used to look up
#'   circuit-specific `grid_pos_corr_avg`.
#' @param new_data A tibble from `generate_new_data()` containing at least
#'   `driver_id` and `grid_pos_corr_avg`.
#' @param historical_data Historical race data from `clean_data()`. If `NULL`,
#'   uses the package's internal loading.
#' @param n_recent Integer number of recent races for the recent-form window
#'   (default 5).
#' @param params A list of simulation parameters from `simulation_params()`.
#'
#' @return A tibble with columns `driver_id`, `position_sd`, `dnf_rate`.
#' @keywords internal
.calculate_race_sim_metrics <- function(
  season,
  circuit_id,
  new_data,
  historical_data = NULL,
  n_recent = 5L,
  params = simulation_params()
) {
  if (is.null(historical_data)) {
    historical_data <- clean_data()
  }

  # Use the same 3-way blending as calculate_driver_performance()
  perf <- calculate_driver_performance(
    season = season,
    historical_data = historical_data,
    n_recent_races = n_recent
  )

  # Pull per-circuit grid_pos_corr_avg from new_data (one value per driver, but
  # it's a circuit-level stat so all rows should be equal — take the mean to be
  # safe).
  grid_corr <- if (
    "grid_pos_corr_avg" %in%
      names(new_data) &&
      any(!is.na(new_data$grid_pos_corr_avg))
  ) {
    mean(new_data$grid_pos_corr_avg, na.rm = TRUE)
  } else {
    0.5682137 # package default from processing_params()
  }

  # SD circuit multiplier: high correlation → tight order → multiply near 1.
  # Low correlation → wide spread → multiply toward 2.
  circuit_multiplier <- if (isTRUE(params$circuit_sd_scale)) {
    2 - grid_corr
  } else {
    1
  }

  result <- new_data |>
    dplyr::select("driver_id") |>
    dplyr::left_join(perf, by = "driver_id") |>
    dplyr::mutate(
      position_sd = tidyr::replace_na(
        .data$position_sd,
        params$default_position_sd
      ) *
        circuit_multiplier,
      dnf_rate = tidyr::replace_na(
        .data$dnf_rate,
        params$default_dnf_rate
      )
    ) |>
    dplyr::select("driver_id", "position_sd", "dnf_rate")

  result
}


#' Simulate a Single F1 Race
#'
#' @description
#' Runs a Monte Carlo simulation of an upcoming F1 race and returns the
#' collapsed per-driver probability tibble. This is the main user-facing
#' controller function that replaces `predict_round()`.
#'
#' @details
#' The simulation proceeds as follows:
#'
#' 1. The ML prediction pipeline (internal `.predict_position()`) provides a
#'    mean finishing position per driver based on all engineered features.
#' 2. Historical data are used to estimate each driver's finishing-position
#'    standard deviation and DNF rate via `calculate_driver_performance()`.
#' 3. Optionally, a sprint result from the current weekend adjusts the ML mean
#'    toward the observed sprint position with weight `sprint_mean_weight`.
#' 4. Optionally, wet weather scales up all SDs by `wet_sd_multiplier`.
#' 5. `n_simulations` independent races are drawn using `simulate_race_positions()`
#'    and collapsed into per-position probabilities by `summarise_simulations()`.
#'
#' The seed is set to `as.integer(format(Sys.Date(), "%Y%m%d"))` so that
#' predictions generated on the same day are reproducible.
#'
#' @param new_data A tibble from `generate_new_data()` or
#'   `generate_next_race_data()`. If `NULL` (default), calls
#'   `generate_next_race_data()` automatically.
#' @param results_models A named list of fitted model objects as returned by
#'   `model_results_*()`, or a character string `"early"`, `"late"`, or
#'   `"after_quali"` to load models from disk. If `NULL` (default), the
#'   appropriate timing is inferred from `new_data` column names.
#' @param sprint_results An optional data frame with columns `driver_id` and
#'   `sprint_position` giving the sprint race results for the current weekend.
#'   When provided, the ML mean for each driver is nudged toward their sprint
#'   result. Use `NULL` (default) if no sprint has occurred.
#' @param weather A character string, one of `"dry"`, `"wet"`, `"cold"`,
#'   `"warm"`, or `"cloudy"`. When `"wet"`, position SDs are scaled up.
#'   Defaults to `"dry"`. Pass `NULL` to attempt automatic weather lookup via
#'   `getWeather()` (requires Wikipedia access).
#' @param historical_data Historical race data from `clean_data()`. If `NULL`
#'   (default), loads internally.
#' @param n_simulations Integer number of Monte Carlo simulations (default
#'   inherits from `params$n_simulations`).
#' @param params A list of simulation hyperparameters from `simulation_params()`.
#' @param engine The model engine to use when loading models from disk. Defaults
#'   to `"ensemble"`. Can also be `"ranger"`.
#'
#' @return A tibble with one row per driver containing:
#'   \describe{
#'     \item{`driver_id`}{Driver identifier.}
#'     \item{`season`}{Season year.}
#'     \item{`round`}{Round number.}
#'     \item{`win_prob`}{Probability of winning.}
#'     \item{`podium_prob`}{Probability of a top-3 finish.}
#'     \item{`top10_prob`}{Probability of a top-10 finish.}
#'     \item{`likely_position`}{Median finishing position across simulations.}
#'     \item{`expected_points`}{Mean championship points across simulations.}
#'     \item{`position_sd`}{Standard deviation of finishing position across
#'       simulations.}
#'     \item{`.probs`}{Matrix list-column (drivers × positions) of finishing
#'       position probabilities. Compatible with `format_results_prob_table()`.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' preds <- simulate_race()
#' format_results_prob_table(preds)
#' }
simulate_race <- function(
  new_data = NULL,
  results_models = NULL,
  sprint_results = NULL,
  weather = "dry",
  historical_data = NULL,
  n_simulations = NULL,
  params = simulation_params(),
  engine = "ensemble"
) {
  if (is.null(new_data)) {
    new_data <- generate_next_race_data()
  }

  n_simulations <- if (is.null(n_simulations)) {
    params$n_simulations
  } else {
    n_simulations
  }
  n_simulations <- as.integer(n_simulations)

  # --- Load / validate models ---
  if (is.null(results_models) || is.character(results_models)) {
    if (is.character(results_models)) {
      valid_timings <- c("early", "late", "after_quali")
      rlang::arg_match(results_models, valid_timings)
      model_timing <- results_models
    } else {
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

  required_models <- c("win", "podium", "t10", "position")
  if (!all(required_models %in% names(results_models))) {
    cli::cli_abort(
      "The {.arg results_models} list must contain: {.val {required_models}}"
    )
  }

  # --- Weather resolution ---
  weather <- .resolve_weather(new_data, weather)

  # --- ML mean positions ---
  season <- new_data$season[1]
  round <- new_data$round[1]
  circuit_id <- if ("circuit_id" %in% names(new_data)) {
    new_data$circuit_id[1]
  } else {
    NA_character_
  }
  n_drivers <- nrow(new_data)

  ml_positions <- .predict_position(new_data, results_models$position)

  avg_positions <- ml_positions$likely_position

  # --- Sprint update ---
  if (!is.null(sprint_results)) {
    avg_positions <- .apply_sprint_update(
      driver_ids = new_data$driver_id,
      avg_positions = avg_positions,
      sprint_results = sprint_results,
      weight = params$sprint_mean_weight
    )
  }

  # --- Historical SDs and DNF rates ---
  sim_metrics <- .calculate_race_sim_metrics(
    season = season,
    circuit_id = circuit_id,
    new_data = new_data,
    historical_data = historical_data,
    params = params
  )

  position_sds <- sim_metrics$position_sd
  dnf_rates <- sim_metrics$dnf_rate

  # --- Weather SD scaling ---
  if (identical(weather, "wet")) {
    position_sds <- position_sds * params$wet_sd_multiplier
  }

  # --- Run simulations ---
  set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))

  sim_matrix <- matrix(
    NA_integer_,
    nrow = n_drivers,
    ncol = n_simulations
  )

  pb <- cli::cli_progress_bar(
    total = n_simulations,
    format = "Simulating race {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total})"
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  for (sim in seq_len(n_simulations)) {
    cli::cli_progress_update(id = pb)
    sim_matrix[, sim] <- simulate_race_positions(
      avg_positions,
      position_sds,
      dnf_rates,
      n_drivers
    )
  }

  # --- Summarise ---
  results <- summarise_simulations(
    sim_matrix = sim_matrix,
    driver_ids = new_data$driver_id,
    season = season,
    round = round,
    n_simulations = n_simulations
  )

  results
}


#' Apply Sprint Result Update to ML Mean Positions
#'
#' @param driver_ids Character vector of driver IDs in the order they appear in
#'   `new_data`.
#' @param avg_positions Numeric vector of ML-predicted mean finishing positions.
#' @param sprint_results Data frame with `driver_id` and `sprint_position`.
#' @param weight Numeric weight for the sprint position (0–1).
#'
#' @return Updated numeric vector of mean positions.
#' @keywords internal
.apply_sprint_update <- function(
  driver_ids,
  avg_positions,
  sprint_results,
  weight = 0.15
) {
  sprint_lookup <- stats::setNames(
    sprint_results$sprint_position,
    sprint_results$driver_id
  )

  updated <- avg_positions
  for (i in seq_along(driver_ids)) {
    id <- driver_ids[i]
    if (!is.na(sprint_lookup[id])) {
      updated[i] <- (1 - weight) *
        avg_positions[i] +
        weight * sprint_lookup[[id]]
    }
  }
  updated
}


#' Resolve Weather for Simulation
#'
#' @description
#' Returns the effective weather string. If `weather` is `NULL`, attempts to
#' look it up via `getWeather()` (which scrapes Wikipedia). Falls back to
#' `"dry"` on any error so the simulation always proceeds.
#'
#' @param new_data Tibble from `generate_new_data()` (used to determine season /
#'   round for the weather lookup).
#' @param weather User-supplied weather string or `NULL`.
#'
#' @return A character string, one of `"dry"`, `"wet"`, `"cold"`, `"warm"`,
#'   `"cloudy"`, or `"unknown"`.
#' @keywords internal
.resolve_weather <- function(new_data, weather) {
  if (!is.null(weather)) {
    valid <- c("dry", "wet", "cold", "warm", "cloudy", "unknown")
    if (!weather %in% valid) {
      cli::cli_warn(
        "{.arg weather} value {.val {weather}} is not recognised; defaulting to {.val dry}."
      )
      return("dry")
    }
    return(weather)
  }

  # Attempt live lookup
  season <- new_data$season[1]
  round_num <- new_data$round[1]
  round_url <- tryCatch(
    {
      sched <- f1predicter::schedule |>
        dplyr::filter(
          .data$season == !!season,
          .data$round == !!round_num
        )
      if (nrow(sched) > 0 && "round_url" %in% names(sched)) {
        sched$round_url[1]
      } else {
        NULL
      }
    },
    error = \(e) NULL
  )

  if (is.null(round_url)) {
    cli::cli_inform("Weather lookup skipped (no round URL); using {.val dry}.")
    return("dry")
  }

  tryCatch(
    getWeather(round_url),
    error = \(e) {
      cli::cli_inform(
        "Weather lookup failed; defaulting to {.val dry}."
      )
      "dry"
    }
  )
}


#' Summarise Monte Carlo Simulation Matrix
#'
#' @description
#' Collapses a raw simulation matrix (drivers × simulations) into a per-driver
#' summary tibble suitable for downstream formatting and social posting.
#'
#' @param sim_matrix Integer matrix of size `n_drivers × n_simulations`. Each
#'   cell is the finishing position of driver `i` in simulation `j`, or `NA`
#'   for a DNF.
#' @param driver_ids Character vector of driver IDs (length `n_drivers`).
#' @param season Numeric season year.
#' @param round Numeric round number.
#' @param n_simulations Integer number of simulations (used for probability
#'   denominators).
#'
#' @return A tibble with one row per driver and columns:
#'   `driver_id`, `season`, `round`, `win_prob`, `podium_prob`, `top10_prob`,
#'   `likely_position`, `expected_points`, `position_sd`, `.probs`.
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(sample(1:20, 20 * 1000, replace = TRUE), nrow = 20)
#' ids <- paste0("driver_", seq_len(20))
#' summarise_simulations(mat, ids, 2025, 1, 1000)
#' }
summarise_simulations <- function(
  sim_matrix,
  driver_ids,
  season,
  round,
  n_simulations
) {
  n_drivers <- length(driver_ids)
  n_positions <- n_drivers # max possible finishing position

  pts_system <- gp_points_system()

  # --- Position frequency matrix (n_drivers × n_positions) ---
  probs_matrix <- matrix(0, nrow = n_drivers, ncol = n_positions)
  rownames(probs_matrix) <- driver_ids
  colnames(probs_matrix) <- as.character(seq_len(n_positions))

  win_counts <- integer(n_drivers)
  podium_counts <- integer(n_drivers)
  top10_counts <- integer(n_drivers)
  points_total <- numeric(n_drivers)
  position_sum <- numeric(n_drivers) # for median via sorted sims
  position_sq_sum <- numeric(n_drivers)
  valid_sim_count <- integer(n_drivers) # simulations where driver finished

  for (sim in seq_len(n_simulations)) {
    for (i in seq_len(n_drivers)) {
      pos <- sim_matrix[i, sim]
      if (!is.na(pos) && pos >= 1L && pos <= n_positions) {
        probs_matrix[i, pos] <- probs_matrix[i, pos] + 1L
        if (pos == 1L) {
          win_counts[i] <- win_counts[i] + 1L
        }
        if (pos <= 3L) {
          podium_counts[i] <- podium_counts[i] + 1L
        }
        if (pos <= 10L) {
          top10_counts[i] <- top10_counts[i] + 1L
        }
        pts_key <- as.character(pos)
        pt <- if (pts_key %in% names(pts_system)) pts_system[[pts_key]] else 0
        points_total[i] <- points_total[i] + pt
        position_sum[i] <- position_sum[i] + pos
        position_sq_sum[i] <- position_sq_sum[i] + pos^2
        valid_sim_count[i] <- valid_sim_count[i] + 1L
      }
    }
  }

  # Normalise probs matrix to probabilities
  probs_matrix <- probs_matrix / n_simulations

  # Compute median via weighted mean as an approximation (fast and consistent
  # with the championship simulation's 'avg_position' concept).
  likely_pos <- numeric(n_drivers)
  for (i in seq_len(n_drivers)) {
    likely_pos[i] <- stats::weighted.mean(
      seq_len(n_positions),
      probs_matrix[i, ]
    )
  }

  # Compute SD of finishing positions (over finishing sims only)
  pos_sd <- numeric(n_drivers)
  for (i in seq_len(n_drivers)) {
    n <- valid_sim_count[i]
    if (n > 1) {
      mean_pos <- position_sum[i] / n
      pos_sd[i] <- sqrt(position_sq_sum[i] / n - mean_pos^2)
    } else {
      pos_sd[i] <- NA_real_
    }
  }

  tibble::tibble(
    driver_id = driver_ids,
    season = season,
    round = round,
    win_prob = win_counts / n_simulations,
    podium_prob = podium_counts / n_simulations,
    top10_prob = top10_counts / n_simulations,
    likely_position = likely_pos,
    expected_points = points_total / n_simulations,
    position_sd = pos_sd,
    .probs = I(probs_matrix)
  )
}


# ---------------------------------------------------------------------------
# Qualifying Monte Carlo simulation
# ---------------------------------------------------------------------------

#' Compute Per-Driver Qualifying Position SD
#'
#' Derives the empirical SD of qualifying positions for each driver, modulated
#' by the driver's average qualifying gap (`driver_avg_qgap`) and practice lap
#' ranks. Used internally by `simulate_quali()`.
#'
#' @param new_data A tibble returned by [generate_new_data()].
#' @param historical_data A tibble of historical results from [clean_data()],
#'   used to compute empirical SDs via [calculate_driver_performance()].
#' @param season (`integer(1)`) Season year.
#' @param round (`integer(1)`) Round number.
#' @param weather (`character(1)`) One of `"dry"` or `"wet"`.
#' @param params A named list from [simulation_params()].
#' @returns A tibble with columns `driver_id`, `position_sd`.
#' @keywords internal
.calculate_quali_sim_metrics <- function(
  new_data,
  historical_data,
  season,
  round,
  weather = "dry",
  params = simulation_params()
) {
  driver_ids <- new_data$driver_id
  n <- length(driver_ids)

  # --- Empirical SD from historical qualifying positions ---
  perf <- tryCatch(
    calculate_driver_performance(
      historical_data,
      season = season,
      round = round
    ),
    error = \(e) NULL
  )

  if (!is.null(perf) && "position_sd" %in% names(perf)) {
    sd_lookup <- stats::setNames(perf$position_sd, perf$driver_id)
    position_sd <- vapply(
      driver_ids,
      \(d) {
        v <- sd_lookup[d]
        if (!is.na(v) && is.finite(v) && v > 0) {
          v
        } else {
          params$quali_default_position_sd
        }
      },
      numeric(1)
    )
  } else {
    position_sd <- rep(params$quali_default_position_sd, n)
  }

  # --- Scale SD by driver's average qualifying gap ---
  if ("driver_avg_qgap" %in% names(new_data)) {
    qgap <- pmax(new_data$driver_avg_qgap, 0)
    position_sd <- position_sd * (1 + params$quali_qgap_sd_weight * qgap)
  }

  # --- Weather scaling ---
  if (identical(weather, "wet")) {
    position_sd <- position_sd * params$quali_wet_sd_multiplier
  }

  tibble::tibble(
    driver_id = driver_ids,
    position_sd = position_sd
  )
}


#' Summarise Monte Carlo Qualifying Simulation Matrix
#'
#' @description
#' Collapses a raw qualifying simulation matrix (drivers × simulations) into a
#' per-driver summary tibble suitable for downstream formatting.
#'
#' @param sim_matrix Integer matrix of size `n_drivers × n_simulations`. Each
#'   cell is the qualifying position of driver `i` in simulation `j`.
#' @param driver_ids Character vector of driver IDs (length `n_drivers`).
#' @param season Numeric season year.
#' @param round Numeric round number.
#' @param n_simulations Integer number of simulations.
#'
#' @return A tibble with one row per driver and columns:
#'   `driver_id`, `season`, `round`, `pole_prob`, `top3_prob`, `top10_prob`,
#'   `likely_quali_position`, `position_sd`, `.probs`.
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(sample(1:20, 20 * 1000, replace = TRUE), nrow = 20)
#' ids <- paste0("driver_", seq_len(20))
#' summarise_quali_simulations(mat, ids, 2025, 1, 1000)
#' }
summarise_quali_simulations <- function(
  sim_matrix,
  driver_ids,
  season,
  round,
  n_simulations
) {
  n_drivers <- length(driver_ids)
  n_positions <- n_drivers

  probs_matrix <- matrix(0, nrow = n_drivers, ncol = n_positions)
  rownames(probs_matrix) <- driver_ids
  colnames(probs_matrix) <- as.character(seq_len(n_positions))

  pole_counts <- integer(n_drivers)
  top3_counts <- integer(n_drivers)
  top10_counts <- integer(n_drivers)
  position_sum <- numeric(n_drivers)
  position_sq_sum <- numeric(n_drivers)
  valid_sim_count <- integer(n_drivers)

  for (sim in seq_len(n_simulations)) {
    for (i in seq_len(n_drivers)) {
      pos <- sim_matrix[i, sim]
      if (!is.na(pos) && pos >= 1L && pos <= n_positions) {
        probs_matrix[i, pos] <- probs_matrix[i, pos] + 1L
        if (pos == 1L) {
          pole_counts[i] <- pole_counts[i] + 1L
        }
        if (pos <= 3L) {
          top3_counts[i] <- top3_counts[i] + 1L
        }
        if (pos <= 10L) {
          top10_counts[i] <- top10_counts[i] + 1L
        }
        position_sum[i] <- position_sum[i] + pos
        position_sq_sum[i] <- position_sq_sum[i] + pos^2
        valid_sim_count[i] <- valid_sim_count[i] + 1L
      }
    }
  }

  probs_matrix <- probs_matrix / n_simulations

  likely_pos <- numeric(n_drivers)
  for (i in seq_len(n_drivers)) {
    likely_pos[i] <- stats::weighted.mean(
      seq_len(n_positions),
      probs_matrix[i, ]
    )
  }

  pos_sd <- numeric(n_drivers)
  for (i in seq_len(n_drivers)) {
    n_v <- valid_sim_count[i]
    if (n_v > 1) {
      mean_pos <- position_sum[i] / n_v
      pos_sd[i] <- sqrt(position_sq_sum[i] / n_v - mean_pos^2)
    } else {
      pos_sd[i] <- NA_real_
    }
  }

  tibble::tibble(
    driver_id = driver_ids,
    season = season,
    round = round,
    pole_prob = pole_counts / n_simulations,
    top3_prob = top3_counts / n_simulations,
    top10_prob = top10_counts / n_simulations,
    likely_quali_position = likely_pos,
    position_sd = pos_sd,
    .probs = I(probs_matrix)
  )
}


#' Monte Carlo Qualifying Simulation
#'
#' @description
#' Simulates the qualifying session for a single F1 race weekend using a Monte
#' Carlo approach. Each driver's qualifying position is drawn from a normal
#' distribution whose mean is provided by the ML regression model
#' (`quali_pos`) and whose SD is derived from historical qualifying performance
#' blended with the driver's average qualifying gap (`driver_avg_qgap`) and
#' practice lap ranks.
#'
#' @details
#' The simulation seed is set to `as.integer(format(Sys.Date(), "%Y%m%d"))` for
#' reproducibility within a given day.
#'
#' Weather is resolved via [.resolve_weather()]. If `weather` is `NULL`
#' (default) a dry session is assumed; `"wet"` widens each driver's SD by
#' `params$quali_wet_sd_multiplier`.
#'
#' @param new_data A tibble returned by [generate_new_data()] for the round to
#'   be simulated. Season and round are read from `new_data$season[1]` and
#'   `new_data$round[1]`. Pass `NULL` to load the next upcoming round via
#'   [generate_next_race_data()].
#' @param historical_data A tibble of processed historical results from
#'   [clean_data()], used to derive empirical per-driver qualifying SDs.
#' @param quali_models A named list of fitted model objects as returned by
#'   `model_quali_early()` or `model_quali_late()`. Must contain a `quali_pos`
#'   element. If `NULL` (default), models are loaded from disk using
#'   [load_models()] with timing auto-detected from `new_data`.
#' @param engine (`character(1)`) Model engine used when loading models from
#'   disk. Defaults to `"ensemble"`.
#' @param weather (`character(1)` or `NULL`) Session weather. One of `"dry"` or
#'   `"wet"`. `NULL` (default) assumes dry conditions.
#' @param n_simulations (`integer(1)` or `NULL`) Number of simulations. `NULL`
#'   uses `params$n_simulations` (default 10 000).
#' @param params A named list of simulation parameters as returned by
#'   [simulation_params()].
#'
#' @returns A tibble with one row per driver and columns `driver_id`, `season`,
#'   `round`, `pole_prob`, `top3_prob`, `top10_prob`, `likely_quali_position`,
#'   `position_sd`, `.probs`.
#' @seealso [simulate_race()], [simulation_params()],
#'   [summarise_quali_simulations()]
#' @export
#'
#' @examples
#' \dontrun{
#' nd <- generate_new_data(historical_data, season = 2025, round = 1)
#' result <- simulate_quali(nd, historical_data)
#' }
simulate_quali <- function(
  new_data = NULL,
  historical_data,
  quali_models = NULL,
  engine = "ensemble",
  weather = NULL,
  n_simulations = NULL,
  params = simulation_params()
) {
  if (is.null(new_data)) {
    new_data <- generate_next_race_data()
  }

  n_sim <- as.integer(
    if (is.null(n_simulations)) params$n_simulations else n_simulations
  )

  set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))

  weather <- .resolve_weather(new_data, weather)

  # --- Load / validate models ---
  if (is.null(quali_models) || is.character(quali_models)) {
    if (is.character(quali_models)) {
      rlang::arg_match(quali_models, c("early", "late"))
      model_timing <- quali_models
    } else {
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

  if (!"quali_pos" %in% names(quali_models)) {
    cli::cli_abort(
      "The {.arg quali_models} list must contain a {.val quali_pos} model."
    )
  }

  season <- new_data$season[1]
  round <- new_data$round[1]
  n_drivers <- nrow(new_data)

  # --- ML mean qualifying positions ---
  ml_quali <- .predict_quali_pos(new_data, quali_models$quali_pos)
  mean_pos <- ml_quali$likely_quali_position

  # --- Blend with practice rank if available ---
  practice_col <- if ("practice_optimal_rank" %in% names(new_data)) {
    "practice_optimal_rank"
  } else if ("practice_best_rank" %in% names(new_data)) {
    "practice_best_rank"
  } else {
    NULL
  }

  if (!is.null(practice_col)) {
    w <- params$quali_practice_weight
    mean_pos <- (1 - w) * mean_pos + w * new_data[[practice_col]]
  }

  # --- Per-driver qualifying SDs ---
  sim_metrics <- .calculate_quali_sim_metrics(
    new_data = new_data,
    historical_data = historical_data,
    season = season,
    round = round,
    weather = weather,
    params = params
  )

  # --- Run simulations (no DNFs in qualifying) ---
  sim_matrix <- matrix(NA_integer_, nrow = n_drivers, ncol = n_sim)
  dnf_rates_zero <- rep(0, n_drivers)

  pb <- cli::cli_progress_bar(
    total = n_sim,
    format = "Simulating qualifying {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total})"
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  for (sim in seq_len(n_sim)) {
    cli::cli_progress_update(id = pb)
    sim_matrix[, sim] <- simulate_race_positions(
      mean_pos,
      sim_metrics$position_sd,
      dnf_rates_zero,
      n_drivers
    )
  }

  summarise_quali_simulations(
    sim_matrix = sim_matrix,
    driver_ids = new_data$driver_id,
    season = season,
    round = round,
    n_simulations = n_sim
  )
}
