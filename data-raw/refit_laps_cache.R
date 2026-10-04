# Re-fetch and re-cache the laps table only.
#
# Re-runs the laps pipeline from get_weekend_data() for every completed round
# in the given season(s) and overwrites the `laps` table in the SQLite cache,
# so that newly-added lap columns (e.g. `lap_start_time`) are backfilled into
# existing races. Unlike get_weekend_data(force = TRUE), this script does NOT
# touch results, quali, pitstops, grids, or sprint_results.
#
# The only extra API call beyond the laps sessions is one load_results() per
# round, which is required because the cached laps table stores `constructor_id`
# (joined from results) alongside `driver_id`.
#
# Usage:
#   Rscript data-raw/refit_laps_cache.R
#   # or, to limit to specific seasons:
#   Rscript data-raw/refit_laps_cache.R 2024 2025

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
seasons <- if (length(args) > 0) {
  as.numeric(args)
} else {
  2018:f1dataR::get_current_season()
}

schedule <- f1predicter::schedule |>
  dplyr::mutate(
    date = as.Date(.data$date),
    season = as.numeric(.data$season),
    round = as.numeric(.data$round),
    sprint_date = as.Date(.data$sprint_date)
  ) |>
  tibble::as_tibble()

# Mirror the laps block of get_weekend_data() without the other datasets.
refit_laps_for_round <- function(season, round, con, cache_writes_allowed) {
  laps <- f1predicter:::get_laps(season = season, round = round)
  laps <- f1predicter:::add_drivers_to_laps(laps, season = season)

  # constructor_id lives in results; this is the only non-laps fetch needed.
  results <- try(
    f1dataR::load_results(season = season, round = round),
    silent = TRUE
  )
  if (inherits(results, "try-error") || is.null(results)) {
    if (round > 1) {
      results <- try(
        f1dataR::load_results(season = season, round = round - 1),
        silent = TRUE
      )
    } else {
      results <- try(
        f1dataR::load_results(season = season - 1),
        silent = TRUE
      )
    }
  }

  if (inherits(results, "try-error") || is.null(results)) {
    return(NULL)
  }

  laps <- laps |>
    dplyr::left_join(
      results[, c("driver_id", "constructor_id")],
      by = c("driver_id")
    ) |>
    dplyr::mutate(season = season, round = round) |>
    dplyr::select(
      "driver_id",
      "constructor_id",
      "lap_time",
      "lap_number",
      "lap_start_time",
      "stint",
      "sector1time",
      "sector2time",
      "sector3time",
      "speed_i1",
      "speed_i2",
      "speed_fl",
      "speed_st",
      "is_personal_best",
      "compound",
      "tyre_life",
      "fresh_tyre",
      "track_status",
      "position",
      "deleted",
      "deleted_reason",
      "is_accurate",
      "air_temp",
      "humidity",
      "pressure",
      "rainfall",
      "track_temp",
      "wind_direction",
      "wind_speed",
      "session_type",
      "season",
      "round"
    )

  if (cache_writes_allowed) {
    f1predicter:::write_cache_table(laps, "laps", con, overwrite = TRUE)
  }

  laps
}

con <- f1predicter:::open_cache_db()
on.exit(DBI::dbDisconnect(con), add = TRUE)

n_done <- 0
n_skip <- 0

for (season in seasons) {
  rounds <- schedule[
    schedule$season == season & schedule$date <= Sys.Date(),
  ]$round

  if (length(rounds) == 0) {
    cli::cli_warn("No completed rounds for season {season}; skipping.")
    next
  }

  cli::cli_h1("Refitting laps for season {season}")

  for (round in rounds) {
    cache_writes_allowed <- f1predicter:::.can_cache_event_data(
      season = season,
      round = round,
      schedule = schedule
    )

    if (!cache_writes_allowed) {
      cli::cli_warn(
        "Skipping {season} round {round}: race date has not fully passed."
      )
      n_skip <- n_skip + 1
      next
    }

    cli::cli_inform("Refitting {season} round {round}...")
    res <- try(
      refit_laps_for_round(
        season,
        as.numeric(round),
        con,
        cache_writes_allowed
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      cli::cli_warn(
        "Failed {season} round {round}: {conditionMessage(res)}"
      )
      n_skip <- n_skip + 1
      next
    }

    if (is.null(res)) {
      cli::cli_warn("No laps available for {season} round {round}; skipped.")
      n_skip <- n_skip + 1
      next
    }

    n_done <- n_done + 1
  }
}

cli::cli_alert_success(
  "Done: {n_done} round(s) refit, {n_skip} skipped."
)
