# This script is designed to be run automatically on an hourly basis (e.g., via cron).
# It checks a schedule of F1 races and, if the timing is appropriate,
# generates and posts predictions for qualifying sessions or races.

# --- Configuration ---
# Path to the schedule file. Assumes the script is run from the project root.
SCHEDULE_FILE <- "R/autorunner.csv"

# --- 1. Load necessary libraries and source package functions ---
# It's good practice to load all required packages at the top.
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load the f1predicter package from source. This makes functions like
# post_quali_predictions() and post_race_predictions() available.
devtools::load_all()


# --- 2. Define Helper Functions ---

#' Load and Process the Schedule
#'
#' Reads the schedule file, which is a CSV commented out in an R file.
#' It parses dates and handles local timezones.
#'
#' @param file_path The path to the schedule file.
#' @return A tibble with the schedule, with date columns parsed to POSIXct.
load_schedule <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Schedule file not found at: ", file_path)
  }

  # The schedule is in a commented R file, so we read it as a CSV,
  # skipping the initial comment lines.
  schedule_data <- readr::read_csv(file_path, comment = "#")

  # Parse base event times and calculate prediction trigger times
  schedule_processed <- schedule_data %>%
    dplyr::mutate(
      # Ensure base date/time columns are in the correct format and timezone
      quali_time = lubridate::ymd_hm(.data$quali_dt, tz = Sys.timezone()),
      race_time = lubridate::ymd_hm(.data$race_dt, tz = Sys.timezone()),

      # --- Calculate Qualifying Prediction Times ---
      # `quali_early` model: 2 days before qualifying
      quali_early_trigger = quali_time - lubridate::days(2),
      # `quali_late` model: 18 hours before qualifying
      quali_late_trigger1 = quali_time - lubridate::hours(18),
      # `quali_late` model: 1 hour before qualifying
      quali_late_trigger2 = quali_time - lubridate::hours(1),

      # --- Calculate Race Prediction Times ---
      # `race_early` model: 3 days before the race
      race_early_trigger = race_time - lubridate::days(3),
      # `race_late` model: 30 hours days before the race
      race_late_trigger = race_time - lubridate::hours(30),
      # `race_after_quali` model: 3 hours after qualifying starts
      race_after_quali_trigger1 = quali_time + lubridate::hours(3),
      # `race_after_quali` model: 1 hour before the race starts
      race_after_quali_trigger2 = race_time - lubridate::hours(1)
    )
  return(schedule_processed)
}

#' Check if a Prediction Should Run
#'
#' Compares the current time to a scheduled prediction time to see if it's
#' within the last hour.
#'
#' @param current_time The current time (POSIXct).
#' @param scheduled_time The time the prediction is scheduled for (POSIXct).
#' @return TRUE if the script should run, FALSE otherwise.
should_run_now <- function(current_time, scheduled_time) {
  # Don't run if the scheduled time is in the future or not specified.
  if (is.na(scheduled_time) || current_time < scheduled_time) {
    return(FALSE)
  }

  # Check if the current time is within one hour past the scheduled time.
  time_diff_hours <- as.numeric(difftime(
    current_time,
    scheduled_time,
    units = "hours"
  ))
  return(time_diff_hours >= 0 && time_diff_hours < 1)
}

# --- 3. Main Execution Logic ---

#' Main function to run the prediction checks.
run_prediction_checks <- function() {
  cli::cli_h1("Checking for F1 Predictions to Run")

  current_time <- lubridate::now()
  cli::cli_inform(
    "Current time: {format(current_time, '%Y-%m-%d %H:%M:%S %Z')}"
  )

  schedule <- load_schedule(SCHEDULE_FILE)

  # Loop through each race in the schedule
  for (i in 1:nrow(schedule)) {
    event <- schedule[i, ]
    cli::cli_h2("Checking: {event$Race} (Round {event$round})")

    # --- Qualifying Predictions (using 'early' and 'late' models) ---
    if (should_run_now(current_time, event$quali_early_trigger)) {
      cli::cli_alert_info(
        "Running EARLY qualifying predictions for {event$Race}..."
      )
      f1predicter::post_quali_predictions(
        season = event$season,
        round = event$round,
        timing = "early"
      )
    }
    if (
      should_run_now(current_time, event$quali_late_trigger1) ||
        should_run_now(current_time, event$quali_late_trigger2)
    ) {
      cli::cli_alert_info(
        "Running LATE qualifying predictions for {event$Race}..."
      )
      f1predicter::post_quali_predictions(
        season = event$season,
        round = event$round,
        timing = "late"
      )
    }

    # --- Race Predictions (using 'early', 'late', and 'after_quali' models) ---
    if (should_run_now(current_time, event$race_early_trigger)) {
      cli::cli_alert_info("Running EARLY race predictions for {event$Race}...")
      f1predicter::post_race_predictions(
        season = event$season,
        round = event$round,
        timing = "early"
      )
    }
    if (should_run_now(current_time, event$race_late_trigger)) {
      cli::cli_alert_info("Running LATE race predictions for {event$Race}...")
      f1predicter::post_race_predictions(
        season = event$season,
        round = event$round,
        timing = "late"
      )
    }
    if (
      should_run_now(current_time, event$race_after_quali_trigger1) ||
        should_run_now(current_time, event$race_after_quali_trigger2)
    ) {
      cli::cli_alert_info(
        "Running AFTER QUALI race predictions for {event$Race}..."
      )
      f1predicter::post_race_predictions(
        season = event$season,
        round = event$round,
        timing = "after_quali"
      )
    }
  }

  cli::cli_h1("Prediction run check complete.")
}

# --- 4. Run the script ---
# This makes the script runnable from the command line via `Rscript R/run_predictions.R`
if (!interactive()) {
  run_prediction_checks()
}
