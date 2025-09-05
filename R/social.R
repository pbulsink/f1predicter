# This script provides functions to format and post F1 predictions to social media,
# inspired by the pbulsink/HockeyModel repository.
#
# It requires the 'bskyr', 'glue', 'scales', and 'gt' packages.
# install.packages(c("bskyr", "glue", "scales", "gt"))

#' Get Driver Name from ID
#'
#' @description
#' Returns the full name of a driver based on their `driver_id`.
#' This function contains an internal lookup table and is vectorized.
#'
#' @param season The season to load drivers for.
#' @param driver_ids A character vector of driver IDs.
#'
#' @return A character vector of full driver names. If an ID is not found,
#'   it returns a title-cased version of the ID as a fallback.
#' @noRd
get_driver_name <- function(season, driver_ids) {
  if (!requireNamespace("f1dataR", quietly = TRUE)) {
    stop(
      "Package 'f1dataR' is required. Please install it.",
      call. = FALSE
    )
  }

  # Load drivers for the specified season
  driver_lookup <- tryCatch(
    {
      f1dataR::load_drivers(season = season)
    },
    error = function(e) {
      warning(
        "Could not load drivers using f1dataR for season ",
        season,
        ": ",
        e$message,
        call. = FALSE
      )
      return(NULL)
    }
  )

  if (is.null(driver_lookup)) {
    # Fallback to string manipulation if API fails
    return(stringr::str_to_title(gsub("_", " ", driver_ids)))
  }

  driver_lookup <- driver_lookup %>%
    dplyr::mutate(full_name = paste(.data$given_name, .data$family_name))

  # Match driver_ids with the lookup table
  match_indices <- match(driver_ids, driver_lookup$driver_id)
  found_names <- driver_lookup$full_name[match_indices]

  # For any IDs not found, create a fallback name
  not_found_indices <- which(is.na(match_indices))
  if (length(not_found_indices) > 0) {
    fallback_names <- stringr::str_to_title(gsub(
      "_",
      " ",
      driver_ids[not_found_indices]
    ))
    found_names[not_found_indices] <- fallback_names
  }

  return(found_names)
}

#' Get Race Name
#'
#' @param season The season year
#' @param round The round number
#'
#' @return A string with the race name, e.g. "Bahrain Grand Prix"
get_race_name <- function(season, round) {
  # Assumes f1predicter::schedule is available in the package namespace
  race_info <- f1predicter::schedule %>%
    dplyr::filter(.data$season == !!season, .data$round == !!round)

  if (nrow(race_info) == 0) {
    return(glue::glue("Race {round}"))
  }

  return(race_info$race_name[1])
}


#' Format Predictions for a Skeet
#'
#' @param predictions A data frame of predictions from `predict_round()`
#'
#' @return A list containing the formatted string for the skeet body and a vector of tags.
format_skeet_predictions <- function(predictions) {
  current_season <- predictions$season[1]
  current_round <- predictions$round[1]

  predictions_formatted <- predictions %>%
    dplyr::mutate(
      driver_name = get_driver_name(current_season, .data$driver_id)
    )

  race_name <- get_race_name(current_season, current_round)
  race_hashtag <- stringr::str_replace_all(race_name, " ", "")

  # Top 3 for Win
  win_preds <- predictions_formatted %>%
    dplyr::arrange(dplyr::desc(.data$win_odd)) %>%
    dplyr::slice_head(n = 3) %>%
    dplyr::mutate(
      text = glue::glue(
        "{dplyr::row_number()}. {.data$driver_name}: {scales::percent(.data$win_odd, 0.1)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Top 5 for Podium
  podium_preds <- predictions_formatted %>%
    dplyr::arrange(dplyr::desc(.data$podium_odd)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{dplyr::row_number()}. {.data$driver_name}: {scales::percent(.data$podium_odd, 0.1)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Top 5 likely positions (ordered by win probability)
  position_preds <- predictions_formatted %>%
    dplyr::arrange(dplyr::desc(.data$win_odd)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue("{.data$driver_name}: P{.data$likely_position}")
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Combine into a single skeet body
  skeet_body <- glue::glue(
    "Predictions for the {race_name} 🏎️",
    "",
    "🏆 Win Chance:",
    "{win_preds}",
    "",
    "🍾 Podium Chance:",
    "{podium_preds}",
    "",
    "🔮 Likely Finishing Position (Top 5):",
    "{position_preds}",
    .sep = "\n"
  )

  # Bluesky tags are handled separately from the text and don't use '#'
  tags <- c("F1", "F1Predictions", race_hashtag)

  return(list(text = skeet_body, tags = tags))
}


#' Format Qualifying Predictions for a Skeet
#'
#' @param predictions A data frame of predictions from `predict_quali_round()`
#'
#' @return A list containing the formatted string for the skeet body and a vector of tags.
format_quali_skeet_predictions <- function(predictions) {
  current_season <- predictions$season[1]
  current_round <- predictions$round[1]

  predictions_formatted <- predictions %>%
    dplyr::mutate(
      driver_name = get_driver_name(current_season, .data$driver_id)
    )

  race_name <- get_race_name(current_season, current_round)
  race_hashtag <- stringr::str_replace_all(race_name, " ", "")

  # Top 5 for Pole
  pole_preds <- predictions_formatted %>%
    dplyr::arrange(dplyr::desc(.data$pole_odd)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{dplyr::row_number()}. {.data$driver_name}: {scales::percent(.data$pole_odd, 0.1)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Top 5 likely qualifying positions (ordered by pole probability)
  position_preds <- predictions_formatted %>%
    dplyr::arrange(dplyr::desc(.data$pole_odd)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{.data$driver_name}: P{round(.data$likely_quali_position, 0)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Combine into a single skeet body
  skeet_body <- glue::glue(
    "Qualifying Predictions for the {race_name} 🏎️",
    "",
    "Pole Position Chance:",
    "{pole_preds}",
    "",
    "🔮 Likely Qualifying Position (Top 5):",
    "{position_preds}",
    .sep = "\n"
  )

  # Bluesky tags are handled separately from the text and don't use '#'
  tags <- c("F1", "F1Predictions", "F1Quali", race_hashtag)

  return(list(text = skeet_body, tags = tags))
}


#' Post Predictions to Bluesky
#'
#' This function takes a status string and posts it to Bluesky.
#' It requires credentials to be set as environment variables for `bskyr`.
#' See the bskyr documentation for details (e.g., BSKY_HANDLE, BSKY_APP_PASSWORD).
#'
#' @param text The text content of the skeet.
#' @param tags A character vector of tags to apply to the post.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
post_skeet_predictions <- function(text, tags = NULL) {
  if (!requireNamespace("bskyr", quietly = TRUE)) {
    stop(
      "Package 'bskyr' is required. Please install it with install.packages('bskyr').",
      call. = FALSE
    )
  }

  # bskyr can automatically use environment variables for authentication.
  # Check if the handle and password variables are set.
  if (
    Sys.getenv("BSKY_HANDLE") == "" || Sys.getenv("BSKY_APP_PASSWORD") == ""
  ) {
    message(
      "No Bluesky handle or app password found in environment variables.",
      "See bskyr documentation for non-interactive setup.",
      "Set BSKY_HANDLE and BSKY_APP_PASSWORD for this to work."
    )
    return(invisible(NULL))
  }

  message("Posting skeet...")
  response <- tryCatch(
    {
      # bskyr::login() will use env vars automatically
      bskyr::login()
      bskyr::post_post(text = text, tags = tags)
    },
    error = function(e) {
      warning("Failed to post skeet: ", e$message, call. = FALSE)
      return(NULL)
    }
  )

  if (!is.null(response)) {
    message("Skeet posted successfully!")
  }

  return(invisible(response))
}

#' Post Qualifying Predictions to Bluesky
#'
#' A wrapper function that formats qualifying predictions and posts them to Bluesky.
#'
#' @param predictions A data frame of predictions from `predict_quali_round()`.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
#' @export
post_quali_predictions <- function(predictions) {
  formatted_post <- format_quali_skeet_predictions(predictions)
  post_skeet_predictions(
    text = formatted_post$text,
    tags = formatted_post$tags
  )
}

#' Format Qualifying Probabilities as a Table
#'
#' This function takes the results from `predict_quali_round()` and creates a
#' `gt` table visualizing the probability of each driver achieving each
#' qualifying position.
#'
#' @details
#' The table is formatted with a color heatmap, where darker/more colorful
#' cells indicate a higher probability. It requires the `gt` package to be
#' installed. The function returns a `gt` object which can be printed or saved
#' using `gt::gtsave()`.
#'
#' @param predictions A data frame of predictions from `predict_quali_round()`.
#'   This must contain the `.probs` list-column with position probabilities.
#'
#' @return A `gt_tbl` object.
#' @export
format_quali_prob_table <- function(predictions) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for this function. Please install it with `install.packages('gt')`.",
      call. = FALSE
    )
  }

  if (!".probs" %in% names(predictions)) {
    stop(
      "The `predictions` data frame must contain a '.probs' column.",
      call. = FALSE
    )
  }

  # Get race info for the title
  current_season <- predictions$season[1]
  current_round <- predictions$round[1]

  # Get driver names
  predictions_formatted <- predictions %>%
    dplyr::mutate(
      driver_name = get_driver_name(current_season, .data$driver_id)
    )

  race_name <- get_race_name(current_season, current_round)

  probs <- as.data.frame(prob_data$.probs)
  # Wrangle the probability data into a wide format for the table
  prob_data <- predictions_formatted %>%
    dplyr::select(
      .data$driver_name,
      .data$likely_quali_position_class
    ) %>%
    dplyr::bind_cols(probs) %>%
    dplyr::arrange(-.data$`1`) %>%
    dplyr::select(.data$driver_name, dplyr::matches("^\\d+$"))

  # Create the gt table
  prob_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Qualifying Position Probabilities**"),
      subtitle = race_name
    ) %>%
    gt::tab_spanner(
      label = "Odds of Qualifying at Each Position",
      columns = -driver_name
    ) %>%
    gt::fmt_percent(columns = -driver_name, decimals = 1) %>%
    gt::data_color(
      columns = -driver_name,
      palette = "viridis",
      domain = range(as.matrix(prob_data[, -1]), na.rm = TRUE)
    ) %>%
    gt::cols_label(driver_name = "Driver") %>%
    gt::tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = gt::px(3)
    )
}
