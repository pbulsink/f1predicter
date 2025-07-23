# This script provides functions to format and post F1 predictions to social media,
# inspired by the pbulsink/HockeyModel repository.
#
# It requires the 'rtweet', 'glue', and 'scales' packages.
# install.packages(c("rtweet", "glue", "scales"))

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


#' Format Predictions for a Tweet
#'
#' @param predictions A data frame of predictions from `predict_round()`
#'
#' @return A formatted string ready for tweeting.
format_tweet_predictions <- function(predictions) {
  # Assumes f1predicter::drivers is loaded for full names
  predictions_formatted <- predictions %>%
    dplyr::left_join(f1predicter::drivers, by = "driver_id") %>%
    dplyr::mutate(driver_name = paste(.data$given_name, .data$family_name))

  current_season <- predictions$season[1]
  current_round <- predictions$round[1]
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

  # Combine into a single tweet body
  tweet_body <- glue::glue(
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
    "",
    "#F1 #F1Predictions #{race_hashtag}",
    .sep = "\n"
  )

  return(tweet_body)
}


#' Post Predictions to Twitter
#'
#' This function takes a status string and posts it to Twitter.
#' It requires API keys to be set as environment variables.
#' See `?rtweet::create_token` for details.
#'
#' @param status The text content of the tweet.
#'
#' @return Invisibly returns the response from the Twitter API, or NULL on failure.
post_tweet_predictions <- function(status) {
  if (!requireNamespace("rtweet", quietly = TRUE)) {
    stop(
      "Package 'rtweet' is required. Please install it with install.packages('rtweet').",
      call. = FALSE
    )
  }

  # rtweet will automatically use a token from environment variables if available.
  # This is the best practice for non-interactive scripts.
  if (!rtweet::auth_has_default()) {
    message(
      "No default rtweet token found. See `?rtweet::create_token` for setup."
    )
    message(
      "You must set Twitter API environment variables for this to work non-interactively."
    )
    return(invisible(NULL))
  }

  message("Posting tweet...")
  response <- tryCatch(
    {
      rtweet::post_tweet(status = status)
    },
    error = function(e) {
      warning("Failed to post tweet: ", e$message)
      return(NULL)
    }
  )

  if (!is.null(response)) {
    message("Tweet posted successfully!")
  }

  return(invisible(response))
}
