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
format_race_skeet_predictions <- function(predictions) {
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

  image <- format_results_prob_table(predictions, save_image = TRUE)

  image_alt <- paste0(
    "A table of F1 race predicted results Brighter/more yellow ",
    "colours indicate more likely finishing positions. Darker/more purple colours indicate ",
    "less likely outcomes. For the ",
    race_name,
    ", the predictions have drivers most likely ",
    "in the following order: ",
    paste(predictions_formatted$driver_names, collapse = ", "),
    "."
  )

  return(list(
    text = skeet_body,
    tags = tags,
    image = image$filename,
    image_alt = image_alt
  ))
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
  race_hashtag <- stringr::str_replace_all(race_name, "Grand", "G")
  race_hashtag <- stringr::str_replace_all(race_hashtag, "Prix", "P")
  race_hashtag <- stringr::str_replace_all(race_hashtag, " ", "")

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
    dplyr::arrange(.data$pole_odd) %>%
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
    "🔮 Most Likely Qualifying Position (Top 5):",
    "{position_preds}",
    .sep = "\n"
  )

  # Bluesky tags are handled separately from the text and don't use '#'
  tags <- c("F1", "F1Predictions", "F1Quali", race_hashtag)

  image <- format_quali_prob_table(predictions, save_image = TRUE)

  image_alt <- paste0(
    "A table of F1 qualifying predicted outcomes. Brighter/more yellow ",
    "colours indicate more likely finishing positions. Darker/more purple colours indicate ",
    "less likely outcomes. For the ",
    race_name,
    ", the predictions have drivers most likely ",
    "in the following order: ",
    paste(predictions_formatted$driver_names, collapse = ", "),
    "."
  )

  return(list(
    text = skeet_body,
    tags = tags,
    image = image$filename,
    image_alt = image_alt
  ))
}


#' Post Predictions to Bluesky
#'
#' This function takes a status string and posts it to Bluesky.
#' It requires credentials to be set as environment variables for `bskyr`.
#' See the bskyr documentation for details (e.g., BSKY_HANDLE, BSKY_APP_PASSWORD).
#'
#' @param text The text content of the skeet.
#' @param image file path to a graphic image, if to be added
#' @param image_alt Alternative text for the image, for accessibility.
#' @param tags A character vector of tags to apply to the post.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
post_skeet_predictions <- function(
  text,
  image = NULL,
  image_alt = NULL,
  tags = NULL
) {
  if (!requireNamespace("atrrr", quietly = TRUE)) {
    stop(
      "Package 'atrrr' is required. Please install it with install.packages('atrrr').",
      call. = FALSE
    )
  }

  message("Posting skeet...")
  if (is.null(image)) {
    response <- tryCatch(
      {
        atrrr::post(text = text, tags = tags)
      },
      error = function(e) {
        warning("Failed to post skeet: ", e$message, call. = FALSE)
        return(NULL)
      }
    )
  } else {
    response <- tryCatch(
      {
        atrrr::post(
          text = text,
          image = image,
          image_alt = image_alt,
          tags = tags
        )
      },
      error = function(e) {
        warning("Failed to post skeet: ", e$message, call. = FALSE)
        return(NULL)
      }
    )
  }

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
    tags = formatted_post$tags,
    image = formatted_post$image,
    image_alt = formatted_post$image_alt
  )
}


#' Post Race Predictions to Bluesky
#'
#' A wrapper function that formats race predictions and posts them to Bluesky.
#'
#' @param predictions A data frame of predictions from `predict_round()`.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
#' @export
post_race_predictions <- function(predictions) {
  formatted_post <- format_race_skeet_predictions(predictions)
  post_skeet_predictions(
    text = formatted_post$text,
    tags = formatted_post$tags,
    image = formatted_post$image,
    image_alt = formatted_post$image_alt
  )
}

#' Format Qualifying Probabilities as a Table
#'
#' This function takes the results from `predict_quali_round()` and creates a
#' `gt` table visualizing the probability of each driver achieving each
#' qualifying position.
#'
#' @details
#' The table is formatted with a color heatmap, where brighter/more colorful
#' cells indicate a higher probability. It requires the `gt` package to be
#' installed. The function returns a `gt` object which can be printed or saved
#' using `gt::gtsave()`.
#'
#' @param predictions A data frame of predictions from `predict_quali_round()`.
#'   This must contain the `.probs` list-column with position probabilities.
#' @param save_image A logical value. If `TRUE`, saves the table as a PNG file
#'   and returns a list containing the table object and the file path. If `FALSE`
#'   (default), returns only the `gt_tbl` object.
#'
#' @return A `gt_tbl` object, or a list containing the `gt_tbl` and a filename if `save_image = TRUE`.
#' @export
format_quali_prob_table <- function(predictions, save_image = FALSE) {
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

  probs <- as.data.frame(predictions_formatted$.probs)
  # Wrangle the probability data into a wide format for the table
  prob_data <- predictions_formatted %>%
    dplyr::select("driver_name", "likely_quali_position_class") %>%
    dplyr::bind_cols(probs) %>%
    dplyr::arrange(.data$likely_quali_position_class, -.data$`1`)

  # Create the gt table
  prob_table <- prob_data %>%
    dplyr::select(-"likely_quali_position_class") %>%
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
    gt::cols_label(driver_name = "Driver") %>%
    gt::tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = gt::px(3)
    ) %>%
    gt::tab_source_note(
      source_note = paste0("Generated: ", Sys.Date(), " | @bot.bulsink.ca")
    )
  for (i in seq_len(nrow(prob_data))) {
    prob_table <- gt::data_color(
      prob_table,
      columns = -driver_name,
      rows = i,
      direction = 'row',
      palette = "viridis"
    )
  }
  if (!save_image) {
    return(prob_table)
  } else {
    tempdir <- tempdir(check = TRUE)
    filename <- tempfile(pattern = "preds", tmpdir = tempdir, fileext = ".png")
    gt::gtsave(prob_table, filename = filename)
    return(list(prob_table = prob_table, filename = filename))
  }
}
