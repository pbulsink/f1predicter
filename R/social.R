# This script provides functions to format and post F1 predictions to social media,
# inspired by the pbulsink/HockeyModel repository.
#
# It requires the 'atrrr', 'glue', 'scales', and 'gt' packages.
# install.packages(c("atrrr", "glue", "scales", "gt"))

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
    cli::cli_abort(c(
      "Package {.pkg f1dataR} is required to get driver names."
    ))
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
  race_hashtag <- stringr::str_replace_all(race_hashtag, "Grand", "G")
  race_hashtag <- stringr::str_replace_all(race_hashtag, "Prix", "P")

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

  # Top 5 likely positions (ordered by predicted position)
  position_preds <- predictions_formatted %>%
    dplyr::arrange(.data$likely_position) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{.data$driver_name}: P{round(.data$likely_position, 1)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  # Bluesky tags are handled separately from the text and don't use '#'
  tags <- c("F1", "F1Predictions", race_hashtag)

  prob_image <- format_results_prob_table(predictions, save_image = TRUE)
  odds_image <- format_results_odds_table(predictions, save_image = TRUE)

  driver_list <- predictions_formatted %>%
    dplyr::arrange(.data$likely_position) %>%
    dplyr::pull(.data$driver_name)

  prob_image_alt <- paste0(
    "A table of F1 race predicted results Brighter/more yellow ",
    "colours indicate more likely finishing positions. Darker/more purple colours indicate ",
    "less likely outcomes. For the ",
    race_name,
    ", the predictions have drivers most likely ",
    "in the following order: ",
    paste(driver_list, collapse = ", "),
    "."
  )

  driver_list <- predictions_formatted %>%
    dplyr::arrange(.data$win_odd) %>%
    dplyr::pull(.data$driver_name)

  odds_image_alt <- paste0(
    "A table of F1 race predicted results Brighter/more yellow ",
    "colours indicate more likely finishing positions. Darker/more purple colours indicate ",
    "less likely outcomes. For the ",
    race_name,
    ", the predictions have drivers ranked with highest to lowest chance in this order: ",
    paste(driver_list, collapse = ", "),
    "."
  )

  skeet1_body <- glue::glue(
    "#F1 Predictions for the {race_name} 🏎️",
    "",
    "🏆 Win Chance:",
    "{win_preds}",
    .sep = "\n"
  )

  skeet2_body <- glue::glue(
    "🍾 Podium Chance (Top 5):",
    "{podium_preds}",
    .sep = "\n"
  )

  skeet3_body <- glue::glue(
    "🔮 Most Likely Drivers to finish in top 5, with position:",
    "{position_preds}",
    "\n#F1 #{race_hashtag}",
    .sep = "\n"
  )

  return(list(
    list(
      text = skeet1_body,
      tags = tags,
      image = odds_image$filename,
      image_alt = odds_image_alt
    ),
    list(text = skeet2_body),
    list(
      text = skeet3_body,
      image = prob_image$filename,
      image_alt = prob_image_alt
    )
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
    dplyr::arrange(.data$likely_quali_position) %>%
    dplyr::filter(.data$likely_quali_position <= 5) %>%
    dplyr::mutate(
      text = glue::glue(
        "{.data$driver_name}: P{round(.data$likely_quali_position, 0)}"
      )
    ) %>%
    dplyr::pull(.data$text) %>%
    paste(collapse = "\n")

  image <- format_quali_prob_table(predictions, save_image = TRUE)
  driver_list <- predictions_formatted %>%
    dplyr::arrange(.data$likely_quali_position) %>%
    dplyr::pull(.data$driver_name)

  image_alt <- paste0(
    "A table of F1 qualifying predicted outcomes. Brighter/more yellow ",
    "colours indicate more likely finishing positions. Darker/more purple colours indicate ",
    "less likely outcomes. For the ",
    race_name,
    ", the predictions have drivers most likely ",
    "in the following order: ",
    paste(driver_list, collapse = ", "),
    "."
  )

  # Bluesky tags are handled separately from the text and don't use '#'
  tags <- c("F1", "F1Predictions", "F1Quali", race_hashtag)

  skeet1_body <- glue::glue(
    "#F1 Qualifying Predictions for the {race_name} 🏎️",
    "",
    "Pole Position Chance:",
    "{pole_preds}",
    .sep = "\n"
  )

  skeet2_body <- glue::glue(
    "🔮 Most Likely Drivers to qualify in top 5, with position:",
    "{position_preds}",
    "\n#F1 #{race_hashtag}",
    .sep = "\n"
  )

  return(list(
    list(text = skeet1_body, tags = tags),
    list(text = skeet2_body, image = image$filename, image_alt = image_alt)
  ))
}


#' Post Predictions to Bluesky
#'
#' This function takes a status string and posts it to Bluesky.
#' It requires credentials to be set as environment variables for `bskyr`.
#' See the bskyr documentation for details (e.g., BSKY_HANDLE, BSKY_APP_PASSWORD).
#'
#' @param skeets A list of skeets to post. Each element of the list should be
#'   another list containing `text`, and optionally `image`, `image_alt`, and `tags`.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
post_skeet_predictions <- function(skeets) {
  if (!requireNamespace("atrrr", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg atrrr} is required. Please install it with {.code install.packages('atrrr')}."
    )
  }

  if (!is.list(skeets) || !is.list(skeets[[1]])) {
    cli::cli_abort("{.arg skeets} must be a list of lists.")
  }

  last_post_response <- NULL
  all_responses <- list()

  for (i in seq_along(skeets)) {
    skeet <- skeets[[i]]

    # Determine if this is a reply
    reply_to_arg <- if (i > 1 && !is.null(last_post_response)) {
      last_post_response$uri
    } else {
      NULL
    }

    cli::cli_inform("Posting skeet {i} of {length(skeets)}...")

    response <- tryCatch(
      {
        atrrr::post_skeet(
          text = skeet$text,
          image = skeet$image,
          image_alt = skeet$image_alt,
          tags = skeet$tags,
          in_reply_to = reply_to_arg
        )
      },
      error = function(e) {
        cli::cli_warn("Failed to post skeet {i}: {e$message}")
        return(NULL)
      }
    )

    if (is.null(response)) {
      cli::cli_warn("Aborting thread due to posting failure.")
      break
    }

    cli::cli_inform("Skeet {i} posted successfully!")
    last_post_response <- response
    all_responses[[i]] <- response

    # Add a small delay between posts
    if (i < length(skeets)) Sys.sleep(2)
  }

  invisible(all_responses)
}

#' Post Qualifying Predictions to Bluesky
#'
#' A wrapper function that formats qualifying predictions and posts them to Bluesky.
#'
#' @param predictions A data frame of predictions from `predict_quali_round()`.
#'
#' @return Invisibly returns the response from the Bluesky API, or NULL on failure.
#' @export
post_quali_predictions <- function(predictions = predict_quali_round()) {
  skeet_thread <- format_quali_skeet_predictions(predictions)
  post_skeet_predictions(skeets = skeet_thread)
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
  # Race predictions are shorter, so we post as a single skeet
  skeet_list <- list(format_race_skeet_predictions(predictions))
  post_skeet_predictions(skeets = skeet_list)
}

#' Format Results Probabilities as a Table
#'
#' This function takes the results from `predict_round()` and creates a
#' `gt` table visualizing the probability of each driver achieving each
#' race position.
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
format_results_prob_table <- function(predictions, save_image = FALSE) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required for this function. Please install it with {.code `install.packages('gt')`}."
    )
  }

  if (!".probs" %in% names(predictions)) {
    cli::cli_abort(
      "The {.arg predictions} data frame must contain a {.val .probs} column."
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
    dplyr::select("driver_name", "win_odd", "likely_position_class") %>%
    dplyr::bind_cols(probs) %>%
    dplyr::arrange(.data$likely_position_class)

  # Create the gt table
  prob_table <- prob_data %>%
    dplyr::select(-"likely_position_class") %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Race Finishing Position Probabilities**"),
      subtitle = race_name
    ) %>%
    gt::tab_spanner(
      label = "Odds of Finishing at Each Position",
      columns = -c("driver_name")
    ) %>%
    gt::fmt_percent(columns = -driver_name, decimals = 1) %>%
    gt::cols_label(
      driver_name = "Driver",
      win_odd = "Chance of Winning"
    ) %>%
    gt::tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = gt::px(3)
    ) %>%
    gt::tab_source_note(
      source_note = paste0(
        "Each result model trained independently.\n",
        "Generated: ",
        Sys.Date(),
        " | @bot.bulsink.ca"
      )
    )

  for (i in seq_len(nrow(prob_data))) {
    prob_table <- gt::data_color(
      prob_table,
      columns = -c('driver_name'), # Check this works instead of columns = -driver_name
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
    gt::gtsave(prob_table, filename = filename, vwidth = 1400)
    return(list(prob_table = prob_table, filename = filename))
  }
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
    cli::cli_abort(
      "Package {.pkg gt} is required for this function. Please install it with {.code `install.packages('gt')`}."
    )
  }

  if (!".probs" %in% names(predictions)) {
    cli::cli_abort(
      "The {.arg predictions} data frame must contain a {.val .probs} column."
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
    dplyr::select("driver_name", "pole_odd", "likely_quali_position_class") %>%
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
      columns = -c("driver_name", "pole_odd")
    ) %>%
    gt::fmt_percent(columns = -driver_name, decimals = 1) %>%
    gt::cols_label(driver_name = "Driver", pole_odd = "Pole Odds") %>%
    gt::tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = gt::px(3)
    ) %>%
    gt::tab_source_note(
      source_note = paste0(
        "Results from two separate models: odds of pole and likely finishing position.\n",
        "Generated: ",
        Sys.Date(),
        " | @bot.bulsink.ca"
      )
    ) %>%
    gt::data_color(
      'pole_odd', # Check this works instead of columns = -driver_name
      direction = 'column',
      palette = "viridis"
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        color = "white",
        weight = gt::px(3),
        style = "solid"
      ),
      locations = gt::cells_body(columns = 'pole_odd')
    )

  for (i in seq_len(nrow(prob_data))) {
    prob_table <- gt::data_color(
      prob_table,
      columns = -c('driver_name', 'pole_odd'), # Check this works instead of columns = -driver_name
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
    gt::gtsave(prob_table, filename = filename, vwidth = 1400)
    return(list(prob_table = prob_table, filename = filename))
  }
}


#' Format Results Probabilities as a Table
#'
#' This function takes the results from `predict_round()` and creates a
#' `gt` table visualizing the probability of each driver achieving each
#' race position.
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
format_results_odds_table <- function(predictions, save_image = FALSE) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required for this function. Please install it with {.code `install.packages('gt')`}."
    )
  }

  if (!".probs" %in% names(predictions)) {
    cli::cli_abort(
      "The {.arg predictions} data frame must contain a {.val .probs} column."
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

  prob_data <- predictions_formatted %>%
    dplyr::select("driver_name", "win_odd", "podium_odd", "t10_odd") %>%
    dplyr::arrange(-.data$win_odd)

  # Create the gt table
  prob_table <- prob_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Race Results Odds**"),
      subtitle = race_name
    ) %>%
    gt::fmt_percent(columns = -driver_name, decimals = 1) %>%
    gt::cols_label(
      driver_name = "Driver",
      win_odd = "Win Odds",
      podium_odd = "Podium Odds",
      t10_odd = "Top 10 Odds"
    ) %>%
    gt::tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = gt::px(3)
    ) %>%
    gt::tab_source_note(
      source_note = paste0("Generated: ", Sys.Date(), " | @bot.bulsink.ca")
    ) %>%
    gt::data_color(
      columns = "win_odd",
      direction = 'column',
      palette = "viridis"
    ) %>%
    gt::data_color(
      columns = "podium_odd",
      direction = 'column',
      palette = "viridis"
    ) %>%
    gt::data_color(
      columns = "t10_odd",
      direction = 'column',
      palette = "viridis"
    )

  if (!save_image) {
    return(prob_table)
  } else {
    tempdir <- tempdir(check = TRUE)
    filename <- tempfile(pattern = "preds", tmpdir = tempdir, fileext = ".png")
    gt::gtsave(prob_table, filename = filename)
    return(list(prob_table = prob_table, filename = filename))
  }
}
