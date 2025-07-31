# Data

# Functions to build data files

#' Get Weekend's laps
#'
#' @description
#' Get all laps (timing) from a weekend and return as one data.frame
#'
#' @param season season, from 2018 onward
#' @param round round, compatible with fastf1 round selector
#'
#' @return a data.frame with all lap information
get_laps <- function(season, round) {
  r <- get_laps_or_null(season = season, round = round, session = "R")
  quali <- get_laps_or_null(season = season, round = round, session = "Q")
  fp1 <- get_laps_or_null(season = season, round = round, session = "FP1")
  fp2 <- get_laps_or_null(season = season, round = round, session = "FP2")
  fp3 <- get_laps_or_null(season = season, round = round, session = "FP3")
  s <- get_laps_or_null(season = season, round = round, session = "S")
  ss <- get_laps_or_null(season = season, round = round, session = "SS")

  laps <- dplyr::bind_rows(fp1, fp2, fp3, s, ss, quali, r)

  return(laps)
}

get_laps_or_null <- function(season, round, session) {
  laps <- tryCatch(
    f1dataR::load_session_laps(
      season = season,
      round = round,
      session = session,
      add_weather = T
    ),
    error = function(e) {
      return(NULL)
    }
  )

  if (!is.null(laps)) {
    if (!('deleted_reason' %in% colnames(laps))) {
      laps$deleted_reason <- NA_character_
    }
    laps <- laps %>%
      dplyr::mutate(
        is_personal_best = as.logical(unlist(.data$is_personal_best)),
        track_status = as.numeric(unlist(.data$track_status)),
        deleted_reason = unlist(.data$deleted_reason),
        air_temp = unlist(.data$air_temp),
        humidity = unlist(.data$humidity),
        pressure = unlist(.data$pressure),
        rainfall = as.logical(unlist(.data$rainfall)),
        track_temp = unlist(.data$track_temp),
        wind_direction = unlist(.data$wind_direction),
        wind_speed = unlist(.data$wind_speed),
        fresh_tyre = unlist(.data$fresh_tyre),
        is_accurate = unlist(.data$is_accurate)
      ) %>%
      janitor::clean_names()
  }

  return(laps)
}

#' Get Quali position, starting grid, and finishing position
#'
#' @param season season
#' @param round round
#' @param session Usually 'R', but also available is 'S'
#'
#' @return data frame
get_grids <- function(season, round, session) {
  if (session == "R") {
    # data frame Position (1-20), QualiResults (Driver), Start Grid (Driver), Final Position (Driver)
    results <- f1dataR::load_results(season = season, round = round)
    quali <- f1dataR::load_quali(season = season, round = round)
  } else if (session == "S") {
    if (season < 2021) {
      return(NULL)
    }
    schedule <- f1predicter::schedule %>%
      dplyr::mutate(
        date = as.Date(.data$date),
        season = as.numeric(.data$season),
        round = as.numeric(.data$round),
        lat = as.numeric(.data$round),
        long = as.numeric(.data$long),
        sprint_date = as.Date(.data$sprint_date)
      ) %>%
      tibble::as_tibble()

    if (
      !is.na(
        schedule[
          schedule$season == season & schedule$round == round,
        ]$sprint_date
      )
    ) {
      results <- f1dataR::load_sprint(season = season, round = round)
    } else {
      results <- NULL
    }

    if (is.null(results)) {
      return(NULL)
    }
    quali <- f1dataR::load_quali(season = season, round = round)
  }

  startgrid <- results %>%
    dplyr::arrange("grid") %>%
    dplyr::pull("driver_id")
  ndrivers <- max(nrow(results), nrow(quali))
  grid <- data.frame(position = 1:max(nrow(results), nrow(quali))) %>%
    dplyr::mutate(
      quali_results = expand_val(quali$driver_id, ndrivers, NA),
      start_grid = expand_val(startgrid, ndrivers, NA),
      race_results = expand_val(results$driver_id, ndrivers, NA)
    ) %>%
    janitor::clean_names()
  return(grid)
}


#' Get weekend's Data
#'
#' @description
#' Downloads and saves all data for a round weekend
#'
#' @param season season from 2018 onward
#' @param round round number
#' @param force whether to force getting new data
#'
#' @export
#'
#' @return a list of data frames
get_weekend_data <- function(season, round, force = FALSE) {
  schedule <- f1predicter::schedule %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      season = as.numeric(.data$season),
      round = as.numeric(.data$round),
      lat = as.numeric(.data$round),
      long = as.numeric(.data$long),
      sprint_date = as.Date(.data$sprint_date)
    ) %>%
    tibble::as_tibble()

  rgrid <- NA
  sgrid <- NA
  results <- NA
  sprint_results <- NA
  pitstops <- NA
  quali <- NA
  laps <- NA

  if (!force) {
    rgrid <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_rgrid.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    sgrid <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_sgrid.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    results <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_results.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    sprint_results <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_sptrint_results.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    pitstops <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_pitstops.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    quali <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(season, "_", round, "_quali.csv")
      )),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    laps <- tryCatch(
      utils::read.csv(
        file.path(
          options("f1predicter.cache"),
          paste0(season, "_", round, "_laps.csv")
        ),
        colClasses = c(
          "character",
          "character",
          "numeric",
          "integer",
          "integer",
          "numeric",
          "numeric",
          "numeric",
          "integer",
          "integer",
          "integer",
          "integer",
          "logical",
          "character",
          "integer",
          "logical",
          "integer",
          "integer",
          "logical",
          "character",
          "logical",
          "numeric",
          "numeric",
          "numeric",
          "logical",
          "numeric",
          "integer",
          "numeric",
          "character",
          "integer",
          "integer"
        )
      ),
      error = function(e) return(NA),
      warning = function(w) return(NA)
    ) %>%
      ensure_tidy()
    if (any(!is.na(c(rgrid, sgrid, results, pitstops, quali, laps)))) {
      cat("Found some old data - won't reload what's already available.\n")
    }
  }

  if (!any(!is.na(results))) {
    results <- f1dataR::load_results(season = season, round = round)
    if (!is.null(results)) {
      if (!("fastest_rank") %in% colnames(results)) {
        # handles 2021.12 (Belgian GP) and possible others.
        results$fastest_rank <- NA
        results$fastest <- NA
        results$top_speed_kph <- NA
        results$time_sec <- NA
      }
      results <- results %>%
        dplyr::mutate(
          points = as.numeric(.data$points),
          position = as.numeric(.data$position),
          grid = as.numeric(.data$grid),
          laps = as.numeric(.data$laps),
          fastest_rank = as.numeric(.data$fastest_rank),
          top_speed_kph = as.numeric(.data$top_speed_kph),
          season = season,
          round = round
        ) %>%
        janitor::clean_names()
      utils::write.csv(
        x = results,
        file = file.path(
          options("f1predicter.cache"),
          paste0(season, "_", round, "_results.csv")
        ),
        quote = F,
        row.names = F
      )
    }
  }

  if (season >= 2021) {
    if (!any(!is.na(sprint_results))) {
      if (
        !is.null(
          schedule[
            schedule$season == season & schedule$round == round,
          ]$sprint_date
        )
      ) {
        sprint_results <- f1dataR::load_sprint(season = season, round = round)
        if (!is.null(sprint_results)) {
          sprint_results <- sprint_results %>%
            dplyr::mutate(
              points = as.numeric(.data$points),
              position = as.numeric(.data$position),
              grid = as.numeric(.data$grid),
              laps = as.numeric(.data$laps),
              lap = as.numeric(.data$lap)
            ) %>%
            janitor::clean_names()
          utils::write.csv(
            x = sprint_results,
            file = file.path(
              options("f1predicter.cache"),
              paste0(season, "_", round, "_sprint_results.csv")
            ),
            quote = F,
            row.names = F
          )
        }
      }
    }
  }

  if (season >= 2011) {
    if (!any(!is.na(pitstops))) {
      pitstops <- f1dataR::load_pitstops(season = season, round = round)
      if (!is.null(pitstops)) {
        if (length(pitstops) == 0) {
          pitstops <- NULL
        } else {
          pitstops <- pitstops %>%
            dplyr::mutate(
              stop = as.numeric(.data$stop),
              lap = as.numeric(.data$lap),
              duration = as.numeric(.data$duration),
              season = season,
              round = round
            ) %>%
            janitor::clean_names()
          utils::write.csv(
            x = pitstops,
            file = file.path(
              options("f1predicter.cache"),
              paste0(season, "_", round, "_pitstops.csv")
            ),
            quote = F,
            row.names = F
          )
        }
      }
    }
  } else {
    pitstops <- NULL
  }

  if (season >= 2003) {
    if (!any(!is.na(rgrid))) {
      rgrid <- get_grids(season = season, round = round, session = "R")
      if (!is.null(rgrid)) {
        rgrid <- rgrid %>%
          dplyr::mutate(season = season, round = round) %>%
          janitor::clean_names()
        utils::write.csv(
          x = rgrid,
          file = file.path(
            options("f1predicter.cache"),
            paste0(season, "_", round, "_rgrid.csv")
          ),
          quote = F,
          row.names = F
        )
      }
    }

    if (!any(!is.na(sgrid))) {
      sgrid <- get_grids(season = season, round = round, session = "S")
      if (!is.null(sgrid)) {
        sgrid <- sgrid %>%
          dplyr::mutate(season = season, round = round) %>%
          janitor::clean_names()
        utils::write.csv(
          x = sgrid,
          file = file.path(
            options("f1predicter.cache"),
            paste0(season, "_", round, "_sgrid.csv")
          ),
          quote = F,
          row.names = F
        )
      }
    }

    if (!any(!is.na(quali))) {
      quali <- f1dataR::load_quali(season = season, round = round)
      if (!is.null(quali)) {
        if (length(quali) == 0) {
          quali <- NULL
        } else {
          quali <- quali %>%
            dplyr::mutate(
              position = as.numeric(.data$position),
              season = season,
              round = round
            ) %>%
            dplyr::mutate(
              q1_sec = expand_val(unlist(.data$q1_sec), dplyr::n())
            ) %>%
            janitor::clean_names()
          utils::write.csv(
            x = quali,
            file = file.path(
              options("f1predicter.cache"),
              paste0(season, "_", round, "_quali.csv")
            ),
            quote = F,
            row.names = F
          )
        }
      }
    }
  } else {
    rgrid <- NULL
    sgrid <- NULL
    quali <- NULL
  }

  if (season >= 2018) {
    if (!any(!is.na(laps))) {
      drivers <- f1dataR::load_drivers(season = season) %>%
        dplyr::select("driver_id", "code")
      laps <- get_laps(season = season, round = round)
      if (!is.null(laps)) {
        laps <- laps %>%
          dplyr::left_join(drivers, by = c(driver = "code")) %>%
          dplyr::left_join(
            results[, c("driver_id", "constructor_id")],
            by = c("driver_id")
          ) %>%
          dplyr::mutate(season = season, round = round) %>%
          dplyr::select(
            "driver_id",
            "constructor_id",
            "lap_time",
            "lap_number",
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
        utils::write.csv(
          x = laps,
          file = file.path(
            options("f1predicter.cache"),
            paste0(season, "_", round, "_laps.csv")
          ),
          quote = F,
          row.names = F
        )
      }
    }
  } else {
    laps = NULL
  }
  closeAllConnections()
  return(list(
    rgrid = rgrid,
    sgrid = sgrid,
    results = results,
    sprint_results = sprint_results,
    pitstops = pitstops,
    laps = laps,
    quali = quali
  ))
}

#' Get the schedule
#'
#' @param save_data whether or not to update the saved schedule object
#'
#' @return Schedule data.frame
#' @export
get_schedule <- function(save_data = FALSE) {
  schedule <- NULL
  for (y in c(1990:f1dataR::get_current_season())) {
    schedule <- dplyr::bind_rows(schedule, f1dataR::load_schedule(season = y))
  }

  if (save_data) {
    usethis::use_data(schedule, overwrite = TRUE)
  }

  invisible(schedule)
}


#' Get Season Data
#'
#' @description
#' Get a whole season's data (up to today's date, if current data). Saves data to
#' options('f1predicter.cache') directory
#'
#' @param season a season, from 2018 onward
#' @param force whether to force reloading data
#'
#' @return None, writes to file
#' @export
get_season_data <- function(season, force = FALSE) {
  stopifnot(season <= f1dataR::get_current_season())

  schedule <- f1predicter::schedule

  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  for (round in schedule[
    schedule$season == season & schedule$date <= Sys.Date(),
  ]$round) {
    cat(
      "Getting data for round",
      round,
      "of",
      length(
        schedule[
          schedule$season == season & schedule$date <= Sys.Date(),
        ]$round
      ),
      "\n"
    )
    r <- get_weekend_data(
      season = season,
      round = as.numeric(round),
      force = force
    )
    rgrid <- dplyr::bind_rows(rgrid, r$rgrid)
    sgrid <- dplyr::bind_rows(sgrid, r$sgrid)
    results <- dplyr::bind_rows(results, r$results)
    sprint_results <- dplyr::bind_rows(sprint_results, r$sprint_results)
    laps <- dplyr::bind_rows(laps, r$laps)
    pitstops <- dplyr::bind_rows(pitstops, r$pitstops)
    qualis <- dplyr::bind_rows(qualis, r$quali)
  }
  rgrid %>%
    janitor::clean_names() %>%
    utils::write.csv(
      file = file.path(
        options("f1predicter.cache"),
        paste0(season, "_season_rgrid.csv")
      ),
      quote = F,
      row.names = F
    )
  if (!is.null(sgrid)) {
    sgrid %>%
      janitor::clean_names() %>%
      utils::write.csv(
        file = file.path(
          options("f1predicter.cache"),
          paste0(season, "_season_sgrid.csv")
        ),
        quote = F,
        row.names = F
      )
  }
  if (season >= 2018) {
    laps %>%
      janitor::clean_names() %>%
      utils::write.csv(
        file = file.path(
          options("f1predicter.cache"),
          paste0(season, "_season_laps.csv")
        ),
        quote = F,
        row.names = F
      )
  }

  if (season >= 2021) {
    sprint_results %>%
      janitor::clean_names() %>%
      utils::write.csv(
        file = file.path(
          options("f1predicter.cache"),
          paste0(season, "_season_sprint_results.csv")
        ),
        quote = F,
        row.names = F
      )
  }

  results %>%
    janitor::clean_names() %>%
    utils::write.csv(
      file = file.path(
        options("f1predicter.cache"),
        paste0(season, "_season_results.csv")
      ),
      quote = F,
      row.names = F
    )

  pitstops %>%
    janitor::clean_names() %>%
    utils::write.csv(
      file = file.path(
        options("f1predicter.cache"),
        paste0(season, "_season_pitstops.csv")
      ),
      quote = F,
      row.names = F
    )

  qualis %>%
    janitor::clean_names() %>%
    utils::write.csv(
      file = file.path(
        options("f1predicter.cache"),
        paste0(season, "_season_qualis.csv")
      ),
      quote = F,
      row.names = F
    )

  closeAllConnections()
  cat("Success\n")
}


#' Load All Data
#'
#' @description
#' Loads all data previously saved to cache at options('f1predicter.cache') location
#'
#' @return a list of data.frames
#' @export
load_all_data <- function() {
  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  for (y in c(1990:f1dataR::get_current_season())) {
    cat("Reading", y, "data...\n")
    rg <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_rgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    rgrid <- dplyr::bind_rows(rgrid, rg)
    sg <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_sgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    sgrid <- dplyr::bind_rows(sgrid, sg)
    res <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_results.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    results <- dplyr::bind_rows(results, res)
    q <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_qualis.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    qualis <- dplyr::bind_rows(qualis, q)
    if (y >= 2011) {
      pt <- tryCatch(
        utils::read.csv(file.path(
          options("f1predicter.cache"),
          paste0(y, "_season_pitstops.csv")
        )),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
      pitstops <- dplyr::bind_rows(pitstops, pt)
    }
    if (y >= 2021) {
      sr <- tryCatch(
        utils::read.csv(file.path(
          options("f1predicter.cache"),
          paste0(y, "_season_sprint_results.csv")
        )),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
      sprint_results <- dplyr::bind_rows(sprint_results, sr)
    }
    if (y >= 2018) {
      lp <- tryCatch(
        utils::read.csv(
          file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_laps.csv")
          ),
          colClasses = c(
            "character",
            "character",
            "numeric",
            "integer",
            "integer",
            "numeric",
            "numeric",
            "numeric",
            "integer",
            "integer",
            "integer",
            "integer",
            "logical",
            "character",
            "integer",
            "logical",
            "integer",
            "integer",
            "logical",
            "character",
            "logical",
            "numeric",
            "numeric",
            "numeric",
            "logical",
            "numeric",
            "integer",
            "numeric",
            "character",
            "integer",
            "integer"
          )
        ),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
      laps <- dplyr::bind_rows(laps, lp)
    }
    closeAllConnections()
  }

  return(list(
    results = results,
    sprint_results = sprint_results,
    laps = laps,
    pitstops = pitstops,
    sgrid = sgrid,
    rgrid = rgrid,
    qualis = qualis
  ))
}

get_last_drivers <- function() {
  res <- tryCatch(
    utils::read.csv(file.path(
      options("f1predicter.cache"),
      paste0(f1dataR::get_current_season(), "_season_results.csv")
    )),
    error = function(e) return(NULL)
  ) %>%
    ensure_tidy()

  if (is.null(res)) {
    stop("f1predictor::get_last_drivers: Error loading last drivers.")
  }

  res <- utils::tail(res, 30)
  rnd <- max(res$round, na.rm = T)
  drivers <- res[res$round == rnd, c('driver_id', 'constructor_id')]
}


#' getWeather
#'
#' @description given a round url from ergast (to wikipedia) get the weather
#'
#' @param round_url a wikipedia url for a grand prix (in english)
#'
#' @return a weather, as character (one of `warm`, `cold`, `dry`, `wet`, or `cloudy`)
getWeather <- function(round_url) {
  stopifnot(grepl("wikipedia", round_url))

  round_weather <- NULL

  tryCatch(
    {
      round_tables <- rvest::html_table(rvest::read_html(round_url))
      for (table in round_tables) {
        if (
          "Weather" %in%
            (table[, 1] %>%
              dplyr::pull(1))
        ) {
          rw <- which(
            (table[, 1] %>%
              dplyr::pull(1)) ==
              "Weather"
          )
          round_weather <- table[rw, 2] %>%
            dplyr::pull(1)
          break
        }
      }
    },
    error = function(e) {
      message(glue::glue(
        "Error in f1predictor::getWeather: {url} - {err}",
        url = round_url,
        err = e
      ))
    }
  )

  if (is.null(round_weather)) {
    # Try the italian site - it's apparently more robust
    message(glue::glue(
      "f1predictor::getWeather: Trying to get weather from Italian Wikipedia instead of {url}",
      url = round_url
    ))
    it_url <- grep(
      pattern = "https:\\/\\/it.wikipedia.org\\/wiki\\/[a-zA-Z0-9_%]*",
      x = RCurl::getURLContent(round_url, .opts = list(ssl.verifypeer = FALSE)),
      value = TRUE
    )

    tryCatch(
      {
        it_round_table <- rvest::html_table(rvest::read_html(round_url))
        for (table in round_tables) {
          if (
            "Clima" %in%
              (table[, 1] %>%
                dplyr::pull(1))
          ) {
            rw <- which(
              (table[, 1] %>%
                dplyr::pull(1)) ==
                "Clima"
            )
            round_weather <- table[rw, 2] %>%
              dplyr::pull(1)
            break
          }
        }
      },
      error = function(e) {
        message(glue::glue(
          "Error in f1predictor::getWeather: {url} - {err}",
          url = round_url,
          err = e
        ))
      }
    )
  }

  # Bin weather to few option or show the value and mark unknown
  if (is.null(round_weather)) {
    message("f1predictor::getWeather: round Weather not found")
    round_weather <- "unknown"
  } else if (
    grepl(
      "showers|wet|rain|pioggia|damp|thunderstorms|rainy",
      round_weather,
      ignore.case = T
    )
  ) {
    round_weather <- "wet"
  } else if (
    grepl(
      "cloudy|grey|coperto|clouds|nuvoloso|overcast",
      round_weather,
      ignore.case = T
    )
  ) {
    round_weather <- "cloudy"
  } else if (grepl("dry|asciutto", round_weather, ignore.case = T)) {
    round_weather <- "dry"
  } else if (grepl("cold|fresh|chilly|cool", round_weather, ignore.case = T)) {
    round_weather <- "cold"
  } else if (
    grepl(
      "soleggiato|clear|warm|hot|sunny|fine|mild|sereno",
      round_weather,
      ignore.case = T
    )
  ) {
    round_weather <- "warm"
  } else {
    message(glue::glue(
      "f1predictor::getWeather: round Weather of {round_weather} is unknown type. From {url}",
      round_weather = round_weather,
      url = round_url
    ))
    round_weather <- "unknown"
  }

  return(round_weather)
}


#' Schedule Data
#'
#' This contains the schedule data. Can regenerate by calling `get_schedule()`
#'
#' @name schedule
#' @docType data
#' @references derived from ergast data
#' @keywords data
NULL

janitor_data <- function() {
  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  for (y in c(1990:f1dataR::get_current_season())) {
    cat("Reading", y, "data...\n")
    rg <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_rgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    sg <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_sgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    res <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_results.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    q <- tryCatch(
      utils::read.csv(file.path(
        options("f1predicter.cache"),
        paste0(y, "_season_qualis.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    if (y >= 2011) {
      pt <- tryCatch(
        utils::read.csv(file.path(
          options("f1predicter.cache"),
          paste0(y, "_season_pitstops.csv")
        )),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
    }
    if (y >= 2018) {
      lp <- tryCatch(
        utils::read.csv(
          file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_laps.csv")
          ),
          colClasses = c(
            "character",
            "character",
            "numeric",
            "integer",
            "integer"
          )
        ),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
    }
    if (y >= 2021) {
      sr <- tryCatch(
        utils::read.csv(
          file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_sprint_results.csv")
          ),
          colClasses = c(
            "character",
            "character",
            "numeric",
            "integer",
            "integer"
          )
        ),
        error = function(e) return(NULL)
      ) %>%
        ensure_tidy()
    }
    closeAllConnections()
    if (!is.null(rg)) {
      rg %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_rgrid.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(sg)) {
      sg %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_sgrid.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(res)) {
      res %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_results.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(q)) {
      q %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_qualis.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(pt)) {
      pt %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_pitstops.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(lp)) {
      lp %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_laps.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    if (!is.null(sr)) {
      sr %>%
        janitor::clean_names() %>%
        utils::write.csv(
          file = file.path(
            options("f1predicter.cache"),
            paste0(y, "_season_sprint_results.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    closeAllConnections()
  }
}
