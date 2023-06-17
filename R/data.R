# Data

# Functions to build data files

#' Get Weekend's laps
#'
#' @description
#' Get all laps (timing) from a weekend and return as one data.frame
#'
#' @param season season, from 2018 onward
#' @param race race, compatable with fastf1 race selector
#'
#' @return a data.frame with all lap information
get_laps <- function(season, race) {

  r <- get_laps_or_null(season = season, race = race, session = "R")
  quali <- get_laps_or_null(season = season, race = race, session = "Q")
  fp1 <- get_laps_or_null(season = season, race = race, session = "FP1")
  fp2 <- get_laps_or_null(season = season, race = race, session = "FP2")
  fp3 <- get_laps_or_null(season = season, race = race, session = "FP3")
  s <- get_laps_or_null(season = season, race = race, session = "S")
  ss <- get_laps_or_null(season = season, race = race, session = "SS")

  laps <- dplyr::bind_rows(fp1, fp2, fp3, s, ss, quali, r)

  return(laps)
}

get_laps_or_null <- function(season, race, session) {
  laps <- tryCatch(f1dataR::load_session_laps(season = season, race = race, session = session, add_weather = T), error = function(e) {
    return(NULL)
  })

  if (!is.null(laps)) {
    laps <- laps %>%
      dplyr::mutate(IsPersonalBest = as.logical(unlist(.data$IsPersonalBest)), TrackStatus = as.numeric(unlist(.data$TrackStatus)), DeletedReason = unlist(.data$DeletedReason),
        AirTemp = unlist(.data$AirTemp), Humidity = unlist(.data$Humidity), Pressure = unlist(.data$Pressure), Rainfall = as.logical(unlist(.data$Rainfall)),
        TrackTemp = unlist(.data$TrackTemp), WindDirection = unlist(.data$WindDirection), WindSpeed = unlist(.data$WindSpeed))
  }

  return(laps)
}

#' Get Quali position, starting grid, and finishing position
#'
#' @param season season
#' @param race race
#' @param session Usually 'R', but also available is 'S'
#'
#' @return data frame
get_grids <- function(season, race, session) {
  if (session == "R") {
    # data frame Position (1-20), QualiResults (Driver), Start Grid (Driver), Final Position (Driver)
    results <- f1dataR::load_results(season = season, round = race)
    quali <- f1dataR::load_quali(season = season, round = race)


  } else if (session == "S") {
    if (season < 2021) {
      return(NULL)
    }
    results <- f1dataR::load_sprint(season = season, round = race)
    if (is.null(results)) {
      return(NULL)
    }
    quali <- f1dataR::load_quali(season = season, round = race)
  }

  startgrid <- results %>%
    dplyr::arrange("grid") %>%
    dplyr::pull("driverId")
  ndrivers <- max(nrow(results), nrow(quali))
  grid <- data.frame(position = 1:max(nrow(results), nrow(quali))) %>%
    dplyr::mutate(qualiResults = expand_val(quali$driverId, ndrivers, NA), startGrid = expand_val(startgrid, ndrivers, NA), raceResults = expand_val(results$driverId,
      ndrivers, NA))
  return(grid)
}



#' Get weekend's Data
#'
#' @description
#' Downloads and saves all data for a race weekend
#'
#' @param season season from 2018 onward
#' @param race race number
#' @param force whether to force getting new data
#'
#' @return a list of data frames
get_weekend_data <- function(season, race, force = FALSE) {
  rgrid <- NA
  sgrid <- NA
  results <- NA
  pitstops <- NA
  quali <- NA
  laps <- NA

  if (!force) {
    rgrid <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_rgrid.csv"))), error = function(e) return(NA), warning = function(w) return(NA))
    sgrid <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_sgrid.csv"))), error = function(e) return(NA), warning = function(w) return(NA))
    results <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_results.csv"))), error = function(e) return(NA),
      warning = function(w) return(NA))
    pitstops <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_pitstops.csv"))), error = function(e) return(NA),
      warning = function(w) return(NA))
    quali <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_quali.csv"))), error = function(e) return(NA), warning = function(w) return(NA))
    laps <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(season, "_", race, "_laps.csv")), colClasses = c("character", "character",
      "numeric", "integer", "integer", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "logical", "character", "integer", "logical",
      "integer", "integer", "logical", "character", "logical", "numeric", "numeric", "numeric", "logical", "numeric", "integer", "numeric", "character", "integer",
      "integer")), error = function(e) return(NA), warning = function(w) return(NA))
    if (any(!is.na(c(rgrid, sgrid, results, pitstops, quali, laps)))) {
      cat("Found some old data - won't reload what's already available.\n")
    }
  }

  if (!any(!is.na(results))) {
    results <- f1dataR::load_results(season = season, round = race)
    if (!is.null(results)) {
      if (!("fastest_rank") %in% colnames(results)) {
        # handles 2021.12 (Belgian GP) and possible others.
        results$fastest_rank <- NA
        results$fastest <- NA
        results$top_speed_kph <- NA
        results$time_sec <- NA
      }
      results <- results %>%
        dplyr::mutate(points = as.numeric(.data$points), position = as.numeric(.data$position), grid = as.numeric(.data$grid), laps = as.numeric(.data$laps),
          fastest_rank = as.numeric(.data$fastest_rank), top_speed_kph = as.numeric(.data$top_speed_kph), season = season, race = race)
      utils::write.csv(x = results, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_results.csv")), quote = F, row.names = F)
    }
    Sys.sleep(1)
  }

  if (season >= 2011) {
    if (!any(!is.na(pitstops))) {
      pitstops <- f1dataR::load_pitstops(season = season, race = race)
      if (!is.null(pitstops)) {
        if (length(pitstops) == 0) {
          pitstops <- NULL
        } else {
          pitstops <- pitstops %>%
          dplyr::mutate(stop = as.numeric(.data$stop), lap = as.numeric(.data$lap), duration = as.numeric(.data$duration), season = season, race = race)
          utils::write.csv(x = pitstops, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_pitstops.csv")), quote = F, row.names = F)
        }
      }
      Sys.sleep(1)
    }
  } else {
    pitstops <- NULL
  }

  if (season >= 2003) {
    if (!any(!is.na(rgrid))) {
      rgrid <- get_grids(season = season, race = race, session = "R")
      if (!is.null(rgrid)) {
        rgrid <- rgrid %>%
          dplyr::mutate(season = season, race = race)
        utils::write.csv(x = rgrid, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_rgrid.csv")), quote = F, row.names = F)
      }
      Sys.sleep(1)
    }

    if (!any(!is.na(sgrid))) {
      sgrid <- get_grids(season = season, race = race, session = "S")
      if (!is.null(sgrid)) {
        sgrid <- sgrid %>%
          dplyr::mutate(season = season, race = race)
        utils::write.csv(x = sgrid, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_sgrid.csv")), quote = F, row.names = F)
      }
      Sys.sleep(1)
    }

    if (!any(!is.na(quali))) {
      quali <- f1dataR::load_quali(season = season, round = race)
      if (!is.null(quali)) {
        if (length(quali) == 0) {
          quali <- NULL
        } else {
          quali <- quali %>%
          dplyr::mutate(position = as.numeric(.data$position), season = season, race = race)
          utils::write.csv(x = quali, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_quali.csv")), quote = F, row.names = F)

        }
      }
      Sys.sleep(1)
    }
  } else {
    rgrid <- NULL
    sgrid <- NULL
    quali <- NULL
  }


  if (season >= 2018) {
    if (!any(!is.na(laps))) {
      drivers <- f1dataR::load_drivers(season = season) %>%
        dplyr::select("driverId", "code")
      laps <- get_laps(season = season, race = race)
      if (!is.null(laps)) {
        laps <- laps %>%
          dplyr::left_join(drivers, by = c(Driver = "code")) %>%
          dplyr::left_join(results[, c("driverId", "constructorId")], by = c("driverId")) %>%
          dplyr::mutate(season = season, race = race) %>%
          dplyr::select("driverId", "constructorId", lapTime = "LapTime", lapNumber = "LapNumber", stint = "Stint", sector1Time = "Sector1Time", sector2Time = "Sector2Time",
          sector3Time = "Sector3Time", speedI1 = "SpeedI1", speedI2 = "SpeedI2", speedFL = "SpeedFL", speedST = "SpeedST", isPersonalBest = "IsPersonalBest",
          compound = "Compound", tireLife = "TyreLife", freshTire = "FreshTyre", trackStatus = "TrackStatus", position = "Position", deleted = "Deleted",
          deletedReason = "DeletedReason", isAccurate = "IsAccurate", airTemp = "AirTemp", humidity = "Humidity", pressure = "Pressure", rainfall = "Rainfall",
          trackTemp = "TrackTemp", windDirection = "WindDirection", windSpeed = "WindSpeed", sessionType = "SessionType", "season", "race")
        utils::write.csv(x = laps, file = file.path(options("f1predicter.cache"), paste0(season, "_", race, "_laps.csv")), quote = F, row.names = F)

      }
    }
  } else {
    laps = NULL
  }
  closeAllConnections()
  return(list(rgrid = rgrid, sgrid = sgrid, results = results, pitstops = pitstops, laps = laps, quali = quali))
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
  schedule %>%
    dplyr::mutate(date = as.Date(.data$date))

  if (save_data) {
    usethis::use_data(schedule, overwrite = T)
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
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  for (race in schedule[schedule$season == season & schedule$date <= Sys.Date() + 3, ]$round) {
    cat("Getting data for race", race, "of", length(schedule[schedule$season == season & schedule$date <= Sys.Date(), ]$round), "\n")
    r <- get_weekend_data(season = season, race = as.numeric(race), force = force)
    rgrid <- dplyr::bind_rows(rgrid, r$rgrid)
    sgrid <- dplyr::bind_rows(sgrid, r$sgrid)
    results <- dplyr::bind_rows(results, r$results)
    laps <- dplyr::bind_rows(laps, r$laps)
    pitstops <- dplyr::bind_rows(pitstops, r$pitstops)
    qualis <- dplyr::bind_rows(qualis, r$quali)
  }
  utils::write.csv(x = rgrid, file = file.path(options("f1predicter.cache"), paste0(season, "_season_rgrid.csv")), quote = F, row.names = F)
  if (!is.null(sgrid)) {
    utils::write.csv(x = sgrid, file = file.path(options("f1predicter.cache"), paste0(season, "_season_sgrid.csv")), quote = F, row.names = F)
  }
  if (season >= 2018) {
    utils::write.csv(x = laps, file = file.path(options("f1predicter.cache"), paste0(season, "_season_laps.csv")), quote = F, row.names = F)
  }
  utils::write.csv(x = results, file = file.path(options("f1predicter.cache"), paste0(season, "_season_results.csv")), quote = F, row.names = F)
  utils::write.csv(x = pitstops, file = file.path(options("f1predicter.cache"), paste0(season, "_season_pitstops.csv")), quote = F, row.names = F)
  utils::write.csv(x = qualis, file = file.path(options("f1predicter.cache"), paste0(season, "_season_qualis.csv")), quote = F, row.names = F)
  closeAllConnections()
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
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  for (y in c(1990:f1dataR::get_current_season())) {
    cat("Reading", y, "data...\n")
    rg <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_rgrid.csv"))), error = function(e) return(NULL))
    rgrid <- dplyr::bind_rows(rgrid, rg)
    sg <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_sgrid.csv"))), error = function(e) return(NULL))
    sgrid <- dplyr::bind_rows(sgrid, sg)
    res <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_results.csv"))), error = function(e) return(NULL))
    results <- dplyr::bind_rows(results, res)

    q <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_qualis.csv"))), error = function(e) return(NULL))
    qualis <- dplyr::bind_rows(qualis, q)
    if (y >= 2011) {
      pt <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_pitstops.csv"))), error = function(e) return(NULL))
      pitstops <- dplyr::bind_rows(pitstops, pt)
    }
    if (y >= 2018) {
      lp <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(y, "_season_laps.csv")), colClasses = c("character", "character", "numeric",
        "integer", "integer", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "logical", "character", "integer", "logical",
        "integer", "integer", "logical", "character", "logical", "numeric", "numeric", "numeric", "logical", "numeric", "integer", "numeric", "character",
        "integer", "integer")), error = function(e) return(NULL))
      laps <- dplyr::bind_rows(laps, lp)
    }
    closeAllConnections()
  }

  return(list(results = results, laps = laps, pitstops = pitstops, sgrid = sgrid, rgrid = rgrid, qualis = qualis))
}

get_last_drivers <- function() {
  res <- tryCatch(utils::read.csv(file.path(options("f1predicter.cache"), paste0(f1dataR::get_current_season(), "_season_results.csv"))), error = function(e) return(NULL))

  if (is.null(res)) {
    stop("f1predictor::get_last_drivers: Error loading last drivers.")
  }


  res <- utils::tail(res, 30)
  rnd <- max(res$round, na.rm = T)
  drivers <- res[res$round == rnd, ]$driverId
}



#' getRaceWeather
#'
#' @description given a race url from ergast (to wikipedia) get the weather
#'
#' @param race_url a wikipedia url for a grand prix (in english)
#'
#' @return a weather, as character (one of `warm`, `cold`, `dry`, `wet`, or `cloudy`)
getWeather <- function(race_url) {
  stopifnot(grepl("wikipedia", race_url))

  race_weather <- NULL

  tryCatch({
    race_tables <- rvest::html_table(rvest::read_html(race_url))
    for (table in race_tables) {
      if ("Weather" %in% (table[, 1] %>%
        dplyr::pull(1))) {
        rw <- which((table[, 1] %>%
          dplyr::pull(1)) == "Weather")
        race_weather <- table[rw, 2] %>%
          dplyr::pull(1)
        break
      }
    }
  }, error = function(e) {
    message(glue::glue("Error in f1predictor::getWeather: {url} - {err}", url = race_url, err = e))
  })

  if (is.null(race_weather)) {
    # Try the italian site - it's apparently more robust
    logger::log_info(glue::glue("f1predictor::getWeather: Trying to get weather from Italian Wikipedia instead of {url}", url = race_url))
    it_url <- grep(pattern = "https:\\/\\/it.wikipedia.org\\/wiki\\/[a-zA-Z0-9_%]*", x = RCurl::getURLContent(race_url, .opts = list(ssl.verifypeer = FALSE)),
      value = TRUE)

    tryCatch({
      it_race_table <- rvest::html_table(rvest::read_html(race_url))
      for (table in race_tables) {
        if ("Clima" %in% (table[, 1] %>%
          dplyr::pull(1))) {
          rw <- which((table[, 1] %>%
          dplyr::pull(1)) == "Clima")
          race_weather <- table[rw, 2] %>%
          dplyr::pull(1)
          break
        }
      }
    }, error = function(e) {
      message(glue::glue("Error in f1predictor::getWeather: {url} - {err}", url = race_url, err = e))
    })
  }

  # Bin weather to few option or show the value and mark unknown
  if (is.null(race_weather)) {
    logger::log_info("f1predictor::getWeather: Race Weather not found")
    race_weather <- "unknown"
  } else if (grepl("showers|wet|rain|pioggia|damp|thunderstorms|rainy", race_weather, ignore.case = T)) {
    race_weather <- "wet"
  } else if (grepl("cloudy|grey|coperto|clouds|nuvoloso|overcast", race_weather, ignore.case = T)) {
    race_weather <- "cloudy"
  } else if (grepl("dry|asciutto", race_weather, ignore.case = T)) {
    race_weather <- "dry"
  } else if (grepl("cold|fresh|chilly|cool", race_weather, ignore.case = T)) {
    race_weather <- "cold"
  } else if (grepl("soleggiato|clear|warm|hot|sunny|fine|mild|sereno", race_weather, ignore.case = T)) {
    race_weather <- "warm"
  } else {
    logger::log_info(glue::glue("f1predictor::getWeather: Race Weather of {race_weather} is unknown type. From {url}", race_weather = race_weather, url = race_url))
    race_weather <- "unknown"
  }

  return(race_weather)
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
