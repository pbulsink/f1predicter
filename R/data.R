# Data

# Functions to build data files

# ---- Internal cache I/O helpers ----

# Save a data frame to an RDS file in the cache directory.
# Returns the data invisibly so it can be used in pipelines.
cache_to_rds <- function(data, path) {
  saveRDS(data, path)
  invisible(data)
}

# Load a data frame from an RDS file, falling back to CSV for backward
# compatibility with existing cache files written by earlier versions of the
# package.  Returns NULL (not NA) when neither file is found.
# Corrupt/unreadable RDS files emit a warning so the problem is visible.
load_rds_or_csv <- function(rds_path, csv_path = NULL, col_classes = NULL) {
  if (file.exists(rds_path)) {
    return(tryCatch(
      readRDS(rds_path),
      error = function(e) {
        cli::cli_warn(
          "Failed to read {.file {rds_path}}: {conditionMessage(e)}"
        )
        NULL
      }
    ))
  }
  if (!is.null(csv_path) && file.exists(csv_path)) {
    args <- list(file = csv_path)
    if (!is.null(col_classes)) {
      args$colClasses <- col_classes
    }
    return(tryCatch(
      suppressWarnings(do.call(utils::read.csv, args)),
      error = function(e) NULL
    ))
  }
  NULL
}

#' Get Weekend's laps
#'
#' @description
#' Get all laps (timing) from a weekend and return as one data.frame
#'
#' @param season season, from 2018 onward
#' @param round round, compatible with fastf1 round selector
#'
#' @return a data.frame with all lap information
#' @noRd
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

#' Safely Load and Clean Session Laps
#'
#' @description
#' An internal helper that attempts to load lap data for a specific session using
#' `f1dataR::load_session_laps`. It wraps the call in a `tryCatch` block to
#' gracefully handle errors (e.g., when a session does not exist) by returning
#' `NULL`. If data is loaded successfully, it performs basic cleaning, such as
#' ensuring specific columns exist and unlisting list-columns returned by the API.
#'
#' @param season The numeric championship season.
#' @param round The round number of the event.
#' @param session The session identifier (e.g., "R", "Q", "FP1").
#'
#' @return A cleaned data frame of lap data if successful, otherwise `NULL`.
#' @noRd
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
    } else {
      laps$deleted_reason <- as.character(laps$deleted_reason)
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
#' @noRd
get_grids <- function(season, round, session) {
  if (session == "R") {
    # data frame Position (starting from 1), QualiResults (Driver), Start Grid (Driver), Final Position (Driver)
    results <- NULL
    try(
      results <- f1dataR::load_results(season = season, round = round) %>%
        dplyr::mutate(
          'position' = as.numeric(.data$position),
          'grid' = as.numeric(.data$grid)
        )
    )
    quali <- NULL
    try(
      quali <- f1dataR::load_quali(season = season, round = round)
    )
    if (is.null(results) && is.null(quali)) {
      return(NULL)
    }
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

  if (is.null(results) & !is.null(quali)) {
    # this is the situation that quali is done but not the race
    grid <- quali %>%
      dplyr::select(.data$position, .data$driver_id) %>%
      dplyr::rename("quali_results" = "driver_id") %>%
      dplyr::mutate(
        'start_grid' = NA_character_,
        'race_results' = NA_character_
      ) %>%
      janitor::clean_names()
  } else {
    startgrid <- results %>%
      dplyr::arrange(.data$grid) %>%
      dplyr::pull("driver_id")
    ndrivers <- max(nrow(results), nrow(quali))
    grid <- data.frame(position = 1:max(nrow(results), nrow(quali))) %>%
      dplyr::mutate(
        quali_results = expand_val(quali$driver_id, ndrivers, NA),
        start_grid = expand_val(startgrid, ndrivers, NA),
        race_results = expand_val(results$driver_id, ndrivers, NA)
      ) %>%
      janitor::clean_names()
  }
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
#' @examples
#' \dontrun{
#' data <- get_weekend_data(season = 2024, round = 1)
#' }
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
    cache <- getOption("f1predicter.cache")
    pfx <- paste0(season, "_", round, "_")

    rgrid <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "rgrid.rds")),
      file.path(cache, paste0(pfx, "rgrid.csv"))
    ) %>%
      ensure_tidy()
    sgrid <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "sgrid.rds")),
      file.path(cache, paste0(pfx, "sgrid.csv"))
    ) %>%
      ensure_tidy()
    results <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "results.rds")),
      file.path(cache, paste0(pfx, "results.csv"))
    ) %>%
      ensure_tidy()
    sprint_results <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "sprint_results.rds")),
      # note: old files used the typo "sptrint_results"
      file.path(cache, paste0(pfx, "sptrint_results.csv"))
    ) %>%
      ensure_tidy()
    pitstops <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "pitstops.rds")),
      file.path(cache, paste0(pfx, "pitstops.csv"))
    ) %>%
      ensure_tidy()
    quali <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "quali.rds")),
      file.path(cache, paste0(pfx, "quali.csv"))
    ) %>%
      ensure_tidy()
    laps <- load_rds_or_csv(
      file.path(cache, paste0(pfx, "laps.rds")),
      file.path(cache, paste0(pfx, "laps.csv"))
    ) %>%
      ensure_tidy()

    # Convert NULLs from the loader back to NA so downstream !is.na() checks work
    if (is.null(rgrid)) {
      rgrid <- NA
    }
    if (is.null(sgrid)) {
      sgrid <- NA
    }
    if (is.null(results)) {
      results <- NA
    }
    if (is.null(sprint_results)) {
      sprint_results <- NA
    }
    if (is.null(pitstops)) {
      pitstops <- NA
    }
    if (is.null(quali)) {
      quali <- NA
    }
    if (is.null(laps)) {
      laps <- NA
    }

    if (any(!is.na(c(rgrid, sgrid, results, pitstops, quali, laps)))) {
      cat("Found some old data - won't reload what's already available.\n")
    }
  }

  if (!any(!is.na(results))) {
    results <- NULL
    try(
      results <- f1dataR::load_results(season = season, round = round)
    )
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
      cache_to_rds(
        results,
        file.path(
          getOption("f1predicter.cache"),
          paste0(season, "_", round, "_results.rds")
        )
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
          cache_to_rds(
            sprint_results,
            file.path(
              getOption("f1predicter.cache"),
              paste0(season, "_", round, "_sprint_results.rds")
            )
          )
        }
      }
    }
  } else {
    sprint_results <- NULL
  }

  if (season >= 2011) {
    if (!any(!is.na(pitstops))) {
      pitstops <- try(f1dataR::load_pitstops(season = season, round = round))
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
          cache_to_rds(
            pitstops,
            file.path(
              getOption("f1predicter.cache"),
              paste0(season, "_", round, "_pitstops.rds")
            )
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
          dplyr::mutate(position = as.integer(.data$position)) %>%
          janitor::clean_names()
        cache_to_rds(
          rgrid,
          file.path(
            getOption("f1predicter.cache"),
            paste0(season, "_", round, "_rgrid.rds")
          )
        )
      }
    }

    if (!any(!is.na(sgrid))) {
      sgrid <- get_grids(season = season, round = round, session = "S")
      if (!is.null(sgrid)) {
        sgrid <- sgrid %>%
          dplyr::mutate(season = season, round = round) %>%
          janitor::clean_names()
        cache_to_rds(
          sgrid,
          file.path(
            getOption("f1predicter.cache"),
            paste0(season, "_", round, "_sgrid.rds")
          )
        )
      }
    }

    if (!any(!is.na(quali))) {
      try(quali <- f1dataR::load_quali(season = season, round = round))
      if (!is.null(quali)) {
        if (all(is.na(quali)) | length(quali) == 0) {
          quali <- NULL
        } else {
          quali <- quali %>%
            dplyr::mutate(
              position = as.numeric(.data$position),
              season = season,
              round = round
            ) %>%
            dplyr::mutate(
              q1_sec = expand_val(unlist(.data$q1_sec), dplyr::n()),
              q2_sec = expand_val(unlist(.data$q2_sec), dplyr::n()),
              q3_sec = expand_val(unlist(.data$q3_sec), dplyr::n())
            ) %>%
            janitor::clean_names()
          cache_to_rds(
            quali,
            file.path(
              getOption("f1predicter.cache"),
              paste0(season, "_", round, "_quali.rds")
            )
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
      laps <- get_laps(season = season, round = round)
      laps <- add_drivers_to_laps(laps, season = season)
      xresults <- FALSE
      if (is.null(results)) {
        xresults <- TRUE
        #This allows driver/constructor matching regardless on if there's a results object
        if (round > 1) {
          results <- f1dataR::load_results(season = season, round = round - 1)
        } else {
          results <- f1dataR::load_results(season = season - 1)
        }
      }
      if (!is.null(laps) && nrow(laps) > 0) {
        laps <- laps %>%
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
        if (xresults) {
          results <- NULL
        }
        cache_to_rds(
          laps,
          file.path(
            getOption("f1predicter.cache"),
            paste0(season, "_", round, "_laps.rds")
          )
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
#' @description
#' Loads schedule data from 1990 through to the current season.
#'
#'
#' @param save_data whether or not to update the saved schedule object
#'
#' @return Schedule data.frame
#' @export
#' @examples
#' \dontrun{
#' sched <- get_schedule()
#' }
get_schedule <- function(save_data = FALSE) {
  schedule <- NULL
  for (y in c(1990:f1dataR::get_current_season())) {
    schedule <- dplyr::bind_rows(schedule, f1dataR::load_schedule(season = y))
  }

  # Sometimes cancelled races exit in schedule -- this is not useful for predictions
  schedule <- schedule[
    stats::complete.cases(schedule[, c('season', 'round')]),
  ]

  # Cast to numeric when useful
  schedule$season <- as.numeric(schedule$season)
  schedule$round <- as.numeric(schedule$round)

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
#' @examples
#' \dontrun{
#' get_season_data(season = 2024)
#' }
get_season_data <- function(
  season = f1dataR::get_current_season(),
  force = FALSE
) {
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
      "Getting data for",
      season,
      "round",
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
    if (!is.null(r$rgrid)) {
      rgrid <- dplyr::bind_rows(rgrid, r$rgrid)
    }
    if (!is.null(r$sgrid)) {
      sgrid <- dplyr::bind_rows(sgrid, r$sgrid)
    }
    if (!is.null(r$results)) {
      results <- dplyr::bind_rows(results, r$results)
    }
    if (!is.null(r$sprint_results)) {
      sprint_results <- dplyr::bind_rows(sprint_results, r$sprint_results)
    }
    if (!is.null(r$laps)) {
      laps <- dplyr::bind_rows(laps, r$laps)
    }
    if (!is.null(r$pitstops)) {
      pitstops <- dplyr::bind_rows(pitstops, r$pitstops)
    }
    if (!is.null(r$quali)) {
      qualis <- dplyr::bind_rows(qualis, r$quali)
    }
  }

  if (!is.null(rgrid)) {
    cache_to_rds(
      janitor::clean_names(rgrid),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_rgrid.rds")
      )
    )
  }

  if (!is.null(sgrid)) {
    cache_to_rds(
      janitor::clean_names(sgrid),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_sgrid.rds")
      )
    )
  }

  if (!is.null(laps)) {
    cache_to_rds(
      janitor::clean_names(laps),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_laps.rds")
      )
    )
  }

  if (!is.null(sprint_results)) {
    cache_to_rds(
      janitor::clean_names(sprint_results),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_sprint_results.rds")
      )
    )
  }

  if (!is.null(results)) {
    cache_to_rds(
      janitor::clean_names(results),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_results.rds")
      )
    )
  }

  if (!is.null(pitstops)) {
    cache_to_rds(
      janitor::clean_names(pitstops),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_pitstops.rds")
      )
    )
  }

  if (!is.null(qualis)) {
    cache_to_rds(
      janitor::clean_names(qualis),
      file.path(
        getOption("f1predicter.cache"),
        paste0(season, "_season_qualis.rds")
      )
    )
  }

  closeAllConnections()
  cat("Success\n")
}


#' Migrate CSV Cache Files to RDS Format
#'
#' @description
#' One-time maintenance utility that converts all season-level CSV cache files
#' (e.g. `"2022_season_results.csv"`) to the more efficient RDS format used by
#' current versions of the package.  After migration the original CSV files are
#' left in place so that older versions of the package remain functional.
#'
#' Only files that do **not** already have a corresponding `.rds` counterpart are
#' converted, making the function safe to call multiple times.
#'
#' @param cache Path to the cache directory.  Defaults to
#'   `getOption("f1predicter.cache")`.
#' @param years Integer vector of season years to migrate.  Defaults to
#'   `1990:f1dataR::get_current_season()`.
#'
#' @return Invisibly returns a character vector of the RDS paths that were
#'   written during this call.
#' @noRd
migrate_cache_to_rds <- function(
  cache = getOption("f1predicter.cache"),
  years = 1990:f1dataR::get_current_season()
) {
  written <- character(0)

  types <- c(
    "rgrid",
    "sgrid",
    "results",
    "qualis",
    "pitstops",
    "sprint_results",
    "laps"
  )

  for (y in years) {
    for (type in types) {
      csv_path <- file.path(cache, paste0(y, "_season_", type, ".csv"))
      rds_path <- file.path(cache, paste0(y, "_season_", type, ".rds"))

      if (file.exists(csv_path) && !file.exists(rds_path)) {
        dat <- tryCatch(
          suppressWarnings(utils::read.csv(csv_path)),
          error = function(e) NULL
        )
        if (!is.null(dat) && nrow(dat) > 0) {
          cache_to_rds(janitor::clean_names(dat), rds_path)
          written <- c(written, rds_path)
          cli::cli_inform("Migrated {.file {csv_path}} -> {.file {rds_path}}")
        }
      }
    }
  }

  cli::cli_inform("Migration complete: {length(written)} file{?s} written.")
  invisible(written)
}


#' Load All Data
#'
#' @description
#' Loads all data previously saved to cache at options('f1predicter.cache') location
#'
#' @return a list of data.frames
#' @export
#' @examples
#' \dontrun{
#' all_data <- load_all_data()
#' }
load_all_data <- function() {
  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

  cache <- getOption("f1predicter.cache")

  for (y in c(1990:f1dataR::get_current_season())) {
    cat("Reading", y, "data...\n")

    rg <- load_rds_or_csv(
      file.path(cache, paste0(y, "_season_rgrid.rds")),
      file.path(cache, paste0(y, "_season_rgrid.csv"))
    ) %>%
      ensure_tidy()
    rgrid <- dplyr::bind_rows(rgrid, rg)

    sg <- load_rds_or_csv(
      file.path(cache, paste0(y, "_season_sgrid.rds")),
      file.path(cache, paste0(y, "_season_sgrid.csv"))
    ) %>%
      ensure_tidy()
    sgrid <- dplyr::bind_rows(sgrid, sg)

    res <- load_rds_or_csv(
      file.path(cache, paste0(y, "_season_results.rds")),
      file.path(cache, paste0(y, "_season_results.csv"))
    ) %>%
      ensure_tidy()
    results <- dplyr::bind_rows(results, res)

    q <- load_rds_or_csv(
      file.path(cache, paste0(y, "_season_qualis.rds")),
      file.path(cache, paste0(y, "_season_qualis.csv"))
    ) %>%
      ensure_tidy()
    qualis <- dplyr::bind_rows(qualis, q)

    if (y >= 2011) {
      pt <- load_rds_or_csv(
        file.path(cache, paste0(y, "_season_pitstops.rds")),
        file.path(cache, paste0(y, "_season_pitstops.csv"))
      ) %>%
        ensure_tidy()
      pitstops <- dplyr::bind_rows(pitstops, pt)
    }

    if (y >= 2021) {
      sr <- load_rds_or_csv(
        file.path(cache, paste0(y, "_season_sprint_results.rds")),
        file.path(cache, paste0(y, "_season_sprint_results.csv"))
      ) %>%
        ensure_tidy()
      sprint_results <- dplyr::bind_rows(sprint_results, sr)
    }

    if (y >= 2018) {
      lp <- load_rds_or_csv(
          file.path(cache, paste0(y, "_season_laps.rds")),
          file.path(cache, paste0(y, "_season_laps.csv"))
        ) %>%
        dplyr::mutate("deleted_reason" = as.character(.data$deleted_reason)) %>%
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

#' Get Drivers from the Most Recent Race
#'
#' @description
#' This internal helper function retrieves the list of drivers and their
#' constructors from the most recently completed race in the current season's
#' cached data. It is useful for getting an up-to-date roster.
#'
#' @return A data frame with `driver_id` and `constructor_id` columns for the
#'   drivers in the last recorded round.
#' @noRd
get_last_drivers <- function() {
  season <- f1dataR::get_current_season()
  cache <- getOption("f1predicter.cache")
  res <- load_rds_or_csv(
    file.path(cache, paste0(season, "_season_results.rds")),
    file.path(cache, paste0(season, "_season_results.csv"))
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
#' @noRd
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

#' Clean Column Names in Cached Data Files
#'
#' @description
#' This is an internal maintenance function designed to be run manually. It
#' iterates through all seasonally cached data files (e.g.,
#' "2022_season_results.csv"), reads them, cleans their column names using
#' `janitor::clean_names()`, and overwrites the original file with the cleaned
#' version. This helps enforce a consistent snake_case naming convention across
#' all data.
#'
#' @return This function does not return a value; it modifies files directly.
#' @noRd
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
        getOption("f1predicter.cache"),
        paste0(y, "_season_rgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    sg <- tryCatch(
      utils::read.csv(file.path(
        getOption("f1predicter.cache"),
        paste0(y, "_season_sgrid.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    res <- tryCatch(
      utils::read.csv(file.path(
        getOption("f1predicter.cache"),
        paste0(y, "_season_results.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    q <- tryCatch(
      utils::read.csv(file.path(
        getOption("f1predicter.cache"),
        paste0(y, "_season_qualis.csv")
      )),
      error = function(e) return(NULL)
    ) %>%
      ensure_tidy()
    if (y >= 2011) {
      pt <- tryCatch(
        utils::read.csv(file.path(
          getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
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
            getOption("f1predicter.cache"),
            paste0(y, "_season_sprint_results.csv")
          ),
          quote = F,
          row.names = F
        )
    }
    closeAllConnections()
  }
}

#' Add Driver IDs to Lap Data
#'
#' @description
#' This internal helper function enriches a lap data frame by adding the
#' persistent `driver_id`. Lap data from `f1dataR` often identifies drivers
#' by their three-letter code (e.g., "VER"). This function loads the driver
#' list for a given season and performs a left join to map these codes to
#' their corresponding `driver_id`.
#'
#' @param laps A data frame of lap data, as returned by `f1dataR`. It must
#'   contain a `driver` column with the three-letter driver code.
#' @param season The numeric championship season to load driver information for.
#'   Defaults to the current season via `f1dataR::get_current_season()`.
#'
#' @return The input `laps` data frame with an added `driver_id` column.
#' @noRd
add_drivers_to_laps <- function(laps, season = f1dataR::get_current_season()) {
  drivers <- f1dataR::load_drivers(season = season) %>%
    dplyr::select("driver_id", "code")
  laps %>%
    dplyr::left_join(drivers, by = c(driver = "code"))
}


#' Cleaned Data
#'
#' @description A data frame used for internal testing
#'
#' @noRd
