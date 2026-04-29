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

.cache_tables <- c(
  "results",
  "qualis",
  "pitstops",
  "rgrid",
  "sgrid",
  "sprint_results",
  "laps"
)

cache_db_path <- function(
  cache = getOption("f1predicter.cache", default = tempdir())
) {
  if (is.null(cache) || !nzchar(cache)) {
    cache <- tempdir()
  }

  file.path(cache, "f1predicter.sqlite")
}

open_cache_db <- function(
  cache = getOption("f1predicter.cache", default = tempdir())
) {
  path <- cache_db_path(cache)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}

.has_cache_rows <- function(data) {
  is.data.frame(data) && nrow(data) > 0
}

.create_cache_index <- function(con, table) {
  if (!DBI::dbExistsTable(con, table)) {
    return(invisible(NULL))
  }

  fields <- DBI::dbListFields(con, table)
  if (!all(c("season", "round") %in% fields)) {
    return(invisible(NULL))
  }

  DBI::dbExecute(
    con,
    paste0(
      'CREATE INDEX IF NOT EXISTS "',
      table,
      '_season_round_idx" ON "',
      table,
      '" ("season", "round")'
    )
  )

  invisible(NULL)
}

.delete_cache_rows <- function(con, table, data) {
  if (!DBI::dbExistsTable(con, table) || !.has_cache_rows(data)) {
    return(invisible(NULL))
  }

  key_cols <- intersect(c("season", "round"), DBI::dbListFields(con, table))
  if (length(key_cols) == 0) {
    return(invisible(NULL))
  }

  key_data <- unique(data[, key_cols, drop = FALSE])
  where <- paste(sprintf('"%s" = ?', key_cols), collapse = " AND ")
  query <- paste0('DELETE FROM "', table, '" WHERE ', where)

  for (i in seq_len(nrow(key_data))) {
    DBI::dbExecute(
      con,
      query,
      params = unname(as.list(key_data[i, , drop = TRUE]))
    )
  }

  invisible(NULL)
}

#' Write a Cached SQLite Table
#'
#' @description
#' Internal helper to write a cache table into the SQLite database. When
#' `overwrite = TRUE`, the full table is replaced. When `overwrite = FALSE`,
#' rows for matching `season`/`round` keys are deleted before appending the new
#' rows.
#'
#' @param data A data frame to store.
#' @param table Cache table name.
#' @param con A live SQLite connection created by [open_cache_db()].
#' @param overwrite Logical indicating whether to replace the full table.
#'
#' @return Invisibly returns `data`.
#' @noRd
write_cache_table <- function(data, table, con, overwrite = FALSE) {
  if (is.null(data) || !.has_cache_rows(data)) {
    return(invisible(data))
  }

  data <- ensure_tidy(data)

  if (overwrite || !DBI::dbExistsTable(con, table)) {
    DBI::dbWriteTable(con, table, data, overwrite = TRUE)
  } else {
    .delete_cache_rows(con, table, data)
    DBI::dbAppendTable(con, table, data)
  }

  .create_cache_index(con, table)

  invisible(data)
}

#' Read a Cached SQLite Table
#'
#' @description
#' Internal helper to read one cache table from the SQLite database, optionally
#' filtered to a single season and/or round.
#'
#' @param table Cache table name.
#' @param con A live SQLite connection created by [open_cache_db()].
#' @param season Optional season filter.
#' @param round Optional round filter.
#'
#' @return A tibble when rows are found, otherwise `NULL`.
#' @noRd
read_cache_table <- function(table, con, season = NULL, round = NULL) {
  if (!DBI::dbExistsTable(con, table)) {
    return(NULL)
  }

  query <- paste0('SELECT * FROM "', table, '"')
  clauses <- character(0)
  params <- list()

  if (!is.null(season)) {
    clauses <- c(clauses, '"season" = ?')
    params <- c(params, list(as.numeric(season)))
  }
  if (!is.null(round)) {
    clauses <- c(clauses, '"round" = ?')
    params <- c(params, list(as.numeric(round)))
  }
  if (length(clauses) > 0) {
    query <- paste(query, "WHERE", paste(clauses, collapse = " AND "))
  }

  data <- if (length(params) == 0) {
    DBI::dbGetQuery(con, query)
  } else {
    DBI::dbGetQuery(con, query, params = params)
  }
  if (nrow(data) == 0) {
    return(NULL)
  }

  data %>%
    tibble::as_tibble() %>%
    ensure_tidy()
}

.read_cache_with_legacy_fallback <- function(
  table,
  con,
  season = NULL,
  round = NULL,
  rds_path,
  csv_path = NULL,
  col_classes = NULL
) {
  data <- read_cache_table(table, con, season = season, round = round)

  if (is.null(data)) {
    data <- load_rds_or_csv(
      rds_path = rds_path,
      csv_path = csv_path,
      col_classes = col_classes
    ) %>%
      ensure_tidy()

    write_cache_table(data, table, con, overwrite = FALSE)
  }

  data
}

.sqlite_cache_populated <- function(con, tables = .cache_tables) {
  any(vapply(
    tables,
    function(table) DBI::dbExistsTable(con, table),
    logical(1)
  ))
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
  con <- open_cache_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

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

  if (!round %in% schedule[schedule$season == season, ]$round) {
    cli::cli_abort(
      "Error in f1predicter::get_weekend_data(). Round: {round} not in season: {season}."
    )
  }

  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  quali <- NULL
  laps <- NULL

  if (!force) {
    cache <- getOption("f1predicter.cache")
    pfx <- paste0(season, "_", round, "_")

    rgrid <- .read_cache_with_legacy_fallback(
      table = "rgrid",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "rgrid.rds")),
      csv_path = file.path(cache, paste0(pfx, "rgrid.csv"))
    )
    sgrid <- .read_cache_with_legacy_fallback(
      table = "sgrid",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "sgrid.rds")),
      csv_path = file.path(cache, paste0(pfx, "sgrid.csv"))
    )
    results <- .read_cache_with_legacy_fallback(
      table = "results",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "results.rds")),
      csv_path = file.path(cache, paste0(pfx, "results.csv"))
    )
    sprint_results <- .read_cache_with_legacy_fallback(
      table = "sprint_results",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "sprint_results.rds")),
      csv_path = file.path(cache, paste0(pfx, "sptrint_results.csv"))
    )
    pitstops <- .read_cache_with_legacy_fallback(
      table = "pitstops",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "pitstops.rds")),
      csv_path = file.path(cache, paste0(pfx, "pitstops.csv"))
    )
    quali <- .read_cache_with_legacy_fallback(
      table = "qualis",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "quali.rds")),
      csv_path = file.path(cache, paste0(pfx, "quali.csv"))
    )
    laps <- .read_cache_with_legacy_fallback(
      table = "laps",
      con = con,
      season = season,
      round = round,
      rds_path = file.path(cache, paste0(pfx, "laps.rds")),
      csv_path = file.path(cache, paste0(pfx, "laps.csv"))
    )

    if (
      any(vapply(
        list(rgrid, sgrid, results, pitstops, quali, laps),
        .has_cache_rows,
        logical(1)
      ))
    ) {
      cat("Found some old data - won't reload what's already available.\n")
    }
  }

  if (!.has_cache_rows(results)) {
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
      write_cache_table(results, "results", con, overwrite = FALSE)
    }
  }

  if (season >= 2021) {
    if (!.has_cache_rows(sprint_results)) {
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
          write_cache_table(
            sprint_results,
            "sprint_results",
            con,
            overwrite = FALSE
          )
        }
      }
    }
  } else {
    sprint_results <- NULL
  }

  if (season >= 2011) {
    if (!.has_cache_rows(pitstops)) {
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
          write_cache_table(pitstops, "pitstops", con, overwrite = FALSE)
        }
      }
    }
  } else {
    pitstops <- NULL
  }

  if (season >= 2003) {
    if (!.has_cache_rows(rgrid)) {
      rgrid <- get_grids(season = season, round = round, session = "R")
      if (!is.null(rgrid)) {
        rgrid <- rgrid %>%
          dplyr::mutate(season = season, round = round) %>%
          dplyr::mutate(position = as.integer(.data$position)) %>%
          janitor::clean_names()
        write_cache_table(rgrid, "rgrid", con, overwrite = FALSE)
      }
    }

    if (!.has_cache_rows(sgrid)) {
      sgrid <- get_grids(season = season, round = round, session = "S")
      if (!is.null(sgrid)) {
        sgrid <- sgrid %>%
          dplyr::mutate(season = season, round = round) %>%
          janitor::clean_names()
        write_cache_table(sgrid, "sgrid", con, overwrite = FALSE)
      }
    }

    if (!.has_cache_rows(quali)) {
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
          write_cache_table(quali, "qualis", con, overwrite = FALSE)
        }
      }
    }
  } else {
    rgrid <- NULL
    sgrid <- NULL
    quali <- NULL
  }

  if (season >= 2018) {
    if (!.has_cache_rows(laps)) {
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
        write_cache_table(laps, "laps", con, overwrite = FALSE)
      }
    }
  } else {
    laps <- NULL
  }

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

#' Migrate Legacy Cache Files to SQLite Format
#'
#' @description
#' One-time maintenance utility that converts existing season-level cache files
#' (`.rds` preferred, `.csv` fallback) into the SQLite cache used by current
#' versions of the package.
#'
#' @param cache Path to the cache directory. Defaults to
#'   `getOption("f1predicter.cache")`.
#' @param years Integer vector of season years to migrate. Defaults to
#'   `1990:f1dataR::get_current_season()`.
#'
#' @return Invisibly returns a character vector of table names written during
#'   this call.
#' @noRd
migrate_cache_to_sqlite <- function(
  cache = getOption("f1predicter.cache"),
  years = 1990:f1dataR::get_current_season()
) {
  written <- character(0)
  con <- open_cache_db(cache = cache)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (y in years) {
    for (type in .cache_tables) {
      rds_path <- file.path(cache, paste0(y, "_season_", type, ".rds"))
      csv_path <- file.path(cache, paste0(y, "_season_", type, ".csv"))
      dat <- load_rds_or_csv(rds_path = rds_path, csv_path = csv_path)

      if (.has_cache_rows(dat)) {
        source_path <- if (file.exists(rds_path)) rds_path else csv_path
        write_cache_table(dat, type, con, overwrite = FALSE)
        written <- c(written, type)
        cli::cli_inform(
          "Migrated {.file {source_path}} into {.file {cache_db_path(cache)}}"
        )
      }
    }
  }

  processed_path <- file.path(cache, "processed_data.rds")
  if (file.exists(processed_path)) {
    processed_data <- tryCatch(
      readRDS(processed_path),
      error = function(e) NULL
    )

    if (.has_cache_rows(processed_data)) {
      write_cache_table(processed_data, "processed_data", con, overwrite = TRUE)
      written <- c(written, "processed_data")
      cli::cli_inform(
        "Migrated {.file {processed_path}} into {.file {cache_db_path(cache)}}"
      )
    }
  }

  written <- unique(written)
  cli::cli_inform("Migration complete: {length(written)} table{?s} written.")
  invisible(written)
}


# Load all data from legacy season cache files.
.load_all_data_from_files <- function(cache = getOption("f1predicter.cache")) {
  rgrid <- NULL
  sgrid <- NULL
  results <- NULL
  sprint_results <- NULL
  pitstops <- NULL
  laps <- NULL
  qualis <- NULL

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
  }

  list(
    results = results,
    sprint_results = sprint_results,
    laps = laps,
    pitstops = pitstops,
    sgrid = sgrid,
    rgrid = rgrid,
    qualis = qualis
  )
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
  cache <- getOption("f1predicter.cache")
  con <- open_cache_db(cache = cache)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (.sqlite_cache_populated(con)) {
    cached_data <- list(
      results = read_cache_table("results", con),
      sprint_results = read_cache_table("sprint_results", con),
      laps = read_cache_table("laps", con),
      pitstops = read_cache_table("pitstops", con),
      sgrid = read_cache_table("sgrid", con),
      rgrid = read_cache_table("rgrid", con),
      qualis = read_cache_table("qualis", con)
    )

    if (.has_cache_rows(cached_data$laps)) {
      cached_data$laps <- cached_data$laps %>%
        dplyr::mutate("deleted_reason" = as.character(.data$deleted_reason)) %>%
        ensure_tidy()
    }

    if (any(vapply(cached_data, is.null, logical(1)))) {
      legacy_data <- .load_all_data_from_files(cache = cache)
      missing_tables <- names(cached_data)[vapply(
        cached_data,
        is.null,
        logical(1)
      )]
      for (table in missing_tables) {
        cached_data[[table]] <- legacy_data[[table]]
      }
    }

    return(cached_data)
  }

  .load_all_data_from_files(cache = cache)
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
  con <- open_cache_db(cache = cache)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- read_cache_table("results", con, season = season)

  if (is.null(res)) {
    res <- load_rds_or_csv(
      file.path(cache, paste0(season, "_season_results.rds")),
      file.path(cache, paste0(season, "_season_results.csv"))
    ) %>%
      ensure_tidy()
  }

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
