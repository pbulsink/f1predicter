test_that("clean_data() returns a tibble with expected structure and valid data", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  result <- cleaned_data

  # Should be a tibble
  expect_s3_class(result, "tbl_df")

  # Should have rows
  expect_gt(nrow(result), 0)

  # Should have essential columns
  expected_cols <- c(
    "season",
    "round",
    "driver_id",
    "constructor_id",
    "grid",
    "position",
    "points",
    "finished"
  )
  for (col in expected_cols) {
    expect_true(
      col %in% names(result),
      paste("Column", col, "should exist in clean_data()")
    )
  }

  # All seasons should be numeric and reasonable
  expect_true(all(result$season >= 1950))
  expect_true(all(result$season <= 2030))

  # Grid and position should be positive (or zero/NA)
  expect_true(all(result$grid >= 0 | is.na(result$grid)))
  expect_true(all(result$position >= 0 | is.na(result$position)))

  # Finished should be 0, 1, or NA
  valid_vals <- c(0, 1, NA)
  expect_true(all(result$finished %in% valid_vals | is.na(result$finished)))

  # Points should be non-negative
  expect_true(all(result$points >= 0 | is.na(result$points)))
})

test_that("get_season_data() errors on invalid season", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  # Should error on future season or invalid year
  expect_error(get_season_data(9999))
})

test_that("get_weekend_data() returns data for valid race", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")
  if (!dir.exists(getOption('f1predicter.cache'))) {
    skip("No cached data")
  }
  result <- get_weekend_data(2023, 1)

  expect_type(result, "list")
  result <- result$results
  expect_s3_class(result, 'data.frame')
  expect_gt(nrow(result), 0)
  expect_true(all(result$round == 1))
})

test_that("get_weekend_data() errors on invalid round", {
  expect_error(get_weekend_data(2023, 99))
})

# ---- get_processing_params() tests ----

test_that("get_processing_params() returns a named list with all expected keys", {
  params <- get_processing_params()

  expect_type(params, "list")

  expected_keys <- c(
    "grid",
    "position",
    "driver_finish_avg",
    "driver_failure_avg",
    "constructor_failure_avg",
    "grid_pos_corr_avg",
    "constructor_pit_duration_perc",
    "quali_avg_perc",
    "fastest_pit",
    "pos_change",
    "qgap",
    "constructor_pit_num_perc"
  )
  for (k in expected_keys) {
    expect_true(k %in% names(params), info = paste("key", k, "should exist"))
    expect_type(params[[k]], "double")
  }
})

test_that("get_processing_params() values are in plausible ranges", {
  params <- get_processing_params()

  expect_gt(params$grid, 0)
  expect_lt(params$grid, 25)
  expect_gt(params$position, 0)
  expect_lt(params$position, 25)
  expect_true(params$driver_finish_avg > 0 && params$driver_finish_avg < 1)
  expect_true(params$driver_failure_avg > 0 && params$driver_failure_avg < 1)
  expect_true(
    params$constructor_failure_avg > 0 && params$constructor_failure_avg < 1
  )
  expect_true(params$fastest_pit > 0)
})

test_that("clean_data() respects custom params", {
  withr::local_options(f1predicter.cache = "~/Documents/f1predicter/cache")

  # Modify one parameter and verify it can be passed without error
  custom_params <- get_processing_params()
  custom_params$fastest_pit <- 3.0

  # Just check that it runs without error; we skip deep value verification
  # because that requires a full data reload which is expensive in CI.
  cache_dir <- getOption("f1predicter.cache")
  skip_if(
    is.null(cache_dir) || !dir.exists(cache_dir),
    "Cache directory not available in this environment"
  )
  result <- clean_data(params = custom_params)
  expect_s3_class(result, "tbl_df")
})

test_that("process_results_data() derives joined and rolling race features", {
  base_results <- cleaned_data |>
    dplyr::filter(.data$season == 2024, .data$round == 1) |>
    dplyr::slice_head(n = 5)

  raw_input <- list(
    results = base_results |>
      dplyr::transmute(
        driver_id = .data$driver_id,
        constructor_id = c("tyrrell", as.character(.data$constructor_id[-1])),
        position = .data$position,
        grid = c(0, .data$grid[-1]),
        fastest_rank = .data$fastest_rank,
        time_sec = .data$fastest_time,
        points = .data$points,
        status = .data$status,
        season = .data$season,
        round = .data$round
      ),
    rgrid = base_results |>
      dplyr::transmute(
        position = .data$quali_position,
        quali_results = .data$driver_id,
        season = .data$season,
        round = .data$round
      )
  )

  result <- process_results_data(raw_input)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(base_results))
  expect_equal(result$constructor_id[[1]], "mercedes")
  expect_equal(result$grid[[1]], nrow(base_results))
  expect_true(all(result$points_after >= result$points_before))
  expect_false(any(is.na(result$quali_position)))
})

test_that("process_lap_times() and summarize_practice_laps() rank practice sessions", {
  base_results <- cleaned_data |>
    dplyr::filter(.data$season == 2024, .data$round == 1) |>
    dplyr::slice_head(n = 2)

  laps <- tibble::tibble(
    driver_id = c(
      base_results$driver_id[[1]],
      base_results$driver_id[[1]],
      base_results$driver_id[[2]],
      base_results$driver_id[[1]],
      base_results$driver_id[[2]],
      base_results$driver_id[[1]]
    ),
    season = 2024L,
    round = 1L,
    session_type = c("FP1", "FP1", "FP1", "FP2", "FP2", "Q"),
    deleted = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    lap_time = c(90, 91, 91, 89, 90, 88),
    sector1time = c(30, 31, 30.5, 29.5, 30, 29),
    sector2time = c(30, 30, 30.5, 29.5, 30, 29),
    sector3time = c(30, 30, NA, 30, 30, 30)
  )

  processed_laps <- process_lap_times(laps)
  practices <- summarize_practice_laps(processed_laps)

  expect_s3_class(processed_laps, "data.frame")
  expect_true(all(processed_laps$session_type %in% c("FP1", "FP2", "Q")))
  expect_true(all(processed_laps$num_laps >= 1))
  expect_false(any(is.na(processed_laps$rank)))
  expect_false(any(is.na(processed_laps$optimal_rank)))

  expect_s3_class(practices, "data.frame")
  expect_equal(nrow(practices), 2)
  expect_true(all(practices$practice_num_laps >= 2))
  expect_true(all(practices$practice_best_rank <= practices$practice_avg_rank))
})

test_that("process_quali_times() and process_pit_stops() compute fallback metrics", {
  quali_input <- cleaned_data |>
    dplyr::filter(.data$season == 2024, .data$round %in% c(1, 2)) |>
    dplyr::filter(.data$driver_id %in% unique(.data$driver_id)[1:2]) |>
    dplyr::select(
      "driver_id",
      "q1_sec",
      "q2_sec",
      "q3_sec",
      "season",
      "round"
    ) |>
    dplyr::mutate(
      q2_sec = dplyr::if_else(dplyr::row_number() == 1, NA_real_, .data$q2_sec),
      q3_sec = dplyr::if_else(dplyr::row_number() == 1, NA_real_, .data$q3_sec)
    )

  params <- get_processing_params()
  quali_result <- process_quali_times(quali_input, params = params)
  first_driver_id <- quali_result$driver_id[[1]]

  first_driver <- quali_result |>
    dplyr::filter(.data$driver_id == first_driver_id) |>
    dplyr::arrange(.data$season, .data$round)

  pit_input <- tibble::tibble(
    driver_id = c("driver_a", "driver_a", "driver_b"),
    stop = c(1, 2, 1),
    duration = c(24, 26, 25),
    season = 2024L,
    round = 1L
  )
  pit_result <- process_pit_stops(pit_input, params = params)

  expect_false(any(is.na(quali_result$q_min_perc)))
  expect_false(any(is.na(quali_result$qgap)))
  expect_false(any(is.na(quali_result$q_avg_perc)))
  expect_equal(first_driver$driver_avg_qgap[[1]], params$qgap)

  expect_s3_class(pit_result, "data.frame")
  expect_equal(nrow(pit_result), 2)
  expect_true(all(pit_result$pit_duration_perc >= 1))
  expect_true(pit_result$pit_num_perc[pit_result$driver_id == "driver_a"] > 1)
})

test_that("process_sprint_data() builds sprint features by driver and weekend (#noissue)", {
  sprint_results <- tibble::tibble(
    driver_id = c("driver_a", "driver_b", "driver_a"),
    season = c(2024L, 2024L, 2022L),
    round = c(1L, 1L, 2L),
    grid = c(2, 5, 3),
    position = c(1, 4, 2),
    points = c(8, 5, 7)
  )
  sched <- tibble::tibble(
    season = c("2024", "2022"),
    round = c("1", "2"),
    sprint_date = as.Date(c("2024-03-01", NA))
  )

  result <- process_sprint_data(sprint_results, sched)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(
    c(
      "driver_id",
      "has_sprint",
      "sprint_era",
      "sprint_grid",
      "sprint_finish_pos",
      "sprint_points"
    ) %in% names(result)
  ))
  expect_equal(
    unique(result$has_sprint[result$season == 2024L & result$round == 1L]),
    "Yes"
  )
  expect_equal(
    unique(result$has_sprint[result$season == 2022L & result$round == 2L]),
    "No"
  )
  expect_equal(
    unique(result$sprint_era[result$season == 2024L]),
    "2023+"
  )
})

test_that("constructor, circuit, and final feature builders preserve modeled columns", {
  base_results <- cleaned_data |>
    dplyr::filter(.data$season == 2024, .data$round %in% c(1, 2))

  pit_features <- base_results |>
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "pit_duration_perc",
      "pit_num_perc"
    )

  constructor_results_input <- base_results |>
    dplyr::select(-dplyr::any_of(c("pit_duration_perc", "pit_num_perc")))

  constructor_features <- create_constructor_features(
    constructor_results_input,
    pit_features
  )
  circuit_features <- create_circuit_features(base_results)

  qualis <- base_results |>
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "q_min_perc",
      "q_avg_perc",
      "driver_avg_qgap"
    ) |>
    dplyr::mutate(
      q_min_perc = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        .data$q_min_perc
      ),
      q_avg_perc = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        .data$q_avg_perc
      ),
      driver_avg_qgap = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        .data$driver_avg_qgap
      )
    )

  practices <- base_results |>
    dplyr::select(
      "driver_id",
      "season",
      "round",
      "practice_avg_rank",
      "practice_best_rank",
      "practice_num_laps",
      "practice_avg_gap",
      "practice_best_gap",
      "practice_optimal_rank"
    ) |>
    dplyr::mutate(
      practice_best_gap = dplyr::if_else(
        dplyr::row_number() == 1,
        Inf,
        .data$practice_best_gap
      ),
      practice_avg_gap = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        .data$practice_avg_gap
      )
    )

  pitstops <- pit_features |>
    dplyr::mutate(
      pit_duration_perc = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        .data$pit_duration_perc
      )
    )

  results <- base_results |>
    dplyr::select(
      "driver_id",
      "constructor_id",
      "position",
      "grid",
      "quali_position",
      "pos_change",
      "weighted_passes",
      "pos_change_points",
      "points",
      "status",
      "points_before",
      "points_after",
      "driver_experience",
      "season",
      "round",
      "driver_failure",
      "constructor_failure",
      "finished"
    ) |>
    dplyr::mutate(
      quali_position = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_integer_,
        .data$quali_position
      )
    )

  sched <- schedule |>
    dplyr::filter(.data$season == "2024", .data$round %in% c("1", "2"))

  final_data <- combine_and_finalize_features(
    results = results,
    qualis = qualis,
    practices = practices,
    pitstops = pitstops,
    constructor_results = constructor_features,
    sprint_features = tibble::tibble(
      season = c(2024L, 2024L),
      round = c(1L, 2L),
      driver_id = c(results$driver_id[[1]], results$driver_id[[2]]),
      has_sprint = c("Yes", "No"),
      sprint_era = c("2023+", "2023+"),
      sprint_grid = c(NA_real_, 4),
      sprint_finish_pos = c(NA_real_, 5),
      sprint_points = c(NA_real_, 6)
    ),
    schedule = sched
  )

  expect_s3_class(constructor_features, "data.frame")
  expect_false(any(is.na(constructor_features$constructor_grid_avg)))
  expect_s3_class(circuit_features, "data.frame")
  expect_false(any(is.na(circuit_features$grid_pos_corr_avg)))

  expect_s3_class(final_data, "data.frame")
  expect_true("round_id" %in% names(final_data))
  expect_false(any(is.na(final_data$q_min_perc)))
  expect_false(any(is.na(final_data$practice_best_gap)))
  expect_false(any(is.na(final_data$pit_duration_perc)))
  expect_false(any(is.na(final_data$sprint_grid)))
  expect_false(any(is.na(final_data$sprint_finish_pos)))
  expect_false(any(is.na(final_data$sprint_points)))
  expect_equal(final_data$quali_position[[1]], final_data$grid[[1]])
})

test_that("clean_data() prefers cached processed data when requested", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(list(f1predicter.cache = cache_dir))

  cached <- tibble::tibble(
    driver_id = "cached_driver",
    season = 2026L,
    round = 1L
  )
  saveRDS(cached, file.path(cache_dir, "processed_data.rds"))

  result <- clean_data(
    input = list(results = tibble::tibble(season = 2024L)),
    cache_processed = TRUE
  )

  expect_identical(result, cached)
})

# ---- cache_to_rds / load_rds_or_csv round-trip ----

test_that("cache_to_rds and load_rds_or_csv round-trip correctly", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  cache_to_rds(df, tmp)
  expect_true(file.exists(tmp))

  loaded <- load_rds_or_csv(tmp)
  expect_equal(loaded, df)
})

test_that("load_rds_or_csv falls back to CSV when RDS absent", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_rds <- tempfile(fileext = ".rds")
  on.exit({
    unlink(tmp_csv)
    unlink(tmp_rds)
  })

  df <- data.frame(a = 1:3, b = 4:6)
  utils::write.csv(df, tmp_csv, row.names = FALSE)

  loaded <- load_rds_or_csv(tmp_rds, tmp_csv)
  expect_equal(nrow(loaded), 3)
  expect_true("a" %in% names(loaded))
})

test_that("load_rds_or_csv returns NULL when neither file exists", {
  result <- load_rds_or_csv(
    tempfile(fileext = ".rds"),
    tempfile(fileext = ".csv")
  )
  expect_null(result)
})

test_that(".can_cache_event_data() waits until the day after the event (#9)", {
  schedule <- tibble::tibble(
    season = 2024,
    round = 5,
    date = as.Date("2024-05-12")
  )

  expect_false(.can_cache_event_data(2024, 5, schedule, today = "2024-05-12"))
  expect_false(.can_cache_event_data(2024, 5, schedule, today = "2024-05-11"))
  expect_true(.can_cache_event_data(2024, 5, schedule, today = "2024-05-13"))
})

test_that(".read_cache_with_legacy_fallback() skips SQLite writes when caching is disabled (#9)", {
  cache_dir <- withr::local_tempdir()
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  legacy_results <- tibble::tibble(
    season = 2024,
    round = 5,
    driver_id = "hamilton",
    points = 25
  )
  legacy_path <- file.path(cache_dir, "2024_5_results.rds")
  saveRDS(legacy_results, legacy_path)

  loaded <- .read_cache_with_legacy_fallback(
    table = "results",
    con = con,
    season = 2024,
    round = 5,
    rds_path = legacy_path,
    cache_write = FALSE
  )

  expect_equal(loaded, legacy_results)
  expect_null(read_cache_table("results", con, season = 2024, round = 5))
})

# ---- migrate_cache_to_rds() ----

test_that("migrate_cache_to_rds() converts CSVs to RDS in a temp directory", {
  tmp_dir <- tempdir()

  # Write a small fake CSV file
  df <- data.frame(driver_id = "hamilton", points = 25L)
  csv_path <- file.path(tmp_dir, "2022_season_results.csv")
  rds_path <- file.path(tmp_dir, "2022_season_results.rds")
  on.exit({
    unlink(csv_path)
    unlink(rds_path)
  })
  utils::write.csv(df, csv_path, row.names = FALSE)

  withr::local_options(f1predicter.cache = tmp_dir)
  written <- migrate_cache_to_rds(cache = tmp_dir, years = 2022)

  expect_true(file.exists(rds_path))
  expect_true(rds_path %in% written)

  loaded <- readRDS(rds_path)
  expect_equal(nrow(loaded), 1)
})

test_that("migrate_cache_to_rds() skips files already migrated", {
  tmp_dir <- tempdir()

  df <- data.frame(driver_id = "leclerc", points = 18L)
  csv_path <- file.path(tmp_dir, "2023_season_results.csv")
  rds_path <- file.path(tmp_dir, "2023_season_results.rds")
  on.exit({
    unlink(csv_path)
    unlink(rds_path)
  })
  utils::write.csv(df, csv_path, row.names = FALSE)
  saveRDS(df, rds_path) # already exists

  withr::local_options(f1predicter.cache = tmp_dir)
  written <- migrate_cache_to_rds(cache = tmp_dir, years = 2023)

  # Should not re-write anything
  expect_length(written, 0)
})

test_that("SQLite cache helpers round-trip filtered data (#9)", {
  cache_dir <- withr::local_tempdir()
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  results <- tibble::tibble(
    season = c(2024, 2024, 2025),
    round = c(1, 2, 1),
    driver_id = c("hamilton", "leclerc", "piastri"),
    points = c(25, 18, 15)
  )

  write_cache_table(results, "results", con, overwrite = TRUE)

  filtered <- read_cache_table("results", con, season = 2024, round = 2)
  expect_equal(filtered$driver_id, "leclerc")
  expect_equal(filtered$points, 18)

  season_data <- read_cache_table("results", con, season = 2024)
  expect_equal(sort(season_data$round), c(1, 2))
})

test_that("read_cache_table() returns NULL for missing SQLite tables or rows (#9)", {
  cache_dir <- withr::local_tempdir()
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_null(read_cache_table("results", con))

  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "results",
    con,
    overwrite = TRUE
  )

  expect_null(read_cache_table("results", con, season = 2023))
  expect_null(read_cache_table("results", con, season = 2024, round = 2))
})

test_that("write_cache_table() replaces matching season-round rows on append (#9)", {
  cache_dir <- withr::local_tempdir()
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  initial_results <- tibble::tibble(
    season = c(2024, 2024),
    round = c(1, 2),
    driver_id = c("hamilton", "leclerc"),
    points = c(25, 18)
  )
  updated_round <- tibble::tibble(
    season = 2024,
    round = 2,
    driver_id = "leclerc",
    points = 19
  )

  write_cache_table(initial_results, "results", con, overwrite = TRUE)
  write_cache_table(updated_round, "results", con, overwrite = FALSE)

  all_results <- read_cache_table("results", con, season = 2024)
  expect_equal(nrow(all_results), 2)
  expect_equal(
    all_results$points[all_results$round == 2],
    19
  )
})

test_that("migrate_cache_to_sqlite() migrates cached season files into SQLite (#9)", {
  cache_dir <- withr::local_tempdir()
  results <- tibble::tibble(
    season = c(2022, 2022),
    round = c(1, 2),
    driver_id = c("hamilton", "russell"),
    points = c(25, 18)
  )
  results_path <- file.path(cache_dir, "2022_season_results.rds")
  saveRDS(results, results_path)

  written <- migrate_cache_to_sqlite(cache = cache_dir, years = 2022)

  expect_true("results" %in% written)

  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  migrated <- read_cache_table("results", con, season = 2022)
  expect_equal(migrated$driver_id, c("hamilton", "russell"))
})

test_that("load_all_data() reads SQLite cache tables (#9)", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(f1predicter.cache = cache_dir)
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "results",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "sprint_results",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(
      season = 2024,
      round = 1,
      driver_id = "hamilton",
      deleted_reason = NA_character_
    ),
    "laps",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "pitstops",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(season = 2024, round = 1, position = 1L),
    "sgrid",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(season = 2024, round = 1, position = 1L),
    "rgrid",
    con,
    overwrite = TRUE
  )
  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "qualis",
    con,
    overwrite = TRUE
  )

  all_data <- load_all_data()

  expect_named(
    all_data,
    c(
      "results",
      "sprint_results",
      "laps",
      "pitstops",
      "sgrid",
      "rgrid",
      "qualis"
    )
  )
  expect_equal(all_data$results$driver_id, "hamilton")
  expect_equal(all_data$laps$deleted_reason, NA_character_)
})

test_that("clean_data() reads processed data from SQLite cache (#9)", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(f1predicter.cache = cache_dir)
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cached_processed <- tibble::tibble(
    season = 2024,
    round = 1,
    driver_id = "hamilton",
    finished = 1
  )
  write_cache_table(cached_processed, "processed_data", con, overwrite = TRUE)

  loaded <- clean_data(
    input = list(results = tibble::tibble(season = 2024)),
    cache_processed = TRUE
  )

  expect_equal(loaded, cached_processed)
})

test_that(".sqlite_cache_populated() includes processed data tables (#9)", {
  cache_dir <- withr::local_tempdir()
  con <- open_cache_db(cache = cache_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_false(.sqlite_cache_populated(con))

  write_cache_table(
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton"),
    "processed_data",
    con,
    overwrite = TRUE
  )

  expect_true(.sqlite_cache_populated(con))
})
