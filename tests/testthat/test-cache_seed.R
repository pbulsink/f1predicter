test_that(".validate_cache_snapshot() rejects invalid files (#14)", {
  bad_snapshot <- withr::local_tempfile(fileext = ".sqlite")
  writeLines("not a sqlite database", bad_snapshot)

  expect_error(
    .validate_cache_snapshot(bad_snapshot),
    "not a readable SQLite database"
  )
})

test_that(".validate_cache_snapshot() requires expected cache tables (#14)", {
  db_path <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "other_table", tibble::tibble(x = 1L))

  expect_error(
    .validate_cache_snapshot(db_path),
    "does not contain any expected cache tables"
  )
})

test_that("seed_cache_from_release() downloads and validates release asset (#14)", {
  skip_if_not_installed("piggyback")
  cache_dir <- withr::local_tempdir()
  source_db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), source_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(
    con,
    "results",
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton")
  )

  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, overwrite) {
      file.copy(source_db, dest, overwrite = TRUE)
      invisible(dest)
    },
    .package = "piggyback"
  )

  destination <- seed_cache_from_release(
    tag = "cache-2025",
    repo = "pbulsink/f1predicter",
    asset_name = "f1predicter.sqlite",
    cache = cache_dir,
    overwrite = TRUE
  )

  expect_identical(destination, cache_db_path(cache_dir))

  read_con <- DBI::dbConnect(RSQLite::SQLite(), destination)
  on.exit(DBI::dbDisconnect(read_con), add = TRUE)
  expect_true(DBI::dbExistsTable(read_con, "results"))
})

test_that("seed_cache_from_release() errors if cache exists and overwrite is FALSE (#14)", {
  cache_dir <- withr::local_tempdir()
  existing <- cache_db_path(cache_dir)
  file.create(existing)

  expect_error(
    seed_cache_from_release(cache = cache_dir),
    "Cache already exists"
  )
})

test_that("seed_cache_from_release() checks piggyback before overwrite checks (#14)", {
  cache_dir <- withr::local_tempdir()
  existing <- cache_db_path(cache_dir)
  file.create(existing)

  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE) {
      FALSE
    },
    .package = "base"
  )

  expect_error(
    seed_cache_from_release(cache = cache_dir),
    "Package.*piggyback.*is required"
  )
})

test_that("publish_cache_snapshot() uploads local snapshot with piggyback (#14)", {
  skip_if_not_installed("piggyback")
  cache_dir <- withr::local_tempdir()
  db_path <- cache_db_path(cache_dir)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(
    con,
    "results",
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton")
  )

  uploaded <- NULL
  local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite) {
      uploaded <<- list(
        file = file,
        repo = repo,
        tag = tag,
        overwrite = overwrite
      )
      "uploaded"
    },
    .package = "piggyback"
  )

  result <- publish_cache_snapshot(
    tag = "cache-2025",
    repo = "pbulsink/f1predicter",
    cache = cache_dir,
    overwrite = FALSE
  )

  expect_identical(result, "uploaded")
  expect_identical(uploaded$file, db_path)
  expect_identical(uploaded$repo, "pbulsink/f1predicter")
  expect_identical(uploaded$tag, "cache-2025")
  expect_false(uploaded$overwrite)
})

test_that("publish_cache_snapshot() checks piggyback before local cache checks (#14)", {
  cache_dir <- withr::local_tempdir()

  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE) {
      FALSE
    },
    .package = "base"
  )

  expect_error(
    publish_cache_snapshot(cache = cache_dir),
    "Package.*piggyback.*is required"
  )
})

test_that("seed_cache_from_release() seeds cache used by clean_data() (#14)", {
  skip_if_not_installed("piggyback")
  cache_dir <- withr::local_tempdir()
  withr::local_options(f1predicter.cache = cache_dir)
  source_db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), source_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "results",
    tibble::tibble(season = 2024, round = 1, driver_id = "hamilton")
  )
  DBI::dbWriteTable(
    con,
    "processed_data",
    tibble::tibble(
      season = 2024,
      round = 1,
      driver_id = "hamilton",
      finished = 1
    )
  )

  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, overwrite) {
      file.copy(source_db, dest, overwrite = TRUE)
      invisible(dest)
    },
    .package = "piggyback"
  )

  seed_cache_from_release(
    tag = "cache-2025",
    repo = "pbulsink/f1predicter",
    cache = cache_dir,
    overwrite = TRUE
  )

  cleaned <- clean_data(
    input = list(results = tibble::tibble(season = 2024)),
    cache_processed = TRUE
  )

  expect_identical(cleaned$driver_id, "hamilton")
  expect_identical(cleaned$finished, 1)
})
