#' Validate a SQLite Cache Snapshot
#'
#' @param path Path to a SQLite cache snapshot.
#' @param required_tables Expected cache table names.
#'
#' @return Invisibly returns `path` when validation succeeds.
#' @noRd
.validate_cache_snapshot <- function(
  path,
  required_tables = .raw_cache_tables
) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Error in f1predicter::.validate_cache_snapshot(). File does not exist: {.file {path}}."
    )
  }

  con <- tryCatch(
    suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), path)),
    error = function(e) {
      cli::cli_abort(
        "Error in f1predicter::.validate_cache_snapshot(). Snapshot is not a readable SQLite database: {conditionMessage(e)}"
      )
    }
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- tryCatch(
    DBI::dbListTables(con),
    error = function(e) {
      cli::cli_abort(
        "Error in f1predicter::.validate_cache_snapshot(). Snapshot is not a readable SQLite database: {conditionMessage(e)}"
      )
    }
  )
  if (length(intersect(required_tables, tables)) == 0) {
    cli::cli_abort(
      "Error in f1predicter::.validate_cache_snapshot(). Snapshot does not contain any expected cache tables."
    )
  }

  invisible(path)
}

.check_piggyback_installed <- function() {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort(
      "Error in f1predicter::seed_cache_from_release(). Package {.pkg piggyback} is required. Install it with install.packages(\"piggyback\")."
    )
  }

  invisible(TRUE)
}

#' Seed Cache Data from a GitHub Release Asset
#'
#' @description
#' Downloads a prebuilt SQLite cache snapshot from a GitHub release asset using
#' `{piggyback}`, validates it, and stores it as `f1predicter.sqlite` in the
#' configured cache directory.
#'
#' @param tag GitHub release tag containing the snapshot asset. Defaults to
#'   `"latest"`.
#' @param repo Repository in `"owner/repo"` format.
#' @param asset_name Release asset filename to download.
#' @param cache Cache directory path. Defaults to
#'   `getOption("f1predicter.cache", tempdir())`.
#' @param overwrite Logical. When `FALSE`, errors if a local cache database
#'   already exists.
#'
#' @return Invisibly returns the local SQLite cache path.
#' @export
#'
#' @examples
#' \dontrun{
#' seed_cache_from_release()
#' }
seed_cache_from_release <- function(
  tag = "latest",
  repo = "pbulsink/f1predicter",
  asset_name = "f1predicter.sqlite",
  cache = getOption("f1predicter.cache", default = tempdir()),
  overwrite = FALSE
) {
  destination <- cache_db_path(cache = cache)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "Error in f1predicter::seed_cache_from_release(). Cache already exists at {.file {destination}}. Set {.code overwrite = TRUE} to replace it."
    )
  }

  .check_piggyback_installed()

  download_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(download_path), add = TRUE)

  piggyback::pb_download(
    file = asset_name,
    repo = repo,
    tag = tag,
    dest = download_path,
    overwrite = TRUE
  )

  .validate_cache_snapshot(download_path)
  ok <- file.copy(download_path, destination, overwrite = TRUE)
  if (!ok) {
    cli::cli_abort(
      "Error in f1predicter::seed_cache_from_release(). Failed copying downloaded snapshot to {.file {destination}}."
    )
  }

  cli::cli_inform(
    "Seeded cache database from release {.val {tag}} into {.file {destination}}."
  )

  invisible(destination)
}

#' Upload the Local Cache Snapshot to a GitHub Release
#'
#' @description
#' Validates the local SQLite cache and uploads it as a release asset using
#' `{piggyback}`.
#'
#' @param tag GitHub release tag to publish to.
#' @param repo Repository in `"owner/repo"` format.
#' @param cache Cache directory path. Defaults to
#'   `getOption("f1predicter.cache", tempdir())`.
#' @param overwrite Logical passed to `piggyback::pb_upload()`.
#'
#' @return Invisibly returns the upload result from `piggyback::pb_upload()`.
#' @export
#'
#' @examples
#' \dontrun{
#' publish_cache_snapshot(tag = "cache-2025")
#' }
publish_cache_snapshot <- function(
  tag = paste0("cache-", format(Sys.Date(), "%Y")),
  repo = "pbulsink/f1predicter",
  cache = getOption("f1predicter.cache", default = tempdir()),
  overwrite = TRUE
) {
  snapshot <- cache_db_path(cache = cache)
  if (!file.exists(snapshot)) {
    cli::cli_abort(
      "Error in f1predicter::publish_cache_snapshot(). Snapshot not found at {.file {snapshot}}."
    )
  }

  .validate_cache_snapshot(snapshot)
  .check_piggyback_installed()

  upload <- piggyback::pb_upload(
    file = snapshot,
    repo = repo,
    tag = tag,
    overwrite = overwrite
  )

  cli::cli_inform(
    "Uploaded {.file {snapshot}} to release {.val {tag}}."
  )

  invisible(upload)
}
