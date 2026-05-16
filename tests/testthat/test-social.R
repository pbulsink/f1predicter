# ---- Helpers ----------------------------------------------------------------

make_race_preds <- function() {
  n <- 5
  probs_mat <- matrix(
    c(
      0.50,
      0.30,
      0.10,
      0.05,
      0.05,
      0.20,
      0.40,
      0.20,
      0.10,
      0.10,
      0.10,
      0.20,
      0.40,
      0.20,
      0.10,
      0.05,
      0.10,
      0.20,
      0.40,
      0.25,
      0.05,
      0.05,
      0.15,
      0.25,
      0.50
    ),
    nrow = n,
    ncol = n,
    byrow = TRUE
  )
  # Column names must be character integers so as.data.frame() produces
  # columns "1", "2", ... as required by format_results_prob_table().
  colnames(probs_mat) <- as.character(seq_len(n))
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n)]),
    round = "1",
    season = "2024",
    win_odd = c(0.50, 0.20, 0.10, 0.05, 0.05),
    podium_odd = c(0.80, 0.60, 0.40, 0.20, 0.10),
    t10_odd = c(0.95, 0.90, 0.85, 0.75, 0.65),
    likely_position = c(1.2, 2.1, 3.5, 4.2, 5.1),
    likely_position_class = c(1, 2, 3, 4, 5),
    .probs = probs_mat
  )
}

make_quali_preds <- function() {
  n <- 5
  probs_mat <- matrix(
    c(
      0.50,
      0.30,
      0.10,
      0.05,
      0.05,
      0.20,
      0.40,
      0.20,
      0.10,
      0.10,
      0.10,
      0.20,
      0.40,
      0.20,
      0.10,
      0.05,
      0.10,
      0.20,
      0.40,
      0.25,
      0.05,
      0.05,
      0.15,
      0.25,
      0.50
    ),
    nrow = n,
    ncol = n,
    byrow = TRUE
  )
  colnames(probs_mat) <- as.character(seq_len(n))
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n)]),
    round = "1",
    season = "2024",
    pole_odd = c(0.50, 0.20, 0.10, 0.05, 0.05),
    likely_quali_position = c(1.2, 2.1, 3.5, 4.2, 5.1),
    likely_quali_position_class = c(1, 2, 3, 4, 5),
    .probs = probs_mat
  )
}

mock_drivers <- function(n = 5) {
  tibble::tibble(
    driver_id = paste0("driver_", letters[seq_len(n)]),
    given_name = paste0("Given", LETTERS[seq_len(n)]),
    family_name = paste0("Family", LETTERS[seq_len(n)])
  )
}

# ---- get_driver_name() ------------------------------------------------------

test_that("get_driver_name() returns full names from f1dataR lookup (#noissue)", {
  local_mocked_bindings(
    load_drivers = function(season) {
      tibble::tibble(
        driver_id = c("driver_a", "driver_b"),
        given_name = c("Alice", "Bob"),
        family_name = c("Alpha", "Beta")
      )
    },
    .package = "f1dataR"
  )
  result <- f1predicter:::get_driver_name(2024, c("driver_a", "driver_b"))
  expect_equal(result, c("Alice Alpha", "Bob Beta"))
})

test_that("get_driver_name() falls back to title-case when f1dataR errors (#noissue)", {
  local_mocked_bindings(
    load_drivers = function(season) stop("API unavailable"),
    .package = "f1dataR"
  )
  expect_warning(
    result <- f1predicter:::get_driver_name(
      2024,
      c("max_verstappen", "lewis_hamilton")
    ),
    "Could not load drivers"
  )
  expect_equal(result, c("Max Verstappen", "Lewis Hamilton"))
})

test_that("get_driver_name() falls back to title-case for unknown driver IDs (#noissue)", {
  local_mocked_bindings(
    load_drivers = function(season) {
      tibble::tibble(
        driver_id = "known_driver",
        given_name = "Known",
        family_name = "Driver"
      )
    },
    .package = "f1dataR"
  )
  result <- f1predicter:::get_driver_name(2024, c("known_driver", "unknown_id"))
  expect_equal(result, c("Known Driver", "Unknown Id"))
})

# ---- get_race_name() --------------------------------------------------------

test_that("get_race_name() returns the race name for a known season/round (#noissue)", {
  result <- f1predicter:::get_race_name("2024", "1")
  expect_equal(result, "Bahrain Grand Prix")
})

test_that("get_race_name() returns a fallback string for unknown season/round (#noissue)", {
  result <- f1predicter:::get_race_name("9999", "99")
  expect_equal(result, "Race 99")
})

# ---- format_results_prob_table() --------------------------------------------

test_that("format_results_prob_table() errors when .probs column is missing (#noissue)", {
  skip_if_not_installed("gt")
  preds <- make_race_preds()
  preds$.probs <- NULL
  expect_error(format_results_prob_table(preds), "\\.probs")
})

test_that("format_results_prob_table() returns a gt_tbl for valid predictions (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  result <- format_results_prob_table(make_race_preds())
  expect_s3_class(result, "gt_tbl")
})

test_that("format_results_prob_table() with save_image=TRUE returns a list with a filename (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- format_results_prob_table(make_race_preds(), save_image = TRUE)
  expect_type(result, "list")
  expect_named(result, c("prob_table", "filename"))
  expect_s3_class(result$prob_table, "gt_tbl")
  expect_type(result$filename, "character")
})

# ---- format_quali_prob_table() -----------------------------------------------

test_that("format_quali_prob_table() errors when .probs column is missing (#noissue)", {
  skip_if_not_installed("gt")
  preds <- make_quali_preds()
  preds$.probs <- NULL
  expect_error(format_quali_prob_table(preds), "\\.probs")
})

test_that("format_quali_prob_table() returns a gt_tbl for valid predictions (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  result <- format_quali_prob_table(make_quali_preds())
  expect_s3_class(result, "gt_tbl")
})

test_that("format_quali_prob_table() with save_image=TRUE returns a list with a filename (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- format_quali_prob_table(make_quali_preds(), save_image = TRUE)
  expect_type(result, "list")
  expect_named(result, c("prob_table", "filename"))
  expect_s3_class(result$prob_table, "gt_tbl")
  expect_type(result$filename, "character")
})

# ---- format_results_odds_table() --------------------------------------------

test_that("format_results_odds_table() errors when .probs column is missing (#noissue)", {
  skip_if_not_installed("gt")
  preds <- make_race_preds()
  preds$.probs <- NULL
  expect_error(format_results_odds_table(preds), "\\.probs")
})

test_that("format_results_odds_table() returns a gt_tbl for valid predictions (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  result <- format_results_odds_table(make_race_preds())
  expect_s3_class(result, "gt_tbl")
})

test_that("format_results_odds_table() with save_image=TRUE returns a list with a filename (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- format_results_odds_table(make_race_preds(), save_image = TRUE)
  expect_type(result, "list")
  expect_named(result, c("prob_table", "filename"))
  expect_s3_class(result$prob_table, "gt_tbl")
  expect_type(result$filename, "character")
})

# ---- format_race_skeet_predictions() ----------------------------------------

test_that("format_race_skeet_predictions() returns a 3-element list (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_race_skeet_predictions(make_race_preds())
  expect_length(result, 3L)
})

test_that("format_race_skeet_predictions() first skeet has text, tags, image, and image_alt (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_race_skeet_predictions(make_race_preds())
  skeet1 <- result[[1]]
  expect_named(
    skeet1,
    c("text", "tags", "image", "image_alt"),
    ignore.order = TRUE
  )
  expect_in("F1", skeet1$tags)
  expect_in("F1Predictions", skeet1$tags)
  expect_match(skeet1$text, "Bahrain Grand Prix")
})

test_that("format_race_skeet_predictions() third skeet has text and image fields (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_race_skeet_predictions(make_race_preds())
  skeet3 <- result[[3]]
  expect_named(skeet3, c("text", "image", "image_alt"), ignore.order = TRUE)
  expect_match(skeet3$image_alt, "Bahrain Grand Prix")
})

# ---- format_quali_skeet_predictions() ---------------------------------------

test_that("format_quali_skeet_predictions() returns a 2-element list (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_quali_skeet_predictions(make_quali_preds())
  expect_length(result, 2L)
})

test_that("format_quali_skeet_predictions() first skeet has text and tags (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_quali_skeet_predictions(make_quali_preds())
  skeet1 <- result[[1]]
  expect_named(skeet1, c("text", "tags"), ignore.order = TRUE)
  expect_in("F1", skeet1$tags)
  expect_in("F1Quali", skeet1$tags)
  expect_match(skeet1$text, "Bahrain Grand Prix")
})

test_that("format_quali_skeet_predictions() second skeet has image and image_alt fields (#noissue)", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  result <- f1predicter:::format_quali_skeet_predictions(make_quali_preds())
  skeet2 <- result[[2]]
  expect_named(skeet2, c("text", "image", "image_alt"), ignore.order = TRUE)
  expect_match(skeet2$image_alt, "Bahrain Grand Prix")
})

# ---- post_skeet_predictions() -----------------------------------------------

test_that("post_skeet_predictions() errors when input is not a list of lists (#noissue)", {
  expect_error(
    f1predicter:::post_skeet_predictions("not a list"),
    "must be a list of lists"
  )
  expect_error(
    f1predicter:::post_skeet_predictions(list("a", "b")),
    "must be a list of lists"
  )
})

test_that("post_skeet_predictions() posts all skeets successfully with mocked atrrr (#noissue)", {
  skip_if_not_installed("atrrr")
  post_count <- 0L
  local_mocked_bindings(
    post_skeet = function(
      text,
      image = NULL,
      image_alt = NULL,
      tags = NULL,
      in_reply_to = NULL
    ) {
      post_count <<- post_count + 1L
      list(uri = paste0("at://fake/uri/", post_count))
    },
    .package = "atrrr"
  )
  skeets <- list(
    list(text = "First skeet", tags = c("F1")),
    list(text = "Second skeet")
  )
  result <- f1predicter:::post_skeet_predictions(skeets)
  expect_length(result, 2L)
  expect_equal(post_count, 2L)
  expect_equal(result[[1]]$uri, "at://fake/uri/1")
  expect_equal(result[[2]]$uri, "at://fake/uri/2")
})

test_that("post_skeet_predictions() aborts thread when a post fails (#noissue)", {
  skip_if_not_installed("atrrr")
  local_mocked_bindings(
    post_skeet = function(text, ...) stop("posting failed"),
    .package = "atrrr"
  )
  skeets <- list(
    list(text = "First skeet"),
    list(text = "Should not be posted")
  )
  result <- suppressWarnings(f1predicter:::post_skeet_predictions(skeets))
  # Thread is aborted after the first failure, so result is empty
  expect_length(result, 0L)
})


# ---- post_quali_predictions() / post_race_predictions() ---------------------

test_that("post_quali_predictions() formats and posts qualifying predictions (#noissue)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("atrrr")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  post_count <- 0L
  local_mocked_bindings(
    post_skeet = function(
      text,
      image = NULL,
      image_alt = NULL,
      tags = NULL,
      in_reply_to = NULL
    ) {
      post_count <<- post_count + 1L
      list(uri = paste0("at://fake/uri/", post_count))
    },
    .package = "atrrr"
  )
  result <- post_quali_predictions(make_quali_preds())
  expect_equal(post_count, 2L)
  expect_length(result, 2L)
})

test_that("post_race_predictions() formats and posts race predictions (#noissue)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("atrrr")
  local_mocked_bindings(
    load_drivers = function(season) mock_drivers(),
    .package = "f1dataR"
  )
  local_mocked_bindings(
    save_gt_as_png_ragg = function(gt_table, filename, ...) invisible(filename)
  )
  post_count <- 0L
  local_mocked_bindings(
    post_skeet = function(
      text,
      image = NULL,
      image_alt = NULL,
      tags = NULL,
      in_reply_to = NULL
    ) {
      post_count <<- post_count + 1L
      list(uri = paste0("at://fake/uri/", post_count))
    },
    .package = "atrrr"
  )
  result <- post_race_predictions(make_race_preds())
  expect_equal(post_count, 3L)
  expect_length(result, 3L)
})
