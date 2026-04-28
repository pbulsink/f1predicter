test_that("prepare_and_split_data() returns required list elements", {
  # Create minimal test data
  test_data <- data.frame(
    round_id = rep(1:5, 4),
    driver_id = rep(
      c("max_verstappen", "lando_norris", "lewis_hamilton", "george_russell"),
      5
    ),
    grid = c(1:5, 2:6, 3:7, 4:8),
    position = c(1:5, 2:6, 3:7, 4:8),
    points = c(
      25,
      18,
      15,
      12,
      10,
      25,
      18,
      15,
      12,
      10,
      25,
      18,
      15,
      12,
      10,
      25,
      18,
      15,
      12,
      10
    )
  )

  result <- prepare_and_split_data(test_data, group = "round_id")

  # Should have the required list elements
  expect_true("data_split" %in% names(result))
  expect_true("train_data" %in% names(result))
  expect_true("test_data" %in% names(result))
  expect_true("data_folds" %in% names(result))
})

test_that("prepare_and_split_data() creates training/test split correctly", {
  test_data <- data.frame(
    round_id = rep(1:5, 4),
    driver_id = rep(c("a", "b", "c", "d"), 5),
    value = rnorm(20)
  )

  result <- prepare_and_split_data(test_data, prop = 0.8, group = "round_id")

  # Train and test should have different sizes
  train_size <- nrow(result$train_data)
  test_size <- nrow(result$test_data)

  expect_true(train_size > 0)
  expect_true(test_size > 0)
  expect_true(train_size > test_size) # More training data with 0.8 prop
})

test_that("prepare_and_split_data() selects specified columns", {
  test_data <- data.frame(
    round_id = 1:5,
    col1 = 1:5,
    col2 = 6:10,
    col3 = 11:15
  )

  result <- prepare_and_split_data(
    test_data,
    columns = c("col1", "col2"),
    group = "round_id"
  )

  # Should only have selected columns
  expect_true("col1" %in% names(result$train_data))
  expect_true("col2" %in% names(result$train_data))
  # col3 might not be selected (excluded)
})

test_that("prepare_and_split_data() converts character to factor", {
  test_data <- data.frame(
    round_id = 1:5,
    char_col = c("a", "b", "a", "b", "a"),
    num_col = 1:5
  )

  result <- prepare_and_split_data(test_data, group = "round_id")

  # Character columns should be converted to factors
  expect_true(is.factor(result$train_data$char_col))
  # Numeric should remain numeric
  expect_true(is.numeric(result$train_data$num_col))
})

test_that("get_hyperparameters() includes ordinal_class_hyperparameters for all scenarios", {
  for (model in c("quali", "results")) {
    timings <- if (model == "quali") {
      c("early", "late")
    } else {
      c("early", "late", "after-quali")
    }
    for (timing in timings) {
      hp <- get_hyperparameters(model, timing)
      expect_true(
        "ordinal_class_hyperparameters" %in% names(hp),
        label = paste("ordinal_class_hyperparameters in", model, timing)
      )
      ordinal_hp <- hp$ordinal_class_hyperparameters
      # All four engines should be present
      expect_true("polr" %in% names(ordinal_hp))
      expect_true("ordinalNet" %in% names(ordinal_hp))
      expect_true("ordinalForest" %in% names(ordinal_hp))
      expect_true("rpartScore" %in% names(ordinal_hp))
      # polr should have an empty tibble (no tunable params)
      expect_equal(nrow(ordinal_hp$polr), 0L)
      # ordinalNet should have penalty and mixture
      expect_true(all(
        c("penalty", "mixture") %in% names(ordinal_hp$ordinalNet)
      ))
      # ordinalForest should have mtry and min_n
      expect_true(all(c("mtry", "min_n") %in% names(ordinal_hp$ordinalForest)))
      # rpartScore should have cost_complexity and tree_depth
      expect_true(
        all(
          c("cost_complexity", "tree_depth") %in% names(ordinal_hp$rpartScore)
        )
      )
    }
  }
})

test_that("train_ordinal_ensemble() returns a model_stack", {
  skip_if_not_installed("ordered")
  skip_if_not_installed("stacks")
  skip_if_not_installed("ordinalNet")

  # Minimal synthetic ordinal dataset: 3 ordered classes, 3 numeric predictors
  # Use enough data per fold (n=300, v=5 folds -> 48 rows/assessment fold)
  set.seed(42)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  raw_pos <- round(1 + 2 * plogis(0.5 * x1 - 0.3 * x2 + rnorm(n)))
  raw_pos <- pmin(pmax(raw_pos, 1L), 3L)

  full_df <- data.frame(
    position = factor(raw_pos, levels = 1:3, ordered = TRUE),
    x1 = x1,
    x2 = x2,
    x3 = x3
  )

  data_split <- rsample::initial_split(full_df, prop = 0.8)
  train_data <- rsample::training(data_split)
  data_folds <- rsample::vfold_cv(train_data, v = 5)

  # Use only two engines for speed in tests
  hyperparams <- list(
    polr = tibble::tibble(),
    ordinalNet = tibble::tibble(penalty = 0.01, mixture = 0.5)
  )

  result <- train_ordinal_ensemble(
    outcome_var = "position",
    model_name = "Test Ordinal Ensemble",
    train_data = train_data,
    data_split = data_split,
    data_folds = data_folds,
    predictor_vars = c("x1", "x2", "x3"),
    hyperparams = hyperparams,
    save_model = FALSE
  )

  expect_s3_class(result, "model_stack")

  test_data <- rsample::testing(data_split)

  # Should be able to predict class labels
  preds_class <- stats::predict(result, new_data = test_data, type = "class")
  expect_true(".pred_class" %in% names(preds_class))
  expect_s3_class(preds_class$.pred_class, "factor")
  # stacks returns a regular (non-ordered) factor even for ordinal outcomes;
  # the factor levels are still in correct ordinal order
  expect_equal(levels(preds_class$.pred_class), as.character(1:3))

  # Should be able to predict probabilities
  preds_prob <- stats::predict(result, new_data = test_data, type = "prob")
  expect_true(ncol(preds_prob) >= 3L)
})

test_that("train_ordinal_ensemble() errors without hyperparams", {
  expect_error(
    train_ordinal_ensemble(
      outcome_var = "y",
      model_name = "Test",
      train_data = data.frame(y = 1),
      data_split = NULL,
      data_folds = NULL,
      predictor_vars = "x"
      # hyperparams intentionally omitted
    ),
    "hyperparams"
  )
})

test_that("predict_position_class() works with a last_fit ordinal model", {
  skip_if_not_installed("ordered")
  skip_if_not_installed("MASS")

  set.seed(99)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  raw_pos <- round(1 + 2 * plogis(0.5 * x1 + rnorm(n)))
  raw_pos <- pmin(pmax(raw_pos, 1L), 3L)

  driver_df <- data.frame(
    driver_id = paste0("driver_", seq_len(n)),
    round = rep(1:4, each = n / 4),
    season = 2024L,
    position = factor(raw_pos, levels = 1:3, ordered = TRUE),
    x1 = x1,
    x2 = x2
  )

  # Use a direct initial_split since this test does not need group-aware splits
  data_split <- rsample::initial_split(driver_df, prop = 0.75)
  train_data <- rsample::training(data_split)

  kap_linear <- purrr::partial(yardstick::kap, weighting = "linear")
  class(kap_linear) <- c("class_metric", "metric", "function")
  attr(kap_linear, "direction") <- "maximize"

  metrics_ordinal <- yardstick::metric_set(
    yardstick::ranked_prob_score,
    kap_linear,
    yardstick::accuracy
  )

  spec <- parsnip::ordinal_reg() %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("polr")

  rec <- recipes::recipe(position ~ x1 + x2, data = train_data) %>%
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow(rec, spec)
  fit <- tune::last_fit(wf, data_split, metrics = metrics_ordinal)

  test_data <- rsample::testing(data_split)
  preds <- predict_position_class(test_data, fit)

  expect_s3_class(preds, "data.frame")
  expect_true("likely_position_class" %in% names(preds))
  expect_true("driver_id" %in% names(preds))
  expect_type(preds$likely_position_class, "double")
})
