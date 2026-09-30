test_that("train_stacked_model() builds ensemble from all candidate engines, not just the first (#26)", {
  engines_called <- character()

  local_mocked_bindings(
    control_stack_resamples = function(...) "ctrl",
    stacks = function(...) list(),
    add_candidates = function(data_stack, candidates, name) {
      data_stack[[name]] <- candidates
      data_stack
    },
    blend_predictions = function(data_stack, ...) {
      structure(list(members = names(data_stack)), class = "model_stack")
    },
    fit_members = function(blended, ...) blended,
    autoplot = function(...) "mock plot",
    .package = "stacks"
  )

  local_mocked_bindings(
    fit_resamples = function(object, resamples, metrics, control) {
      engine <- workflows::extract_spec_parsnip(object)$engine
      engines_called[length(engines_called) + 1] <<- engine
      structure(list(engine = engine), class = "resample_results")
    },
    .package = "tune"
  )

  set.seed(4813)
  train_data <- data.frame(
    x1 = rnorm(40),
    x2 = rnorm(40),
    y = rnorm(40)
  )
  splits <- rsample::initial_split(train_data)
  folds <- rsample::vfold_cv(rsample::training(splits), v = 2)

  hyperparams <- list(
    ranger = tibble::tibble(mtry = 2, min_n = 5),
    glmnet = tibble::tibble(penalty = 0.01, mixture = 0.5)
  )

  result <- train_stacked_model(
    outcome_var = "y",
    model_name = "test ensemble",
    train_data = rsample::training(splits),
    data_split = splits,
    data_folds = folds,
    predictor_vars = c("x1", "x2"),
    hyperparams = hyperparams
  )

  expect_setequal(engines_called, c("ranger", "glmnet"))
  expect_s3_class(result, "model_stack")
  expect_setequal(result$members, c("ranger", "glmnet"))
})

test_that("train_stacked_model() calls fit_resamples once per candidate engine (#26)", {
  fit_resamples_calls <- 0L

  local_mocked_bindings(
    control_stack_resamples = function(...) "ctrl",
    stacks = function(...) list(),
    add_candidates = function(data_stack, candidates, name) {
      data_stack[[name]] <- candidates
      data_stack
    },
    blend_predictions = function(data_stack, ...) {
      structure(list(members = names(data_stack)), class = "model_stack")
    },
    fit_members = function(blended, ...) blended,
    autoplot = function(...) "mock plot",
    .package = "stacks"
  )

  local_mocked_bindings(
    fit_resamples = function(object, resamples, metrics, control) {
      fit_resamples_calls <<- fit_resamples_calls + 1L
      engine <- workflows::extract_spec_parsnip(object)$engine
      structure(list(engine = engine), class = "resample_results")
    },
    .package = "tune"
  )

  set.seed(9247)
  train_data <- data.frame(
    x1 = rnorm(40),
    x2 = rnorm(40),
    y = rnorm(40)
  )
  splits <- rsample::initial_split(train_data)
  folds <- rsample::vfold_cv(rsample::training(splits), v = 2)

  hyperparams <- list(
    ranger = tibble::tibble(mtry = 2, min_n = 5),
    glmnet = tibble::tibble(penalty = 0.01, mixture = 0.5),
    kknn = tibble::tibble(neighbors = 5)
  )

  train_stacked_model(
    outcome_var = "y",
    model_name = "test ensemble",
    train_data = rsample::training(splits),
    data_split = splits,
    data_folds = folds,
    predictor_vars = c("x1", "x2"),
    hyperparams = hyperparams
  )

  expect_equal(fit_resamples_calls, 3L)
})

test_that("oof_meta_predictions() predicts fold rows with a model that never saw them (#32)", {
  train_rows <- data.frame(
    round_id = rep(1:4, each = 2),
    driver_id = rep(c("a", "b"), 4),
    y = seq_len(8)
  )
  folds <- rsample::group_vfold_cv(train_rows, group = "round_id", v = 4)

  # Each refit records which rounds it was trained on; the returned "model" is
  # that record, so the assertion can confirm the predicting model never saw
  # the row it predicted.
  refit_fn <- function(data) sort(unique(as.character(data$round_id)))
  predict_fn <- function(model, data) {
    expect_false(any(as.character(data$round_id) %in% model))
    rep(length(model), nrow(data))
  }

  preds <- oof_meta_predictions(
    fitted_model = "full",
    refit_fn = refit_fn,
    data_folds = folds,
    new_data = train_rows,
    row_key = c("round_id", "driver_id"),
    predict_fn = predict_fn
  )

  expect_length(preds, 8L)
  expect_true(all(preds == 3))
})

test_that("oof_meta_predictions() uses the full fit for rows outside the folds (#32)", {
  train_rows <- data.frame(
    round_id = rep(1:3, each = 2),
    driver_id = rep(c("a", "b"), 3),
    y = seq_len(6)
  )
  folds <- rsample::group_vfold_cv(train_rows, group = "round_id", v = 3)

  # Rounds 8 and 9 are held-out test rounds: absent from every fold.
  new_data <- rbind(
    train_rows,
    data.frame(
      round_id = rep(8:9, each = 2),
      driver_id = rep(c("a", "b"), 2),
      y = 101:104
    )
  )

  preds <- oof_meta_predictions(
    fitted_model = "full",
    refit_fn = function(data) "fold",
    data_folds = folds,
    new_data = new_data,
    row_key = c("round_id", "driver_id"),
    predict_fn = function(model, data) {
      rep(if (identical(model, "full")) 1 else 0, nrow(data))
    }
  )

  expect_identical(preds[1:6], rep(0, 6))
  expect_identical(preds[7:10], rep(1, 4))
})

test_that("oof_meta_predictions() matches rows by key when new_data is filtered (#32)", {
  train_rows <- data.frame(
    round_id = rep(1:3, each = 2),
    driver_id = rep(c("a", "b"), 3),
    y = seq_len(6)
  )
  folds <- rsample::group_vfold_cv(train_rows, group = "round_id", v = 3)

  filtered <- train_rows[train_rows$driver_id == "a", ]

  preds <- oof_meta_predictions(
    fitted_model = "full",
    refit_fn = function(data) "fold",
    data_folds = folds,
    new_data = filtered,
    row_key = c("round_id", "driver_id"),
    predict_fn = function(model, data) {
      rep(if (identical(model, "full")) 1 else 0, nrow(data))
    }
  )

  # Every filtered row still belongs to a fold, so none should fall through to
  # the full fit.
  expect_identical(preds, rep(0, 3))
})

test_that("oof_meta_predictions() errors when the row key is missing (#32)", {
  train_rows <- data.frame(round_id = rep(1:2, each = 2), y = seq_len(4))
  folds <- rsample::group_vfold_cv(train_rows, group = "round_id", v = 2)

  expect_error(
    oof_meta_predictions(
      fitted_model = "full",
      refit_fn = function(data) "fold",
      data_folds = folds,
      new_data = data.frame(other = 1:4),
      row_key = c("round_id", "driver_id"),
      predict_fn = function(model, data) rep(0, nrow(data))
    ),
    "missing from"
  )
})

test_that("train_stacked_model() suppresses the weight plot when quiet (#32)", {
  plotted <- 0L

  local_mocked_bindings(
    control_stack_resamples = function(...) "ctrl",
    stacks = function(...) list(),
    add_candidates = function(data_stack, candidates, name) {
      data_stack[[name]] <- candidates
      data_stack
    },
    blend_predictions = function(data_stack, ...) {
      structure(list(members = names(data_stack)), class = "model_stack")
    },
    fit_members = function(blended, ...) blended,
    autoplot = function(...) {
      plotted <<- plotted + 1L
      "mock plot"
    },
    .package = "stacks"
  )
  local_mocked_bindings(
    fit_resamples = function(object, resamples, metrics, control) {
      structure(
        list(engine = workflows::extract_spec_parsnip(object)$engine),
        class = "resample_results"
      )
    },
    .package = "tune"
  )

  set.seed(7712)
  train_data <- data.frame(x1 = rnorm(40), x2 = rnorm(40), y = rnorm(40))
  splits <- rsample::initial_split(train_data)
  folds <- rsample::vfold_cv(rsample::training(splits), v = 2)
  hyperparams <- list(ranger = tibble::tibble(mtry = 2, min_n = 5))

  train_stacked_model(
    outcome_var = "y",
    model_name = "quiet ensemble",
    train_data = rsample::training(splits),
    data_split = splits,
    data_folds = folds,
    predictor_vars = c("x1", "x2"),
    hyperparams = hyperparams,
    quiet = TRUE
  )
  expect_identical(plotted, 0L)

  train_stacked_model(
    outcome_var = "y",
    model_name = "loud ensemble",
    train_data = rsample::training(splits),
    data_split = splits,
    data_folds = folds,
    predictor_vars = c("x1", "x2"),
    hyperparams = hyperparams,
    quiet = FALSE
  )
  expect_identical(plotted, 1L)
})
