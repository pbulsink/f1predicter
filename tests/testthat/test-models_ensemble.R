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
