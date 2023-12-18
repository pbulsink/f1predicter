# Metric for win accuracy

event_col <- function(xtab, event_level) {
  if (identical(event_level, "first")) {
    colnames(xtab)[[1]]
  } else {
    colnames(xtab)[[2]]
  }
}


pacc_impl <- function(truth, estimate, event_level){
  xtab <- table(estimate, truth)
  col <- event_col(xtab, event_level)
  col2 <- setdiff(colnames(xtab), col)

  tp <- xtab[col, col]
  fn <- xtab[col2, col]

  tp / (fn + tp)
}

#' Positive Accuracy metric
#'
#' Subsets the normal accuracy metric to only count the target/positive position(s). For example, picking all 0 for win
#' chance would normally give a 95% accuracy (1 in 20 is wrong - 1 in 20 wins). This function only looks at the accuracy
#' relative to the truth == 1 case (i.e. correctly or not pick the winner). Works for win or for podium or points models.
#'
#' Idea inspired by https://towardsdatascience.com/formula-1-race-predictor-5d4bfae887da
#'
#' @param truth truth value (0 or 1)
#' @param estimate estimates (note that they're limited to 0..1 and are rounded in the function to either 0 or 1)
#' @param na_rm whether to remove NA cases - not used
#' @param case_weights ignored, required for yardstick metrics
#' @param event_level Event level = normally th first level in truth is assumed to be the event case, can change here.
#' @param ... unused
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [pacc()] for data.frame version
pacc_vec <- function(truth,
                     estimate,
                     estimator = NULL,
                     na_rm = TRUE,
                     case_weights = NULL,
                     event_level = "first",
                     ...) {
  estimator <- yardstick::finalize_estimator(truth, estimator, metric_class = "pacc")

  yardstick::check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  pacc_impl(truth = truth, estimate = estimate, event_level = event_level)
}


#' @export
pacc <- function(data, ...) {
  UseMethod("pacc")
}


pacc <- yardstick::new_class_metric(pacc, direction = "maximize")

#' Positive Accuracy metric
#'
#' Subsets the normal accuracy metric to only count the target position(s) (i.e. truth == 1). For example, picking all
#' 0 for win chance would normally give a 95% accuracy (1 in 20 is wrong - 1 in 20 wins). This function only looks at
#' the accuracy relative to the truth == 1 case (i.e. correctly or not pick the winner). Works for win or for podium
#' or points models
#'
#' Idea inspired by https://towardsdatascience.com/formula-1-race-predictor-5d4bfae887da
#'
#' @param data data
#' @param truth name of the data.frame column containing truth
#' @param estimate name of the data.frame column containing the estimates.
#' @param na_rm whether to remove NA values
#' @param case_weights Not used, required for yardstick metrics
#' @param ... other elements, passed to yardstick::numeric_metric_summarizer
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [pacc_vec()] for vector version
pacc.data.frame <- function(data, truth, estimate, na_rm = TRUE,  case_weights = NULL, ...) {

  yardstick::class_metric_summarizer(
    name = "pacc",
    fn = pacc_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    case_weights = !! enquo(case_weights),
    ...
  )
}

finalize_estimator_internal.pacc <- function(metric_dispatcher, x, estimator, call) {

  yardstick::validate_estimator(estimator, estimator_override = "binary")
  if (!is.null(estimator)) {
    return(estimator)
  }

  lvls <- levels(x)
  if (length(lvls) > 2) {
    stop("A multiclass `truth` input was provided, but only `binary` is supported.")
  }
  "binary"
}
