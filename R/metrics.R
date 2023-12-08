# Metric for win accuracy

win_impl <- function(truth, estimate, case_weights = NULL){
  if(is.factor(truth)){
    truth <- as.numeric(as.character(truth))
  }
  if(is.factor(estimate)){
    estimate <- as.numeric(as.character(estimate))
  }

  #make sure both estimates and truths are just between
  stopifnot(max(truth) != 1 & min(truth) != 0)
  stopifnot(max(estimate) <= 1 & min(estimate) >= 0)
  stopifnot(length(truth) == length(estimate))

  #optional: binary-code estimate
  estimate <- round(estimate)

  #Determine matches
  winners<-truth[truth == 1]
  estimated_winners<-estimate[truth == 1]
  acc <- sum(winners == estimated_winners)/length(winners)
  return(acc)
}

#' Win metric
#'
#' Subsets the normal accuracy metric to only count the target position. For example, picking all 0 for win chance would
#' normally give a 95% accuracy (1 in 20 is wrong - 1 in 20 wins). This function only looks at the accuracy relative
#' to the truth == 1 case (i.e. correctly or not pick the winner). Works for win or for podium or points models
#'
#' @param truth truth value (0 or 1)
#' @param estimate estimates (note that they're limited to 0..1 and are rounded in the function to either 0 or 1)
#' @param case_weights ignored
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [win()] for data.frame version
win_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  win_impl(truth = truth, estimate = estimate, case_weights = case_weights)
}


#' Win metric
#'
#' Subsets the normal accuracy metric to only count the target position. For example, picking all 0 for win chance would
#' normally give a 95% accuracy (1 in 20 is wrong - 1 in 20 wins). This function only looks at the accuracy relative
#' to the truth == 1 case (i.e. correctly or not pick the winner). Works for win or for podium or points models
#'
#' @param data data
#' @param ... other elements
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [win_vec()] for vector version
win <- function(data, ...) {
  UseMethod("win")
}

win <- yardstick::new_numeric_metric(win, direction = "maximize")

#' @export
win.data.frame <- function(data, truth, estimate, na_rm = TRUE,  case_weights = NULL, ...) {

  yardstick::numeric_metric_summarizer(
    name = "win",
    fn = win_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    case_weights = !! enquo(case_weights),
    ...
  )
}

