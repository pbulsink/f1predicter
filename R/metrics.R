# Metric for win accuracy

pacc_impl <- function(truth, estimate, case_weights = NULL){
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
  pacc <- sum(winners == estimated_winners)/length(winners)
  return(pacc)
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
#' @param ... unused
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [pacc()] for data.frame version
pacc_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  pacc_impl(truth = truth, estimate = estimate, case_weights = case_weights)
}


#' @export
pacc <- function(data, ...) {
  UseMethod("pacc")
}


pacc <- yardstick::new_numeric_metric(pacc, direction = "maximize")

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

  yardstick::numeric_metric_summarizer(
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

