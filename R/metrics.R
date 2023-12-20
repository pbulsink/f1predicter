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

#' @rdname pacc
#' @export
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
#' @param ... other elements, passed to yardstick::prob_metric_summarizer()
#'
#' @return a numeric value between 0 and 1 (higher is better)
#' @export
#' @seealso [pacc_vec()] for vector version
pacc <- function(data, ...) {
  UseMethod("pacc")
}

pacc <- yardstick::new_prob_metric(pacc, direction = "maximize")

#' @rdname pacc
#' @export
pacc.data.frame <- function(data, truth, estimate, ..., event_level = yardstick::yardstick_event_level(),
                            na_rm = TRUE, case_weights = NULL) {

  #replace numeric estimates with 1/0 "class-like" estimates to match the truth class.
  # All this shit should be in impl
  # elucidate truth as win/podium/10 from the number of truths.
  #

  if (truth == 'win') {
    data <- data %>%
      dplyr::group_by(.data$round_id) %>%
      dplyr::mutate(!!rlang::sym(estimate) := ifelse(!!rlang::sym(estimate) == max(!!rlang::sym(estimate), na.rm = T), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!rlang::sym(estimate) := factor(!!rlang::sym(estimate), levels = c(1,0))) %>%
      as.data.frame()
  } else if (truth == 'podium') {
    data <- data %>%
      dplyr::group_by(.data$round_id) %>%
      dplyr::mutate(!!rlang::sym(estimate) := ifelse(!!rlang::sym(estimate) >= dplyr::nth(!!rlang::sym(estimate), n = 3, order_by = order(!!rlang::sym(estimate))), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!rlang::sym(estimate) := factor(!!rlang::sym(estimate), levels = c(1,0))) %>%
      as.data.frame()
  } else if (truth == 't10') {
    data <- data %>%
      dplyr::group_by(.data$round_id) %>%
      dplyr::mutate(!!rlang::sym(estimate) := ifelse(!!rlang::sym(estimate) >= dplyr::nth(!!rlang::sym(estimate), n = 3, order_by = order(!!rlang::sym(estimate))), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!rlang::sym(estimate) := factor(!!rlang::sym(estimate), levels = c(1,0))) %>%
      as.data.frame()
  } else {
    data <- data %>%
      dplyr::mutate(!!rlang::sym(estimate) := ifelse(!!rlang::sym(estimate) > 0.5, 1, 0)) %>%
      dplyr::mutate(!!rlang::sym(estimate) := factor(!!rlang::sym(estimate), levels = c(1,0))) %>%
      as.data.frame()
  }

  round_ids<-unique(data$round_id)

  if(length(round_ids) > 1){
    pacc<-sapply(round_ids, function(x){pacc_vec(truth = data[data$round_id == x,truth],
                                                 estimate = data[data$round_id == x, estimate])})
    pacc<-mean(pacc, na.rm = T)
  } else {
    pacc<-pacc_vec(truth = data[,truth],
                   estimate = data[, estimate])
  }
  return(dplyr::tibble(.metric = 'pacc', .estimator = 'binary', .estimate = pacc))
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
