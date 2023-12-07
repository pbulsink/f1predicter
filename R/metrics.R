# Metric for win accuracy

win_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  win_metric <- function(truth, estimate){
    stopifnot(length(truth) == length(estimate))
    winners<-truth[truth == 1]
    estimated_winners<-estimate[truth == 1]
    return(yardstick::accuracy_vec(winners, estimated_winners))
  }

  yardstick::metric_vec_template(
    metric_impl = win_metric,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    ...
  )
}

win <- function(data, ...) {
  UseMethod("win")
}

win <- new_numeric_metric(mse, direction = "maximize")

win.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "win",
    metric_fn = win_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

# Metric for Podium Accuracy

podium_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  podium_metric <- function(truth, estimate){
    stopifnot(length(truth) == length(estimate))
    podium<-ifelse(truth <= 3, 1, 0)
    estimated_podium<-estimate[felse(truth <= 3, 1, 0) == 1]
    return(yardstick::accuracy_vec(podium, estimated_podium))
  }

  yardstick::metric_vec_template(
    metric_impl = podium_metric,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    ...
  )
}

podium <- function(data, ...) {
  UseMethod("win")
}

podium <- new_numeric_metric(mse, direction = "maximize")

podium.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "podium",
    metric_fn = podium_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    ...
  )
}
