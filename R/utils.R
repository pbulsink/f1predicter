# Return a weighted mean, filling the supplied vector to length `ln` with value `val` before calculating it.
# Weighted mean is weighted on cubed root of 1:`ln`
wmean <- function(x, ln = 20, val = 0) {
  ln_origin <- length(x)
  if (length(x) < ln) {
    x <- c(rep(val, ln - length(x)), x)
  }
  return(stats::weighted.mean(x, w = cubert(1:length(x))))
}

#return the cumulative weighted mean (with weights as log(1:length(x)))
cumwmean <- function(x, na.val = 0) {
  x[is.na(x)] <- na.val
  return(cumsum(x * log(1:length(x))) / cumsum(log(1:length(x))))
}

# get the cube root
cubert <- function(x) {
  return(nroot(x, 3))
}

#get a nth root
nroot <- function(x, n) {
  nr <- function(x, n) {
    return(x^(1 / n))
  }
  nr_V <- Vectorize(nr, "x")
  return(nr_V(x, n))
}

cumwmean_expanded <- function(x, ln = 10, val = 0, na.val = val) {
  ln_origin <- length(x)

  x <- c(rep(val, ln), x)
  cwm <- cumwmean(x, na.val = na.val)

  return(cwm[(ln + 1):length(cwm)])
  # if(length(x)<ln){ x <- c(rep(val, ln-length(x)), x) } cwm<-cumwmean(x) if(ln_origin < ln){ return(cwm[(ln-ln_origin+1):ln]) } else { return(cwm) }
}

lagged_cumwmean_expanded <- function(x, ln = 20, val = 0, na.val = val) {
  cwme <- cumwmean_expanded(x, ln, val, na.val = na.val)
  return(c(val, cwme[-length(cwme)]))
}

s_lagged_cumwmean_expanded <- function(x, ln = 20, val = 0, na.val = val) {
  lce <- lagged_cumwmean_expanded(x, ln = ln, val = val, na.val = na.val)
  return(lce[length(lce)])
}

expand_val <- function(v, ln, val = NA) {
  if (length(v) < ln) {
    v <- c(v, rep(val, ln - length(v)))
  } else if (length(v) > ln) {
    v <- v[1:ln]
  }
  return(v)
}

ensure_tidy <- function(data) {
  if (is.null(data) | all(is.na(data))) {
    return(data)
  }
  data <- data %>% janitor::clean_names()
  if ('race' %in% colnames(data)) {
    data <- data %>%
      dplyr::rename('round' = 'race')
  }
  return(data)
}

wmean_two <- function(newval, x, ln) {
  wt <- function(newval, x, ln) {
    x <- c(rep(x, ln - 1), newval)
    cwm <- cumwmean(x)

    return(cwm[length(cwm)])
  }
  wt_v <- Vectorize(wt, vectorize.args = c('newval', 'x'))
  return(wt_v(newval, x, ln))
}

#' Normalize a Numeric Vector
#'
#' @description
#' This function takes a numeric vector and scales its values so that they sum
#' to 1. `NA` values are treated as 0.
#'
#' @details
#' If the sum of the vector is 0 (e.g., it contains only zeros and `NA`s),
#' the function will return a vector of uniform values, where each element is
#' `1 / length(x)`. Otherwise, each element is divided by the total sum of the
#' vector after `NA`s have been converted to 0.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector of the same length as `x` where the elements sum to 1.
#'
#' @examples
#' normalize_vector(c(1, 2, 3, 4))
#' normalize_vector(c(10, 10))
#' normalize_vector(c(1, 2, NA, 3))
#' normalize_vector(c(0, 0, 0))
#' normalize_vector(c(NA, NA))
normalize_vector <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }
  if (length(x) == 0) {
    return(numeric(0))
  }
  total <- sum(x, na.rm = TRUE)
  if (total == 0) {
    return(rep(1 / length(x), length(x)))
  } else {
    return(x / total)
  }
}
