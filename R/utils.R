#' Weighted Mean with Padding
#'
#' @description
#' Returns a weighted mean, filling the supplied vector to length `ln` with
#' value `val` before calculating. Weights are the cube root of `1:ln`.
#'
#' @param x A numeric vector.
#' @param ln Target length to pad `x` to.
#' @param val Fill value for padding.
#' @return A single numeric weighted mean.
#' @noRd
wmean <- function(x, ln = 20, val = 0) {
  ln_origin <- length(x)
  if (length(x) < ln) {
    x <- c(rep(val, ln - length(x)), x)
  }
  return(stats::weighted.mean(x, w = cubert(1:length(x))))
}

#' Cumulative Weighted Mean
#'
#' @description
#' Returns the cumulative weighted mean of `x`, with weights equal to
#' `log(1:length(x))`. `NA` values are replaced by `na.val` before computing.
#'
#' @param x A numeric vector.
#' @param na.val Replacement value for `NA`s.
#' @return A numeric vector of the same length as `x`.
#' @noRd
cumwmean <- function(x, na.val = 0) {
  x[is.na(x)] <- na.val
  return(cumsum(x * log(1:length(x))) / cumsum(log(1:length(x))))
}

#' Cube Root
#'
#' @param x A numeric vector.
#' @return The element-wise cube root of `x`.
#' @noRd
cubert <- function(x) {
  return(nroot(x, 3))
}

#' Nth Root
#'
#' @param x A numeric vector.
#' @param n The root to compute.
#' @return The element-wise nth root of `x`.
#' @noRd
nroot <- function(x, n) {
  nr <- function(x, n) {
    return(x^(1 / n))
  }
  nr_V <- Vectorize(nr, "x")
  return(nr_V(x, n))
}

#' Cumulative Weighted Mean with Prepended Padding
#'
#' @description
#' Prepends `ln` copies of `val` to `x` and returns the cumulative weighted
#' mean for the original portion. This anchors early values toward `val`.
#'
#' @param x A numeric vector.
#' @param ln Number of padding values to prepend.
#' @param val Padding value.
#' @param na.val Replacement value for `NA`s in `x`.
#' @return A numeric vector of the same length as `x`.
#' @noRd
cumwmean_expanded <- function(x, ln = 10, val = 0, na.val = val) {
  ln_origin <- length(x)

  x <- c(rep(val, ln), x)
  cwm <- cumwmean(x, na.val = na.val)

  return(cwm[(ln + 1):length(cwm)])
  # if(length(x)<ln){ x <- c(rep(val, ln-length(x)), x) } cwm<-cumwmean(x) if(ln_origin < ln){ return(cwm[(ln-ln_origin+1):ln]) } else { return(cwm) }
}

#' Lagged Cumulative Weighted Mean (Expanded)
#'
#' @description
#' Like `cumwmean_expanded()` but lagged by one step, so each value represents
#' the weighted mean of all *preceding* values. Useful for avoiding data leakage
#' when building rolling historical features.
#'
#' @param x A numeric vector.
#' @param ln Number of padding values prepended.
#' @param val Padding value.
#' @param na.val Replacement value for `NA`s.
#' @return A numeric vector of the same length as `x`.
#' @noRd
lagged_cumwmean_expanded <- function(x, ln = 20, val = 0, na.val = val) {
  cwme <- cumwmean_expanded(x, ln, val, na.val = na.val)
  return(c(val, cwme[-length(cwme)]))
}

#' Scalar Lagged Cumulative Weighted Mean (Expanded)
#'
#' @description
#' Returns only the final (scalar) value from `lagged_cumwmean_expanded()`.
#' Intended for use inside `slider::slide()` to compute a single summary
#' statistic per sliding window.
#'
#' @param x A numeric vector (the sliding window).
#' @param ln Number of padding values prepended.
#' @param val Padding value.
#' @param na.val Replacement value for `NA`s.
#' @return A single numeric value.
#' @noRd
s_lagged_cumwmean_expanded <- function(x, ln = 20, val = 0, na.val = val) {
  lce <- lagged_cumwmean_expanded(x, ln = ln, val = val, na.val = na.val)
  return(lce[length(lce)])
}

#' Expand or Trim a Vector to a Given Length
#'
#' @description
#' If `v` is shorter than `ln`, appends `val` until `length(v) == ln`.
#' If `v` is longer, truncates to the first `ln` elements.
#'
#' @param v A vector.
#' @param ln Target length.
#' @param val Fill value when padding.
#' @return A vector of length `ln`.
#' @noRd
expand_val <- function(v, ln, val = NA) {
  if (length(v) < ln) {
    v <- c(v, rep(val, ln - length(v)))
  } else if (length(v) > ln) {
    v <- v[1:ln]
  }
  return(v)
}

#' Ensure Tidy Column Names
#'
#' @description
#' Applies `janitor::clean_names()` and renames a `race` column to `round` for
#' consistency with the rest of the package. Returns `data` unchanged if it is
#' `NULL` or all `NA`.
#'
#' @param data A data frame (or `NULL`/`NA`).
#' @return The data frame with cleaned column names, or the original value if empty.
#' @noRd
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

#' Weighted Mean Update with a New Value
#'
#' @description
#' Computes an updated weighted mean by simulating a history of `ln - 1` copies
#' of `x` followed by `newval`, then returning the final cumulative weighted mean.
#' Vectorized over both `newval` and `x`.
#'
#' @param newval The new observation(s) to incorporate.
#' @param x The current historical mean(s).
#' @param ln The effective window length for the weighting.
#' @return A numeric vector of updated weighted means.
#' @noRd
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
#' @noRd
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
