# Return a weighted mean, filling the supplied vector to length `ln` with value `val` before calculating it.
# Weighted mean is weighted on cubed root of 1:`ln`
wmean <- function(x, ln = 20, val = 0) {
  ln_origin <- length(x)
  if (length(x) < ln) {
    x <- c(rep(val, ln - length(x)), x)
  }
  return(stats::weighted.mean(x, w = cubert(1:length(x))))
}

# return the cumulative weighted mean (with weights as log(1:length(x)))
cumwmean <- function(x, na.val = 0) {
  x[is.na(x)] <- na.val
  return(cumsum(x * log(1:length(x))) / cumsum(log(1:length(x))))
}

# get the cube root
cubert <- function(x) {
  return(nroot(x, 3))
}

# get a nth root
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
  data <- data |> janitor::clean_names()
  if ("race" %in% colnames(data)) {
    data <- data |>
      dplyr::rename("round" = "race")
  }
  return(data)
}

wmean_two <- function(newval, x, ln) {
  wt <- function(newval, x, ln) {
    x <- c(rep(x, ln - 1), newval)
    cwm <- cumwmean(x)

    return(cwm[length(cwm)])
  }
  wt_v <- Vectorize(wt, vectorize.args = c("newval", "x"))
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

#' Apply exponential season decay to historical data
#'
#' Weights recent seasons more heavily, with older seasons decaying exponentially.
#' Current season (2026) gets weight 1.0, prior seasons decay by factor alpha.
#'
#' @param seasons Numeric vector of season years
#' @param current_season The reference season (typically 2026)
#' @param alpha Decay rate per season (0.5 means prior season gets 50% weight)
#'
#' @return Numeric vector of weights (same length as seasons)
#' @keywords internal
#' @noRd
season_decay_weights <- function(seasons, current_season = 2026, alpha = 0.5) {
  seasons_back <- current_season - seasons
  weights <- alpha^seasons_back
  return(weights)
}

#' Apply exponential weighting to a sliding window with season decay
#'
#' Combines in-season cumulative weighting (log-based) with season-to-season decay.
#' This prioritizes 2026 data while still using historical context for new teams.
#'
#' @param x Performance values to average
#' @param seasons Season year for each value
#' @param ln Window length for historical lookback
#' @param val Default value for padding
#' @param current_season Reference season (typically 2026)
#' @param season_alpha Season decay rate
#' @param na.val Value to impute for NA entries
#'
#' @return Single weighted average value
#' @keywords internal
#' @noRd
s_lagged_cumwmean_expanded_with_season_decay <- function(
  x, seasons, ln = 20, val = 0, current_season = 2026,
  season_alpha = 0.5, na.val = val
) {
  # Handle empty or single-element input
  if (length(x) == 0) {
    return(val)
  }
  if (length(x) == 1) {
    return(x[1])
  }

  # Expand to full window size, padding with default values
  ln_origin <- length(x)
  x_padded <- c(rep(val, ln), x)
  seasons_padded <- c(rep(min(seasons, na.rm = TRUE), ln), seasons)

  # Replace NA with na.val
  x_padded[is.na(x_padded)] <- na.val

  # Compute in-season cumulative weighted mean (log-based)
  cwm <- cumwmean(x_padded, na.val = na.val)

  # Apply season decay: multiply each point by its season weight
  season_wts <- season_decay_weights(seasons_padded, current_season, season_alpha)
  weighted_cwm <- cwm * season_wts

  # Normalize weights so they sum appropriately
  weight_sum <- cumsum(log(1:length(x_padded)) * season_wts)
  weighted_mean_series <- cumsum(x_padded * log(1:length(x_padded)) * season_wts) / pmax(weight_sum, 1e-10)

  # Extract the lagged version (return previous value)
  lagged_result <- c(val, weighted_mean_series[-length(weighted_mean_series)])

  # Return the last value (current result)
  return(lagged_result[length(lagged_result)])
}

#' Wrapper for use with slider::slide for season-aware weighted averaging
#'
#' Handles the data passing to apply season decay during sliding operations.
#' Current year is inferred from the calling context (requires season column).
#'
#' @param x Performance values
#' @param seasons Season years (passed as auxiliary argument via slider)
#' @param ln Window length
#' @param val Default value
#' @param current_season Reference season
#' @param season_alpha Season decay rate
#' @param na.val NA imputation value
#'
#' @return Single weighted average value
#' @keywords internal
#' @noRd
s_lagged_cumwmean_expanded_slide_wrapper <- function(
  x, seasons = NULL, ln = 20, val = 0,
  current_season = 2026, season_alpha = 0.5, na.val = val
) {
  # If seasons not provided, fall back to standard weighting
  if (is.null(seasons) || length(seasons) == 0) {
    return(s_lagged_cumwmean_expanded(x, ln = ln, val = val, na.val = na.val))
  }

  return(s_lagged_cumwmean_expanded_with_season_decay(
    x, seasons,
    ln = ln, val = val,
    current_season = current_season, season_alpha = season_alpha, na.val = na.val
  ))
}

#' Create a season-aware sliding window function
#'
#' Factory function that returns a closure with access to season data.
#' Used to wrap slider::slide calls with season decay information.
#'
#' @param season_vec Vector of season values corresponding to x
#' @param current_season Reference season (typically 2026)
#' @param season_alpha Season decay rate
#'
#' @return A function suitable for use with slider::slide
#' @keywords internal
#' @noRd
make_season_weighted_slider <- function(season_vec, current_season = 2026, season_alpha = 0.5) {
  force(season_vec)
  force(current_season)
  force(season_alpha)

  function(x, ln = 20, val = 0, na.val = val) {
    # Get corresponding seasons for the windowed x values
    # slider::slide passes consecutive chunks, so we need the matching seasons
    window_idx <- length(season_vec) - length(x) + 1
    window_seasons <- season_vec[window_idx:length(season_vec)]

    return(s_lagged_cumwmean_expanded_with_season_decay(
      x, window_seasons,
      ln = ln, val = val,
      current_season = current_season, season_alpha = season_alpha, na.val = na.val
    ))
  }
}
