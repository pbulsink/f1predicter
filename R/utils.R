wmean <- function(x, ln = 20, val=0){
  ln_origin<-length(x)
  if(length(x) < ln){
    x <- c(x, rep(val, ln-length(x)))
  }
  return(weighted.mean(x, w = cubert(1:length(x))))
}

cumwmean<-function(x){
  return(cumsum(x*log(1:length(x)))/cumsum(log(1:length(x))))
}

cubert <- function(x){
  return(nroot(x, 3))
}

nroot <- function(x, n){
  nr<-function(x, n){
    return(x ^ (1/n))
  }
  nr_V <- Vectorize(nr, 'x')
  return(nr_V(x, n))
}

cumwmean_expanded <- function(x, ln=10, val=0){
  ln_origin<-length(x)

  x <- c(rep(val, ln), x)
  cwm <- cumwmean(x)

  return(cwm[(ln+1):length(cwm)])
  # if(length(x)<ln){
  #   x <- c(rep(val, ln-length(x)), x)
  # }
  # cwm<-cumwmean(x)
  # if(ln_origin < ln){
  #   return(cwm[(ln-ln_origin+1):ln])
  # } else {
  #   return(cwm)
  # }
}

lagged_cumwmean_expanded <- function(x, ln=20, val=0){
  cwme <- cumwmean_expanded(x, ln, val)
  return(c(val, cwme[-length(cwme)]))
}

s_lagged_cumwmean_expanded <- function(x, ln = 20, val = 0){
  lce <- lagged_cumwmean_expanded(x, ln = ln, val = val)
  return(lce[length(lce)])
}

expand_val <- function(v, ln, val = NA){
  if(length(v) < ln){
    v<-c(v, rep(val, ln - length(v)))
  } else if(length(v) > ln){
    v<-v[1:ln]
  }
  return(v)
}
