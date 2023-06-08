wmean <- function(x, ln = 20, val=0){
  ln_origin<-length(x)
  if(length(x) < ln){
    x <- c(x, rep(val, ln-length(x)))
  }
  return(weighted.mean(x, w = sqrt(1:length(x))))
}

cumwmean<-function(x){
  return(cumsum(x*sqrt(1:length(x)))/cumsum(sqrt(1:length(x))))
}

cumwmean_expanded <- function(x, ln=20, val=0){
  ln_origin<-length(x)
  if(length(x)<ln){
    x <- c(rep(val, ln-length(x)), x)
  }
  cwm<-cumwmean(x)
  if(ln_origin < ln){
    return(cwm[(ln-ln_origin+1):ln])
  } else {
    return(cwm)
  }
}

lagged_cumwmean_expanded <- function(x, ln=20, val=0){
  cwme <- cumwmean_expanded(x, ln, val)
  return(c(val, cwme[-length(cwme)]))
}

expand_val <- function(v, ln, val = NA){
  if(length(v) < ln){
    v<-c(v, rep(val, ln - length(v)))
  } else if(length(v) > ln){
    v<-v[1:ln]
  }
  return(v)
}
