#' Calculate the supertrend indicator
#'
#' @param HLC xts object that contains High-Low-Close prices
#' @param n period of days
#' @param f Average True Range factor
#'
#' @return dataframe
#'


calc_supertrend <- function(HLC, n = 10, f = 3) {
  
  atr <- as.data.frame(TTR::ATR(HLC, n = n))
  
  upperbasic <- (HLC[, 1] + HLC[, 2])/2 + (f * atr$atr)
  lowerbasic <- (HLC[, 1] + HLC[, 2])/2 - (f * atr$atr)
  
  lowerfinal <- lowerbasic
  upperfinal <- upperbasic
  close <- HLC[, 3]
  
  for (i in 2:length(upperbasic)) {
    
    if (isTRUE(close[i-1] < upperfinal[i-1])) {
      upperfinal[i] <- min(upperbasic[i], upperfinal[i-1], na.rm = TRUE)
    } else {
      upperfinal[i] <- upperbasic[i]
    }
    
  }
  
  for (i in 2:length(lowerbasic)) {
    
    if (isTRUE(close[i-1] > lowerfinal[i-1])) {
      lowerfinal[i] <- max(lowerbasic[i], lowerfinal[i-1], na.rm = TRUE)
    } else {
      lowerfinal[i] <- lowerbasic[i]
    }
    
  }
  
  supertrend <- data.frame(supertrend_hi=upperfinal, 
                           supertrend_lo=lowerfinal,
                           supertrend=NA)
  st <- supertrend$supertrend
  
  cl_greater_upper <- close > upperfinal
  cl_greater_lower <- close > lowerfinal
  cl_less_upper <- close < upperfinal
  cl_less_lower <- close < lowerfinal
  
  for (i in 2:nrow(supertrend)) {
    
    if ( isTRUE(st[i-1] == upperfinal[i-1]) &
         isTRUE(cl_less_upper[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(st[i-1] == upperfinal[i-1]) &
                isTRUE(cl_greater_upper[i]) ) {
      st[i] <- lowerfinal[i]
    } else if ( isTRUE(st[i-1] == lowerfinal[i-1]) &
                isTRUE(cl_greater_lower[i]) ) {
      st[i] <- lowerfinal[i]
    } else if ( isTRUE(st[i-1] == lowerfinal[i-1]) &
                isTRUE(cl_less_lower[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(cl_less_upper[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(cl_greater_lower[i]) ) {
      st[i] <- lowerfinal[i]
    }
    
  }
  
  supertrend$supertrend <- st
  supertrend$green <- supertrend$supertrend == supertrend$supertrend_lo
  supertrend$buy <- supertrend$sell <- 0
  
  for (i in 2:nrow(supertrend)) {
    
    if (all(!is.na(supertrend$green[(i-1):i]))) {
      if (isTRUE(supertrend$green[i]) & isFALSE(supertrend$green[i-1])) supertrend$buy[i] <- 1
      if (isFALSE(supertrend$green[i]) & isTRUE(supertrend$green[i-1])) supertrend$sell[i] <- 1
    }
  }
  
  return(supertrend)
  
}

