#' Add various technical indicators to a dataframe
#' 
#' Function expects following columns: open, high, low, close, mean, volume, etc
#'
#' @param x dataframe object produced by get_data function
#'
#' @return dataframe
#'


add_indicators <- function(x) {
  
  # Running means
  x$sma_200 <- SMA(x[,"close"], n=20)
  x$sma_50 <- SMA(x[,"close"], n=50) 
  
  #x$sma <- SMA(x[,"close"], n=30) # Simple Moving Average
  x$ema_90 <- EMA(x[,"close"], n=90) # Exponential Moving Average
  x$ema_30 <- EMA(x[,"close"], n=30) # Exponential Moving Average
  x$ema_6 <- EMA(x[,"close"], n=6) # Exponential Moving Average
  
  
  # MACD
  macd <- as.data.frame(MACD(x[,"close"], 12, 36, 9, maType="EMA"))
  x$macd <- macd$macd
  x$macd_signal <- macd$signal
  
  # Relative Strength Index (RSI)
  x$rsi <- RSI(x[,"close"], n=14)
  
  # Average true range
  atr <- as.data.frame(ATR(x[,c("high","low","close")], n=14))
  x$atr <- atr$atr
  
  # Average directional movement index
  adx <- as.data.frame(ADX(x[,c("high","low","close")], n=14))
  x$adx <- adx$ADX
  x$adx_pos <- adx$DIp
  x$adx_neg <- adx$DIn
  
  # Bollinger bands
  bb <- as.data.frame(BBands(HLC=x[,c("high","low","close")], n=20, sd=1.96))
  #bb <- as.data.frame(BBands(HLC=x[,"close"], n=20, sd=2))
  x$bb_avg <- bb$mavg
  x$bb_hi <- bb$up
  x$bb_lo <- bb$dn
  
  # Supertrend
  st <- calc_supertrend(HLC=x[,c("high","low","close")], n=10, f=3)
  x$supertrend <- st$supertrend
  x$supertrend_buy <- st$buy
  x$supertrend_sell <- st$sell
  
  return(x)
  
}

