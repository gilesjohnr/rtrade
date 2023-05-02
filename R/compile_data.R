compile_data <- function(param) {

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = param$limit,
                  verbose = FALSE)

  d <- cbind(d, clean_dates(d$time_open))

  st <- calc_supertrend(HLC=d[,c("high","low","close")], n=param$n_supertrend_short, f=param$f_supertrend_short_buy)
  d$supertrend_1 <- st$supertrend
  d$supertrend_1_buy <- st$buy
  d$supertrend_1_sell <- st$sell

  st <- calc_supertrend(HLC=d[,c("high","low","close")], n=param$n_supertrend_short, f=param$f_supertrend_short_sell)
  d$supertrend_2 <- st$supertrend
  d$supertrend_2_buy <- st$buy
  d$supertrend_2_sell <- st$sell

  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=param$n_atr))$atr

  d$EMA_1 <- EMA(d[,"close"], n=param$n_ema_1) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$EMA_1[i] <- mean(d$EMA_1[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$EMA_1_slope[i] <- d$EMA_1[i] - d$EMA_1[i-1]

  d$EMA_2 <- EMA(d[,"close"], n=param$n_ema_2) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$EMA_2[i] <- mean(d$EMA_2[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$EMA_2_slope[i] <- d$EMA_2[i] - d$EMA_2[i-1]


  return(d)

}
