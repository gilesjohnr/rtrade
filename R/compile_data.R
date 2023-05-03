compile_data <- function(param) {

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = param$limit,
                  verbose = FALSE)

  d <- cbind(d, clean_dates(d$time_open))

  st <- calc_supertrend(HLC=d[,c("high","low","close")], n=param$n_supertrend_1, f=param$f_supertrend_1)
  d$supertrend_1 <- st$supertrend
  d$supertrend_1_buy <- st$buy
  d$supertrend_1_sell <- st$sell

  st <- calc_supertrend(HLC=d[,c("high","low","close")], n=param$n_supertrend_2, f=param$f_supertrend_2)
  d$supertrend_2 <- st$supertrend
  d$supertrend_2_buy <- st$buy
  d$supertrend_2_sell <- st$sell

  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=param$n_atr))$atr

  d$ema_short <- EMA(d[,"mid"], n=param$n_ema_short) # Exponential Moving Average
  #k <- 1
  #for (i in (k+1):nrow(d)) d$ema_short[i] <- mean(d$ema_short[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_short_slope[i] <- d$ema_short[i] - d$ema_short[i-1]

  d$ema_long <- EMA(d[,"mid"], n=param$n_ema_long) # Exponential Moving Average
  #k <- 1
  #for (i in (k+1):nrow(d)) d$ema_long[i] <- mean(d$ema_long[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_long_slope[i] <- d$ema_long[i] - d$ema_long[i-1]

  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands), sd=param$sd_bbands))
  d$bb_avg <- bb$mavg
  d$bb_hi <- bb$up
  d$bb_lo <- bb$dn

  return(d)

}
