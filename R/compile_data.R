compile_data <- function(param, time_stop=NULL, limit=NULL) {

  if (is.null(limit)) limit <- param$limit

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = limit,
                  time_stop = time_stop,
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


  d$ema_buy_hold <- EMA(d[,"mid"], n=param$n_ema_buy_hold) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_buy_hold[i] <- mean(d$ema_buy_hold[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_buy_hold_slope[i] <- d$ema_buy_hold[i] - d$ema_buy_hold[i-1]

  d$ema_sell_hold <- EMA(d[,"mid"], n=param$n_ema_sell_hold) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_sell_hold[i] <- mean(d$ema_sell_hold[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_sell_hold_slope[i] <- d$ema_sell_hold[i] - d$ema_sell_hold[i-1]


  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=param$n_atr))$atr

  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=param$n_bband, sd=param$sd_bband))
  d$bb_avg <- bb$mavg
  d$bb_hi <- bb$up
  d$bb_lo <- bb$dn


  return(d)

}
