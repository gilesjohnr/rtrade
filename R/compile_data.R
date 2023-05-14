compile_data <- function(param, time_stop=NULL, limit=NULL) {

  if (is.null(limit)) limit <- param$limit

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = limit,
                  time_stop = time_stop,
                  verbose = FALSE)

  d <- cbind(d, clean_dates(d$time_open))


  st <- calc_supertrend(HLC=d[,c("high","low","close")],
                        n=round(param$n_supertrend_1, 2),
                        f=round(param$f_supertrend_1, 2))

  d$supertrend_1 <- st$supertrend
  d$supertrend_1_buy <- st$buy
  d$supertrend_1_sell <- st$sell

  st <- calc_supertrend(HLC=d[,c("high","low","close")],
                        n=round(param$n_supertrend_2, 2),
                        f=round(param$f_supertrend_2, 2))

  d$supertrend_2 <- st$supertrend
  d$supertrend_2_buy <- st$buy
  d$supertrend_2_sell <- st$sell

  st <- calc_supertrend(HLC=d[,c("high","low","close")],
                        n=round(param$n_supertrend_3, 2),
                        f=round(param$f_supertrend_3, 2))

  d$supertrend_3 <- st$supertrend
  d$supertrend_3_buy <- st$buy
  d$supertrend_3_sell <- st$sell

  st <- calc_supertrend(HLC=d[,c("high","low","close")],
                        n=round(param$n_supertrend_4, 2),
                        f=round(param$f_supertrend_4, 2))

  d$supertrend_4 <- st$supertrend
  d$supertrend_4_buy <- st$buy
  d$supertrend_4_sell <- st$sell


  d$ema_short <- EMA(d[,"mid"], n=round(param$n_ema_short, 2)) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_short[i] <- mean(d$ema_short[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_short_slope[i] <- d$ema_short[i] - d$ema_short[i-1]

  d$ema_long <- EMA(d[,"mid"], n=round(param$n_ema_long, 2)) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_long[i] <- mean(d$ema_long[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_long_slope[i] <- d$ema_long[i] - d$ema_long[i-1]


  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=round(param$n_atr, 2)))$atr


  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")],
                             n=round(param$n_bband, 0),
                             sd=round(param$sd_bband, 2)))
  d$bb_avg <- bb$mavg
  d$bb_hi <- bb$up
  d$bb_lo <- bb$dn


  return(d)

}
