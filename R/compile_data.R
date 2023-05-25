compile_data <- function(param, time_stop=NULL, limit=NULL) {

  if (is.null(limit)) limit <- param$limit

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = limit,
                  time_stop = time_stop,
                  verbose = FALSE)

  d <- d[,1:9]
  d <- cbind(d, clean_dates(d$time_open))
  d <- d[-c(1,2)]


  # Supertrend
  st <- calc_supertrend(HLC=d[,c("high","low","close")],
                        n=round(param$n_supertrend_1, 2),
                        f=round(param$f_supertrend_1, 2))

  d$supertrend_1 <- st$supertrend
  d$supertrend_1_buy <- st$buy
  d$supertrend_1_sell <- st$sell
  for (i in 2:nrow(d)) d$supertrend_1_slope[i] <- d$supertrend_1[i] - d$supertrend_1[i-1]


  # Parabolic SAR
  d$sar <- SAR(HL=d[,c("high","low")], accel = c(param$accel_sar, param$max_accel_sar))


  d$ema_short <- EMA(d[,"mid"], n=param$n_ema_short) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_short[i] <- mean(d$ema_short[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_short_slope[i] <- d$ema_short[i] - d$ema_short[i-1]


  d$ema_long <- EMA(d[,"mid"], n=param$n_ema_long) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_long[i] <- mean(d$ema_long[(i-k):i], na.rm=T)
  #for (i in 2:nrow(d)) d$ema_long_slope[i] <- d$ema_long[i] - d$ema_long[i-1]


  # Average true range
  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=param$n_atr))$atr


  # Relative Strength Index (RSI)
  d$rsi <- RSI(d[,"close"], n=param$n_rsi)
  d$rsi_smooth <- zoo::rollmean(d$rsi, k=5, align='right', fill = NA, na.pad=T)
  for (i in 2:nrow(d)) d$rsi_smooth_slope[i] <- d$rsi_smooth[i] - d$rsi_smooth[i-1]


  # MACD Oscillator
  macd <- as.data.frame(MACD(d[,"close"],
                             nFast = param$n_fast_macd,
                             nSlow = param$n_slow_macd,
                             nSig = param$n_signal_macd,
                             maType = "EMA"))
  d$macd <- macd$macd
  d$macd_signal <- macd$signal
  d$macd_diff <- d$macd - d$macd_signal


  # Average Directional movement index
  adx <- as.data.frame(ADX(d[,c("high","low","close")], n=param$n_adx))
  d$adx <- adx$ADX
  d$adx_pos <- adx$DIp
  d$adx_neg <- adx$DIn


  # Bollinger bands
  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands), sd=param$sd_bbands))
  d$bb_avg <- bb$mavg
  d$bb_hi <- bb$up
  d$bb_lo <- bb$dn


  return(d)

}
