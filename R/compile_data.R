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

  # Supertrend
  d$supertrend_1 <- st$supertrend
  d$supertrend_1_buy <- st$buy
  d$supertrend_1_sell <- st$sell

  # Parabolic SAR
  d$sar <- SAR(HL=d[,c("high","low")], accel = c(param$accel_sar, param$max_accel_sar))

  # Average true range
  d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=param$n_atr))$atr


  # MACD Oscillator
  macd <- as.data.frame(MACD(d[,"close"],
                             nFast = param$n_fast_macd,
                             nSlow = param$n_slow_macd,
                             nSig = param$n_signal_macd,
                             maType = "EMA"))
  d$macd <- macd$macd
  d$macd_signal <- macd$signal

  # Relative Strength Index (RSI)
  d$rsi <- RSI(d[,"close"], n=param$n_rsi)

  # Average Directional movement index
  adx <- as.data.frame(ADX(d[,c("high","low","close")], n=param$n_adx))
  d$adx <- adx$ADX
  d$adx_pos <- adx$DIp
  d$adx_neg <- adx$DIn

  # Bollinger bands
  #bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands), sd=param$sd_bbands))
  #d$bb_avg <- bb$mavg
  #d$bb_hi <- bb$up
  #d$bb_lo <- bb$dn




  return(d)

}
