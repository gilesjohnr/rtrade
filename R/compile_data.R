compile_data <- function(param, limit=NULL) {

  if (is.null(limit)) limit <- param$limit

  # Get hourly (or other small-scale time interval)
  d <- get_klines(symbol = param$symbol,
                  interval = param$interval_short,
                  limit = limit,
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
  d$ema_long <- EMA(d[,"mid"], n=param$n_ema_long) # Exponential Moving Average

  d$ema_buy_hold <- EMA(d[,"mid"], n=param$n_ema_buy_hold) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_buy_hold[i] <- mean(d$ema_buy_hold[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_buy_hold_slope[i] <- d$ema_buy_hold[i] - d$ema_buy_hold[i-1]

  d$ema_sell_hold <- EMA(d[,"mid"], n=param$n_ema_sell_hold) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_sell_hold[i] <- mean(d$ema_sell_hold[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_sell_hold_slope[i] <- d$ema_sell_hold[i] - d$ema_sell_hold[i-1]

  d$ema_bband_mode <- EMA(d[,"mid"], n=param$n_ema_bband_mode) # Exponential Moving Average
  k <- 1
  for (i in (k+1):nrow(d)) d$ema_bband_mode[i] <- mean(d$ema_bband_mode[(i-k):i], na.rm=T)
  for (i in 2:nrow(d)) d$ema_bband_mode_slope[i] <- d$ema_bband_mode[i] - d$ema_bband_mode[i-1]


  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands_1), sd=param$sd_bbands_1))
  d$bb_avg_1 <- bb$mavg
  d$bb_hi_1 <- bb$up
  d$bb_lo_1 <- bb$dn

  bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands_2), sd=param$sd_bbands_2))
  d$bb_avg_2 <- bb$mavg
  d$bb_hi_2 <- bb$up
  d$bb_lo_2 <- bb$dn

  d$bb_peak_1 <- as.integer(d$open > d$bb_hi_1 | d$close > d$bb_hi_1)
  d$bb_peak_1[is.na(d$bb_peak_1)] <- 0
  d$bb_peak_seq_1 <- d$bb_peak_1
  for (i in 2:nrow(d)) if (d$bb_peak_seq_1[i] > 0) d$bb_peak_seq_1[i] <- d$bb_peak_seq_1[i] + d$bb_peak_seq_1[i-1]

  d$bb_dip_1 <- as.integer(d$open < d$bb_lo_1 | d$close < d$bb_lo_1)
  d$bb_dip_1[is.na(d$bb_dip_1)] <- 0
  d$bb_dip_seq_1 <- d$bb_dip_1
  for (i in 2:nrow(d)) if (d$bb_dip_seq_1[i] > 0) d$bb_dip_seq_1[i] <- d$bb_dip_seq_1[i] + d$bb_dip_seq_1[i-1]

  d$bb_peak_2 <- as.integer(d$open > d$bb_hi_2 | d$close > d$bb_hi_2)
  d$bb_peak_2[is.na(d$bb_peak_2)] <- 0
  d$bb_peak_seq_2 <- d$bb_peak_2
  for (i in 2:nrow(d)) if (d$bb_peak_seq_2[i] > 0) d$bb_peak_seq_2[i] <- d$bb_peak_seq_2[i] + d$bb_peak_seq_2[i-1]


  return(d)

}
