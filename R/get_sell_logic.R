get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL, verbose=TRUE) {

  logic_sell <- FALSE

  if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

    #logic_sell <- d$supertrend_1_sell[t] & d$close[t] < d$sar[t]
    #logic_sell <- logic_sell | d$close[t] < d$supertrend_1[t] & d$close[t-1] >= d$sar[t-1] & d$close[t] < d$sar[t]

    logic_sell <-  d$close[t] < d$supertrend_1[t] & d$close[t] < d$sar[t]
    if (verbose & logic_sell) message("SELL trigger: Supertrend + SAR")

  }


  if (logic_sell) {

    if (live & param$double_check) {

      if (verbose) message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$date_time)

      logic_sell <- FALSE

      if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

        logic_sell <- d$supertrend_1_sell[t] & d$close[t] < d$sar[t]
        logic_sell <- logic_sell | d$close[t] < d$supertrend_1[t] & d$close[t-1] >= d$sar[t-1] & d$close[t] < d$sar[t]

      }

      if (verbose) message(ifelse(logic_sell, 'Positive', 'False-positive'))

    }


  }



  n <- pmin(param$n_quantile_rsi, nrow(d)) - 1

  if (!is.na(d$rsi_smooth[t])) {

    Y <- any(d$rsi_smooth[(t-2):t] > quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_sell_rsi, na.rm=TRUE))
    if (verbose & Y) message("RSI above sell threshold")
    logic_sell <- logic_sell & Y

  }



  # HOLD IF there is a short term uptrend
  if (!is.na(d$ema_short_slope[t])) {

    Y1 <- d$ema_short_slope[t] > param$slope_threshold_sell

    Y2 <- d$ema_short[t] > d$ema_mid[t] &
      d$ema_mid[t] > d$ema_long[t] &
      #d$ema_short_slope[t] > 0 &
      d$ema_mid_slope[t] > 0

    if (is.na(Y2)) Y2 <- FALSE
    Y <- Y1 | Y2

    if (verbose & Y) message("HOLD: upward trend")
    if (Y) logic_sell <- !Y

  }



  if (!is.na(d$rsi_smooth[t]) & !is.na(d$keltner_hi[t])) {

    threshold <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)],
                          probs=param$quantile_overbought_rsi,
                          na.rm=TRUE)

    delta <- 2
    Y <- any(d$rsi_smooth[(t-delta):t] > threshold) &
      any(d$close[(t-delta):t] > d$keltner_hi[(t-delta):t]) &
      any(d$close[(t-delta):t] > d$bb_hi[(t-delta):t]) &
      any(d$macd_diff[(t-delta):t] > 0) &
      any(d$adx_neg[(t-delta):t] < d$adx_pos[(t-delta):t]) &
      d$rsi_smooth_slope[t] < 0

    if (verbose & Y) message("RSI overbought")
    if (Y) logic_sell <- Y

  }


  # HOLD IF there is a short term uptrend
  if (!is.na(d$ema_short_slope[t])) {

    Y <- d$ema_short_slope[t] > param$slope_threshold_sell
    if (verbose & Y) message("HOLD: upward trend")
    if (Y) logic_sell <- !Y

  }



  # SELL IF price moves outside set risk ratio bounds
  if (live) {

    x <- get_all_orders(param$symbol)
    x <- x[x$status %in% c('FILLED', 'PARTIALLY FILLED') & x$side == 'BUY',]
    buy_price <- as.numeric(x$price[which.max(x$date_time)])

  } else {

    buy_price <- trades[which.max(trades$date_time), 'price']

  }

  gain <- mean(c(d$mid[t], d$close[t])) / buy_price - 1

  sel <- which(d$date_time >= max(d$date_time) - 60*60)
  sell_trigger_low <- -1*max(d$atr[sel]/d$mid[sel], na.rm=T)*param$f_atr
  sell_trigger_high <- -1*sell_trigger_low * param$risk_ratio

  Y <- gain > sell_trigger_high | gain < sell_trigger_low
  if (Y) {
    logic_sell <- Y
    if (verbose) message(glue("change = {round(gain, 4)}"))
  }


  if (is.na(logic_sell)) {
    warning('NA returned for logic_sell')
    logic_sell <- FALSE
  }

  if (logic_sell & verbose) message(":: Logic SELL ::")

  return(logic_sell)

}
