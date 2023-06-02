get_buy_logic <- function(d, t, param, live=FALSE, verbose=TRUE) {


  logic_buy <- FALSE

  if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

    #logic_buy <- d$supertrend_1_buy[t] == 1 & d$close[t] > d$sar[t]
    #logic_buy <- logic_buy | d$close[t] > d$supertrend_1[t] & d$close[t-1] <= d$sar[t-1] & d$close[t] > d$sar[t]

    logic_buy <- d$close[t] > d$supertrend_1[t] & d$close[t] > d$sar[t]
    if (verbose & logic_buy) message("BUY trigger: Supertrend + SAR")

  }


  if (logic_buy) {

    if (live & param$double_check) {

      if (verbose) message(":: Double-checking BUY trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$date_time)

      logic_buy <- FALSE

      if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

        logic_buy <- d$supertrend_1_buy[t] == 1 & d$close[t] > d$sar[t]
        logic_buy <- logic_buy | d$close[t] > d$supertrend_1[t] & d$close[t-1] <= d$sar[t-1] & d$close[t] > d$sar[t]

      }

      if (verbose) message(ifelse(logic_buy, 'Positive', 'False-positive'))

    }

  }



  #if (!is.na(d$macd_diff[t]) & !is.na(d$adx[t])) {
  #
  #  Y <- d$macd_diff[t] < 0 & d$adx_neg[t] > d$adx_pos[t]
  #  logic_buy <- logic_buy & Y
  #
  #}


  n <- pmin(param$n_quantile_rsi, nrow(d)) - 1

  if (!is.na(d$rsi_smooth[t-2])) {

    threshold <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_buy_rsi, na.rm=TRUE)
    Y <- any(d$rsi_smooth[(t-2):t] < threshold)
    if (verbose & Y) message("RSI below buy threshold")
    logic_buy <- logic_buy & Y

  }







  if (!is.na(d$rsi_smooth[t])) {

    threshold <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)],
                          probs=param$quantile_oversold_rsi,
                          na.rm=TRUE)
    delta <- 2
    Y <- any(d$rsi_smooth[(t-delta):t] < threshold) &
      any(d$close[(t-delta):t] <= d$keltner_lo[(t-delta):t]) &
      #any(d$close[(t-delta):t] < d$bb_lo[(t-delta):t]) &
      any(d$macd_diff[(t-delta):t] <= 0) &
      any(d$adx_neg[(t-delta):t] > d$adx_pos[(t-delta):t]) &
      d$rsi_smooth_slope[t] > 0

    if (is.na(Y)) Y <- FALSE
    if (verbose & Y) message("RSI oversold")
    if (Y) logic_buy <- Y

  }



  # Only allow BUY if not in downward trend
  if (!is.na(d$ema_short_slope[t])) {

    Y1 <- d$ema_short_slope[t] < param$slope_threshold_buy

    Y2 <- d$ema_short[t] < d$ema_mid[t] &
      d$ema_mid[t] < d$ema_long[t] &
      #d$ema_short_slope[t] < 0 &
      d$ema_mid_slope[t] < 0

    if (is.na(Y2)) Y2 <- FALSE
    Y <- Y1 | Y2
    if (verbose & Y) message("HOLD: downward trend")
    if (Y) logic_buy <- !Y

  }



  if (!is.na(d$ema_short_slope[t])) {

    threshold_overbought <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_overbought_rsi, na.rm=TRUE)

    Y <- d$close[t] > d$supertrend_1[t] &
      d$close[t] > d$sar[t] &
      d$ema_short[t] > d$ema_mid[t] &
      d$ema_mid[t] > d$ema_long[t] &
      d$ema_short_slope[t] > param$slope_threshold_sell*0.95 &
      d$ema_mid_slope[t] > 0 &
      d$rsi_smooth[t] < threshold_overbought &
      d$rsi_smooth_slope[t] > 0

    if (is.na(Y)) Y <- FALSE
    if (verbose & Y) message("Re-enter upward trend")
    if (Y) logic_buy <- Y

  }




  if (is.na(logic_buy)) {
    warning('NA returned for logic_buy')
    logic_buy <- FALSE
  }


  if (logic_buy & verbose) message(":: Logic BUY ::")

  return(logic_buy)

}
