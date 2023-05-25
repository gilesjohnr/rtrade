get_buy_logic <- function(d, t, param, live=FALSE, verbose=TRUE) {

  logic_buy <- FALSE

  if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

    logic_buy <- d$supertrend_1_buy[t] == 1 & d$close[t] > d$sar[t] | d$supertrend_1_buy[t-1] == 1 & d$close[t-1] > d$sar[t-1]
    logic_buy <- logic_buy | d$close[t] > d$supertrend_1[t] & d$close[t-1] <= d$sar[t-1] & d$close[t] > d$sar[t]

  }


  if (logic_buy) {

    if (live & param$double_check) {

      if (verbose) message(":: Double-checking BUY trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$date_time)

      logic_buy <- FALSE

      if (!is.na(d$supertrend_1[t-1]) & !is.na(d$sar[t-1])) {

        logic_buy <- d$supertrend_1_buy[t] == 1 & d$close[t] > d$sar[t] | d$supertrend_1_buy[t-1] == 1 & d$close[t-1] > d$sar[t-1]
        logic_buy <- logic_buy | d$close[t] > d$supertrend_1[t] & d$close[t-1] <= d$sar[t-1] & d$close[t] > d$sar[t]

      }

      if (verbose) message(ifelse(logic_buy, 'Positive', 'False-positive'))

    }

  }


  if (!is.na(d$rsi_smooth[t])) {

    n <- pmin(param$n_quantile_rsi, nrow(d)) - 1
    threshold <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_buy_rsi, na.rm=TRUE)
    #threshold <- param$quantile_buy_rsi
    Y <- d$rsi_smooth[t] <= threshold & d$rsi_smooth_slope[t] >= 0
    logic_buy <- logic_buy & Y

  }


  # Only allow BUY if not in short-term downward trend
  if (!is.na(d$ema_short_slope[t])) {

    Y <- d$ema_short_slope[t] < param$slope_threshold_buy
    logic_buy <- !Y

  }


  if (is.na(logic_buy)) {
    warning('NA returned for logic_buy')
    logic_buy <- FALSE
  }


  if (logic_buy & verbose) message(":: Logic BUY ::")

  return(logic_buy)

}
