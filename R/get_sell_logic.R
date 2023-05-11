get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL, verbose=TRUE) {

  logic_sell <- FALSE

  if (!is.na(d$supertrend_2[t-1])) {

    if (d$supertrend_2[t] < d$mid[t]) {
      logic_sell <- d$supertrend_2_sell[t] == 1 | d$supertrend_2_sell[t-1] == 1
    } else {
      logic_sell <- d$supertrend_1_sell[t] == 1 | d$supertrend_1_sell[t-1] == 1
    }

  }

  if (logic_sell) {

    if (verbose) message(":: Logic SELL (short sell) ::")

    if (live & param$double_check) {

      if (verbose) message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$time_close)

      if (!is.na(d$supertrend_2[t-1])) {

        if (d$supertrend_2[t] < d$mid[t]) {
          logic_sell <- d$supertrend_2_sell[t] == 1 | d$supertrend_2_sell[t-1] == 1
        } else {
          logic_sell <- d$supertrend_1_sell[t] == 1 | d$supertrend_1_sell[t-1] == 1
        }

      }

      if (verbose) message(ifelse(logic_sell, 'Positive', 'False-positive'))

    }


  }



  # HOLD IF there is a short term uptrend
  if (!is.na(d$ema_sell_hold_slope[t])) {

    Y <- d$ema_sell_hold_slope[t] > param$slope_threshold_sell_hold

    if (Y) {
      if (verbose) message(glue(":: HOLD SELL (long term uptrend) ::"))
      logic_sell <- !Y
    }

  }



  # SELL IF short-term SELL trigger occurs just before long-term SELL hold expires
  if (!is.na(d$ema_sell_hold_slope[t-1]) & !is.na(d$supertrend_2_sell[t-2])) {

    Y <- d$ema_sell_hold_slope[t] <= param$slope_threshold_sell_hold & d$ema_sell_hold_slope[t-1] > param$slope_threshold_sell_hold & any(d$supertrend_2_sell[(t-2):t] == 1)

    if (Y) {
      logic_sell <- Y
      if (verbose) message(":: Logic SELL (short sell just before trend change) ::")
    }

  }



  # IF in side-trend, sell when above bband high
  if (!is.na(d$ema_sell_hold_slope[t]) & !is.na(d$bb_hi[t])) {

    Y <- abs(d$ema_sell_hold_slope[t]) < param$slope_threshold_bband & d$close[t] > d$bb_hi[t]

    if (Y) {
      logic_sell <- Y
      if (verbose) message(":: Logic SELL (sidetrend bband high) ::")
    }


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
    if (verbose) message(glue(":: Logic SELL (gain = {round(gain, 4)}) ::"))
  }





  return(logic_sell)

}
