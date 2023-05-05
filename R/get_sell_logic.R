get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL, verbose=TRUE) {

  logic_sell <- FALSE

  if (!is.na(d$ema_short_slope[t]) & d$ema_short_slope[t] < param$slope_threshold_bband_mode & d$ema_short_slope[t] > -1*param$slope_threshold_bband_mode ) {

    # SIDEWAYS TREND
    if (verbose) message(":: Sideways trend ::")
    logic_sell <- d$mid[t] > d$bb_hi[t]
    if (logic_sell & verbose) message(":: Logic SELL (bband mode) ::")


  } else {


    logic_sell <- d$supertrend_2_sell[t]

    if (logic_sell) {

      if (verbose) message(":: Logic SELL (short sell) ::")

      if (live & param$double_check) {

        if (verbose) message(":: Double-checking SELL trigger ::")
        Sys.sleep(param$double_check_wait)

        d <- compile_data(param=param)
        logic_sell <- d$supertrend_2_sell[which.max(d$time_close)]

        if (verbose) message(ifelse(logic_sell, 'Positive', 'False-positive'))

      }


    }



    # BBDANDS high sell
    #if (!is.na(d$bb_hi[t])) {
    #
    #  Y <- d$mid[t] > d$bb_hi[t]
    #  if (Y) {
    #    if (verbose) message(":: Logic SELL (High Bollinger band breached) ::")
    #    logic_sell <- Y
    #  }
    #
    #}



    # HOLD IF there is a short term uptrend
    if (!is.na(d$ema_short[t])) {

      Y <- d$ema_short[t] > d$ema_long[t] & d$ema_short_slope[t] > param$slope_threshold_short_sell

      if (Y) {

        if (verbose) message(glue(":: HOLD SELL (long term uptrend) ::"))
        logic_sell <- !Y
      }

    }


  }


  # IF ema_short crosses BELOW ema_long trigger SELL
  if (!is.na(d$ema_short[t-1]) & !is.na(d$ema_long[t-1])) {

    Y <- d$ema_short[t-1] >= d$ema_long[t-1] & d$ema_short[t] < d$ema_long[t]

    if (Y) {
      if (verbose) message(":: Logic SELL (EMA cross) ::")
      logic_sell <- Y
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
