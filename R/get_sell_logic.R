get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL, verbose=TRUE) {

  logic_sell <- FALSE

  if (!is.na(d$supertrend_1[t-1]))  logic_sell <- d$supertrend_1_sell[t] == 1 | d$supertrend_1_sell[t-1] == 1

  if (!is.na(d$supertrend_2[t-1])) {
    if (d$supertrend_2[t] < d$mid[t]) logic_sell <- d$supertrend_2_sell[t] == 1 | d$supertrend_2_sell[t-1] == 1
  }

  if (!is.na(d$supertrend_3[t-1])) {
    if (d$supertrend_3[t] < d$mid[t]) logic_sell <- d$supertrend_3_sell[t] == 1 | d$supertrend_3_sell[t-1] == 1
  }

  #if (!is.na(d$supertrend_4[t-1])) {
  #  if (d$supertrend_4[t] < d$mid[t]) logic_sell <- d$supertrend_4_sell[t] == 1 | d$supertrend_4_sell[t-1] == 1
  #}


  if (logic_sell) {

    if (verbose) message(":: Logic SELL (short sell) ::")

    if (F) {

      if (verbose) message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$time_close)

      logic_sell <- FALSE

      if (!is.na(d$supertrend_1[t-1]))  logic_sell <- d$supertrend_1_sell[t] == 1 | d$supertrend_1_sell[t-1] == 1

      if (!is.na(d$supertrend_2[t-1])) {
        if (d$supertrend_2[t] < d$mid[t]) logic_sell <- d$supertrend_2_sell[t] == 1 | d$supertrend_2_sell[t-1] == 1
      }

      if (!is.na(d$supertrend_3[t-1])) {
        if (d$supertrend_3[t] < d$mid[t]) logic_sell <- d$supertrend_3_sell[t] == 1 | d$supertrend_3_sell[t-1] == 1
      }

      #if (!is.na(d$supertrend_4[t-1])) {
      #  if (d$supertrend_4[t] < d$mid[t]) logic_sell <- d$supertrend_4_sell[t] == 1 | d$supertrend_4_sell[t-1] == 1
      #}

      if (verbose) message(ifelse(logic_sell, 'Positive', 'False-positive'))

    }


  }


  # SELL IF last 3 closing price above bband hi
  n <- round(param$n_above_bband_hi, 0)
  if (!is.na(d$bb_hi[t-n])) {

    Y <- all(d$close[(t-n):t] >= d$bb_hi[(t-n):t] | d$open[(t-n):t] >= d$bb_hi[(t-n):t])

    if (Y) {

      if (verbose) message(glue(":: Logic SELL (action above bband hi) ::"))
      logic_sell <- Y
    }

  }


  # HOLD IF there is a short term uptrend
  if (!is.na(d$ema_short_slope[t])) {

    Y <- d$ema_short_slope[t] > param$slope_threshold_sell_hold

    if (Y) {

      if (verbose) message(glue(":: HOLD SELL (upward trend) ::"))
      logic_sell <- !Y
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
