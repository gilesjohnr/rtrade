get_buy_logic <- function(d, t, param, live=FALSE) {

  logic_buy <- FALSE

  if (!is.na(d$supertrend_1[t])) {

    #logic_buy <- d$supertrend_1_buy[t] == 1
    logic_buy <- d$supertrend_1[t-1] >= d$mid[t-1] & d$supertrend_1[t] < d$mid[t] #| d$supertrend_2[t-1] >= d$mid[t-1] & d$supertrend_2[t] < d$mid[t]

    if (logic_buy) {

      message(":: Logic BUY (short buy) ::")

      if (live & param$double_check) {

        message(":: Double-checking BUY trigger ::")
        Sys.sleep(param$double_check_wait)

        # Get hourly (or other small-scale time interval)
        tmp <- get_klines(symbol=param$symbol,
                          interval = param$interval_short,
                          limit = param$limit,
                          verbose = FALSE)

        tmp <- cbind(tmp, clean_dates(tmp$time_open))

        st <- calc_supertrend(HLC=tmp[,c("high","low","close")], n=param$n_supertrend_short, f=param$f_supertrend_short_buy)
        tmp$supertrend_1_buy <- st$buy

        logic_buy <- tmp$supertrend_1_buy[t] == 1
        message(ifelse(logic_buy, 'Positive', 'False-positive'))

      }

    }

  }



  # BUY IF short-term buy directly preceded long-term uptrend shift
  if (!is.na(d$ema_short_slope[t-1])) {

    Y <- d$ema_short_slope[t] >= param$slope_threshold_short_buy & d$ema_short_slope[t-1] < param$slope_threshold_short_buy & any(d$supertrend_1_buy[(t-2):t] == 1)

    if (Y) {
      logic_buy <- Y
      message(":: Logic BUY (short buy just before trend change) ::")
    }

  }


  # Only allow BUY if not in short-term downward trend
  if (!is.na(d$ema_short_slope[t])) {

    Y <- d$ema_short_slope[t] <= param$slope_threshold_short_buy
    if (Y) {
      message(":: HOLD BUY (short term downtrend) ::")
      logic_buy <- logic_buy & !Y
    }

  }


  # Only allow BUY if not in long-term downward trend
  if (!is.na(d$ema_long_slope[t])) {

    Y <- d$ema_long_slope[t] <= param$slope_threshold_long_buy
    if (Y) {
      message(":: HOLD BUY (long term downtrend) ::")
      logic_buy <- logic_buy & !Y
    }

  }

  # IF ema_short crosses ABOVE ema_long trigger BUY
  if (!is.na(d$ema_short[t-1]) & !is.na(d$ema_long[t-1])) {

    Y <- d$ema_short[t-1] <= d$ema_long[t-1] & d$ema_short[t] > d$ema_long[t]
    if (Y) {
      message(":: Logic BUY (EMA cross) ::")
      logic_buy <- Y
    }

  }


  return(logic_buy)

}
