get_buy_logic <- function(d, t, param, live=FALSE, verbose=TRUE) {

  logic_buy <- FALSE

  if (!is.na(d$ema_bband_mode_slope[t]) & d$ema_bband_mode_slope[t] < param$slope_threshold_bband_mode & d$ema_bband_mode_slope[t] > -1*param$slope_threshold_bband_mode ) {

    # SIDEWAYS TREND
    if (verbose) message(":: Sideways trend ::")

    if (!is.na(d$bb_dip_seq_1[t])) {

      logic_buy <- d$bb_dip_seq_1[t] >= param$seq_bbands_1
      if (logic_buy & verbose) message(":: Logic BUY (bband sideways dip) ::")

    }

  } else {

    # OUTSIDE SIDEWAYS TREND

    if (!is.na(d$supertrend_1[t])) {

      logic_buy <- d$supertrend_1_buy[t] == 1

      if (logic_buy) {

        if (verbose) message(":: Logic BUY (short buy) ::")

        if (live & param$double_check) {

          if (verbose) message(":: Double-checking BUY trigger ::")
          Sys.sleep(param$double_check_wait)

          d <- compile_data(param=param)
          logic_buy <- d$supertrend_1_buy[which.max(d$time_close)]

          if (verbose) message(ifelse(logic_buy, 'Positive', 'False-positive'))

        }

      }

    }


    # Only allow BUY if not in short-term downward trend
    if (!is.na(d$ema_buy_hold_slope[t])) {

      Y <- d$ema_buy_hold_slope[t] <= param$slope_threshold_buy_hold

      if (Y) {
        if (verbose) message(":: HOLD BUY (short term downtrend) ::")
        logic_buy <- !Y
      }

    }


    # BUY IF short-term buy directly preceded long-term uptrend shift
    if (!is.na(d$ema_buy_hold_slope[t-1])) {

      Y <- d$ema_buy_hold_slope[t] >= param$slope_threshold_buy_hold & d$ema_buy_hold_slope[t-1] < param$slope_threshold_buy_hold & any(d$supertrend_1_buy[(t-2):t] == 1)

      if (Y) {
        logic_buy <- Y
        if (verbose) message(":: Logic BUY (short buy just before trend change) ::")
      }

    }


  }



  # IF ema_short crosses ABOVE ema_long trigger BUY
  if (!is.na(d$ema_short[t-1]) & !is.na(d$ema_long[t-1])) {

    Y <- d$ema_short[t-1] <= d$ema_long[t-1] & d$ema_short[t] > d$ema_long[t]

    if (Y) {
      if (verbose) message(":: Logic BUY (EMA cross) ::")
      logic_buy <- Y
    }

  }




  return(logic_buy)

}
