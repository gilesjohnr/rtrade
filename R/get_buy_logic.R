get_buy_logic <- function(d, t, param, live=FALSE, verbose=TRUE) {

  logic_buy <- FALSE

  if (!is.na(d$bb_slope[t]) & d$ema_short_slope[t] < abs(param$slope_threshold_bband_mode) & d$ema_short_slope[t] > -1*abs(param$slope_threshold_bband_mode) ) {

    # SIDEWAYS TREND

    logic_buy <- d$mid[t] < d$bb_lo[t]
    if (logic_buy & verbose) message(":: Logic BUY (bbdand mode) ::")


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
    if (!is.na(d$ema_short_slope[t])) {

      Y <- d$ema_short_slope[t] <= param$slope_threshold_short_buy
      if (Y) {
        if (verbose) message(":: HOLD BUY (short term downtrend) ::")
        logic_buy <- !Y
      }

    }


    # BUY IF short-term buy directly preceded long-term uptrend shift
    if (!is.na(d$ema_short_slope[t-1])) {

      Y <- d$ema_short_slope[t] >= param$slope_threshold_short_buy & d$ema_short_slope[t-1] < param$slope_threshold_short_buy & any(d$supertrend_1_buy[(t-2):t] == 1)

      if (Y) {
        logic_buy <- Y
        if (verbose) message(":: Logic BUY (short buy just before trend change) ::")
      }

    }

  }




  return(logic_buy)

}
