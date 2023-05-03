get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL) {


  logic_sell <- d$supertrend_2[t-1] <= d$mid[t-1] & d$supertrend_2[t] > d$mid[t]

  if (logic_sell) {

    message(":: Logic SELL 1 (short sell) ::")

    if (live & param$double_check) {

      message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      tmp <- compile_data(param)

      if (tmp$supertrend_2_sell[t] < tmp$mid[t]) {

        logic_sell <- tmp$supertrend_2[t-1] <= tmp$mid[t-1] & tmp$supertrend_2[t] > tmp$mid[t]

      } else {

        logic_sell <- tmp$supertrend_1[t-1] <= tmp$mid[t-1] & tmp$supertrend_1[t] > tmp$mid[t]

      }

      message(ifelse(logic_sell, 'Positive', 'False-positive'))

    }


  }


  # HOLD IF there is a short term uptrend
  if (!is.na(d$ema_short_slope[t])) {

    Y <- d$ema_short_slope[t] >= param$slope_threshold_short_sell

    if (Y) {
      logic_sell <- logic_sell & !Y
      message(glue(":: HOLD SELL (long term uptrend) ::"))
    }

  }

  # HOLD IF there is a long term uptrend
  if (!is.na(d$ema_long_slope[t])) {

    Y <- d$ema_long_slope[t] >= param$slope_threshold_long_sell

    if (Y) {
      logic_sell <- !Y
      message(glue(":: HOLD (long term uptrend) ::"))
    }

  }


  # IF ema_short crosses BELOW ema_long trigger SELL
  if (!is.na(d$ema_short[t-1]) & !is.na(d$ema_long[t-1])) {

    Y <- d$ema_short[t-1] >= d$ema_long[t-1] & d$ema_short[t] < d$ema_long[t]
    if (Y) {
      message(":: Logic SELL (EMA cross) ::")
      logic_sell <- Y
    }

  }

  # BBDANDS high sell
  if (!is.na(d$bb_hi[t])) {

    Y <- d$mid[t] > d$bb_hi[t]
    if (Y) {
      message(":: Logic SELL (High Bollinger band breached) ::")
      logic_sell <- Y
    }

  }




  return(logic_sell)

}
