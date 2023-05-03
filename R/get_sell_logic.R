get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL) {


  logic_sell <- d$supertrend_2_sell[t] == 1

  if (logic_sell) {

    message(":: Logic SELL 1 (short sell) ::")

    if (live & param$double_check) {

      message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      tmp <- get_klines(symbol = param$symbol,
                        interval = param$interval_short,
                        limit = 100,
                        verbose = FALSE)

      tmp <- cbind(tmp, clean_dates(tmp$time_open))

      st <- calc_supertrend(HLC=tmp[,c("high","low","close")], n=param$n_supertrend_2, f=param$f_supertrend_2)
      tmp$supertrend_2_sell <- st$sell

      logic_sell <- tmp$supertrend_2_sell[which.max(tmp$date_time)] == 1

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
