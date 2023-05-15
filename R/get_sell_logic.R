get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL, verbose=TRUE) {

  logic_sell <- FALSE

  if (!is.na(d$supertrend_1[t]) & !is.na(d$sar[t-1])) {

    logic_sell <- d$supertrend_1_sell[t] == 1 &  d$mid[t] < d$sar[t]

  }

  if (logic_sell) {

    if (verbose) message(":: Logic SELL (short sell) ::")

    if (F) {

      if (verbose) message(":: Double-checking SELL trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$time_close)

      logic_sell <- FALSE

      if (!is.na(d$supertrend_1[t]) & !is.na(d$sar[t])) {

        logic_sell <- d$supertrend_1_sell[t] == 1 & d$sar[t] < d$mid[t]

      }

      if (verbose) message(ifelse(logic_sell, 'Positive', 'False-positive'))

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
