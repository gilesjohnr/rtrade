get_sell_logic <- function(d, t, param, live=TRUE, trades=NULL) {


  if (param$manual_sell_triggers) {

    sell_trigger_low <- param$manual_sell_trigger_low
    sell_trigger_high <- param$manual_sell_trigger_high

  } else {

    # High and low sell triggers are based on the ATR in the past 30 minutes.
    sel <- which(d$date_time >= max(d$date_time) - 60*60)
    sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T)*param$f_atr
    sell_trigger_high <- -1*sell_trigger_low * param$risk_ratio

  }


  if (d$supertrend_2_sell[t] < d$mid[t]) {
    #logic_sell <- d$supertrend_2_sell[t] == 1
    logic_sell <- d$supertrend_2[t-1] <= d$mid[t-1] & d$supertrend_2[t] > d$mid[t]
  } else {

    #logic_sell <- d$supertrend_1_sell[t] == 1
    logic_sell <- d$supertrend_1[t-1] <= d$mid[t-1] & d$supertrend_1[t] > d$mid[t]
  }



  if (logic_sell) {

    message(":: Logic SELL 1 (short sell) ::")

    if (param$double_check) {

      message(":: Double-checking SELL trigger ::")
      Sys.sleep(10)

      tmp <- compile_data(param)

      if (tmp$supertrend_2_sell[t] < tmp$mid[t]) {
        #logic_sell <- d$supertrend_2_sell[t] == 1
        logic_sell <- tmp$supertrend_2[t-1] <= tmp$mid[t-1] & tmp$supertrend_2[t] > tmp$mid[t]
      } else {

        #logic_sell <- d$supertrend_1_sell[t] == 1
        logic_sell <- tmp$supertrend_1[t-1] <= tmp$mid[t-1] & tmp$supertrend_1[t] > tmp$mid[t]
      }

      message(ifelse(logic_sell, 'Positive', 'False-positive'))

    }


  }

  # SELL IF price moves outside set risk ratio bounds

  current_price <- d$mid[t]

  if (live) {

    x <- get_all_orders(param$symbol)
    x <- x[x$status == 'FILLED' & x$side == 'BUY',]
    x <- x[which.max(x$date_time),]
    buy_price <- as.numeric(x$price)

  } else {

    buy_price <- trades[which.max(trades$date_time), 'price']

  }

  gain <- current_price / buy_price - 1
  logic_sell_2 <- gain > sell_trigger_high | gain < sell_trigger_low

  if (logic_sell_2 | is.na(logic_sell)) {
    logic_sell <- logic_sell_2
    message(glue(":: Logic SELL 2 (gain = {round(gain, 4)}) ::"))
  }

  ## SELL IF longer-range supertrend detects an extended downward trend
  #logic_sell_3 <- d$EMA_2_slope[t] < param$slope_threshold_sell & d$EMA_2_slope[t-1] >= param$slope_threshold_sell
  #if (logic_sell_3) {
  #  logic_sell <- logic_sell_3
  #  message(":: Logic SELL 3 (downtrend detected) ::")
  #}
  #
  ## SELL IF long term inflexion
  #logic_sell_4 <- d$EMA_2_slope[t-1] > 0 & d$EMA_2_slope[t] < 0
  #if (logic_sell_4) {
  #  logic_sell <- logic_sell_4
  #  message(":: Logic SELL 4 (long term inflexion point) ::")
  #}


  # If there is a strong longer term upward trend, hold even when there are short term dips
  if (!is.na(d$EMA_2_slope[t])) {
    tmp <- d$EMA_2_slope[t] > param$slope_threshold_sell
    if (tmp) {
      logic_sell <- !tmp
      message(glue(":: Hold (uptrend) ::"))
    }
  }




  return(logic_sell)

}
