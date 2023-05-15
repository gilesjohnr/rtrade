get_buy_logic <- function(d, t, param, live=FALSE, verbose=TRUE) {

  logic_buy <- FALSE

  if (!is.na(d$supertrend_1[t]) & !is.na(d$sar[t])) {

    logic_buy <- d$supertrend_1_buy[t] == 1 & d$mid[t] > d$sar[t]

  }


  if (logic_buy) {

    if (verbose) message(":: Logic BUY (short buy) ::")

    if (live & param$double_check) {

      if (verbose) message(":: Double-checking BUY trigger ::")
      Sys.sleep(param$double_check_wait)

      d <- compile_data(param=param, limit=100)
      t <- which.max(d$time_close)

      logic_buy <- FALSE

      if (!is.na(d$supertrend_1[t]) & !is.na(d$sar[t-1])) {

        logic_buy <- d$supertrend_1_buy[t] == 1 & d$mid[t] > d$sar[t]

      }

      if (verbose) message(ifelse(logic_buy, 'Positive', 'False-positive'))

    }

  }




  return(logic_buy)

}
