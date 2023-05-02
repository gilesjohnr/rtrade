get_buy_logic <- function(d, t, par, live=FALSE) {

  logic_buy <- FALSE

  if (!is.na(d$supertrend_1_buy[t])) {

    #logic_buy <- d$supertrend_1_buy[t] == 1
    logic_buy <- d$supertrend_1[t-1] >= d$mid[t-1] & d$supertrend_1[t] < d$mid[t] #| d$supertrend_2[t-1] >= d$mid[t-1] & d$supertrend_2[t] < d$mid[t]

    if (logic_buy) {

      message(":: Logic BUY 1 (short term buy) ::")

      if (FALSE) {

        message(":: Double-checking BUY trigger ::")
        Sys.sleep(10)

        # Get hourly (or other small-scale time interval)
        tmp <- get_klines(symbol=par$symbol,
                        interval = par$interval_short,
                        limit = par$limit,
                        verbose = FALSE)

        tmp <- cbind(tmp, clean_dates(tmp$time_open))

        st <- calc_supertrend(HLC=tmp[,c("high","low","close")], n=par$n_supertrend_short, f=par$f_supertrend_short_buy)
        tmp$supertrend_1_buy <- st$buy

        logic_buy <- tmp$supertrend_1_buy[t] == 1
        message(ifelse(logic_buy, 'Positive', 'False-positive'))

      }

    }

  }



  # BUY IF short-term buy directly preceded long-term uptrend shift
  if (!is.na(d$EMA_1_slope[t-1])) {

    logic_buy_3 <- d$EMA_1_slope[t] >= par$slope_threshold_buy & d$EMA_1_slope[t-1] < par$slope_threshold_buy & any(d$supertrend_1_buy[(t-2):t] == 1)

    if (logic_buy_3) {
      logic_buy <- logic_buy_3
      message(":: Logic BUY 2 (short buy just before trend change) ::")
    }

  }


  # BUY IF longterm inflexion point (be carefule when longterm is < 1h)
  if (!is.na(d$EMA_1_slope[t-1])) {

    logic_buy_4 <- d$EMA_1_slope[t-1] < 0 & d$EMA_1_slope[t] > 0

    if (logic_buy_4) {
      logic_buy <- logic_buy_4
      message(":: Logic BUY 2 (long term inflexion point) ::")
    }

  }


  # Only allow BUY if not in long term downward trend
  if (!is.na(d$EMA_1_slope[t])) {

    logic_buy_2 <- d$EMA_1_slope[t] >= par$slope_threshold_buy
    if (!logic_buy_2) message(":: Hold (long term downtrend) ::")
    logic_buy <- logic_buy & logic_buy_2

  }

  return(logic_buy)

}
