run_trade_algo_paper <- function(param, time_stop=NULL, verbose=TRUE) {


  #-------------------------------------------------------------------------
  # Get current data
  #-------------------------------------------------------------------------

  d <- compile_data(param=param, time_stop=time_stop)
  t <- which.max(d$date_time)


  #-------------------------------------------------------------------------
  # Run trading strategy
  #-------------------------------------------------------------------------

  error_state <- FALSE
  trades <- data.frame()
  state <- NA

  for (i in 3:(nrow(d))) {

    if (is.na(state) | state == 'sell') {


      logic_buy <- get_buy_logic(d=d, t=i, param=param, verbose=verbose)


      if (logic_buy) {

        trade_units <- 0.003

        if (nrow(trades) > 0) trade_units <- trades[nrow(trades), 'total_value']/d$mid[i]


        tmp <- data.frame(trade_id = paste(unlist(strsplit(as.character(d$date_time[i]), '[^0-9]')), collapse = ''),
                          date_time = d$date_time[i],
                          action = 'buy',
                          price = d$close[i], # Or actual execution price from ticket
                          units = trade_units)

        tmp$total_value <- tmp$price*tmp$units
        trades <- rbind(trades, tmp)
        state <- 'buy'

      }

    } else if (state == 'buy') {


      logic_sell <- get_sell_logic(d=d, t=i, param=param, live=FALSE, trades=trades, verbose=verbose)


      if (logic_sell) {

        tmp <- data.frame(trade_id = trades[which.max(trades$date_time), 'trade_id'],
                          date_time = d$date_time[i],
                          action = 'sell',
                          price = d$close[i],
                          units = trades[which.max(trades$date_time), 'units'])

        tmp$total_value <- tmp$price * tmp$units
        trades <- rbind(trades, tmp)
        state <- 'sell'

      }

    } else {

      stop('Unrecognized previous buy/sell state')

    }


  }


  out <- list(n_trades = NA,
              win_prob = NA,
              percent_change = NA,
              trades = NA,
              data = d,
              param = param)


  if (nrow(trades) > 0) {

    trades$profit <- trades$rate <- NA

    for (i in unique(trades$trade_id)) {

      sel <- trades$trade_id == i
      tmp <- trades[sel,]

      if (nrow(tmp) == 2) {

        trades$profit[sel] <- tmp$total_value[tmp$action == 'sell'] - tmp$total_value[tmp$action == 'buy']
        trades$rate[sel] <- tmp$total_value[tmp$action == 'sell'] / tmp$total_value[tmp$action == 'buy'] - 1

      }

    }

    trades[trades$action == 'buy', c('profit', 'rate', 'pips')] <- NA
    out$trades <- trades

    tmp <- trades[trades$action == 'sell',]
    out$win_prob <- round(sum(tmp$rate > 0)/nrow(tmp),2)
    out$n_trades <- nrow(tmp)
    out$percent_change <- round((trades$total_value[nrow(trades)] / trades$total_value[1] - 1)*100, 2)

  }


  return(out)


}
