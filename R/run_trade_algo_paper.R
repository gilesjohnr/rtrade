run_trade_algo_paper <- function(param, time_start, time_stop, last_trade=NULL, verbose=TRUE) {

  #-------------------------------------------------------------------------
  # Get current data
  #-------------------------------------------------------------------------

  d <- tryCatch({

  compile_data(param=param,
                    time_stop=date_to_timestamp(time_stop),
                    limit=1000)

  }, error = function(e) {

    Sys.sleep(2)

    compile_data(param=param,
                 time_stop=date_to_timestamp(time_stop),
                 limit=1000)

  })

  if (!(time_start >= min(d$date_time))) stop ("time_start out of bounds")
  if (!((time_stop - 60) <= max(d$date_time))) stop ("time_stop out of bounds")

  #d <- d[d$date_time >= time_start & d$date_time <= time_stop,]
  t <- which.max(d$date_time)
  all_dates <- d$date_time[d$date_time >= time_start & d$date_time <= time_stop]

  #-------------------------------------------------------------------------
  # Run trading strategy
  #-------------------------------------------------------------------------

  error_state <- FALSE

  if (is.null(last_trade)) {

    trades <- data.frame()
    state <- NA

    } else {

      trades <- last_trade[,c("trade_id", "date_time", "action", "price", "units", "total_value")]
      state <- last_trade$action

    }


  for (ii in all_dates) {

    i <- which(d$date_time == ii)

    if (is.na(state) | state == 'sell') {


      logic_buy <- get_buy_logic(d=d, t=i, param=param, verbose=verbose)


      if (logic_buy) {

        trade_units <- 0.003

        if (nrow(trades) > 0) trade_units <- trades[nrow(trades), 'total_value']/d$mid[i]


        tmp <- data.frame(trade_id = paste(unlist(strsplit(as.character(d$date_time[i]), '[^0-9]')), collapse = ''),
                          date_time = d$date_time[i],
                          action = 'buy',
                          price = mean(c(d$mid[i], d$close[i])), # Or actual execution price from ticket
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
                          price = mean(c(d$mid[i], d$close[i])),
                          units = trades[which.max(trades$date_time), 'units'])

        tmp$total_value <- tmp$price * tmp$units
        trades <- rbind(trades, tmp)
        state <- 'sell'

      }

    } else {

      stop('Unrecognized previous buy/sell state')

    }


  }

  d <- d[d$date_time >= time_start & d$date_time <= time_stop,]

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
