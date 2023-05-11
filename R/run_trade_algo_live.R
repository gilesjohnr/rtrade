run_trade_algo_live <- function(param, verbose=TRUE, display=TRUE) {

  error_state <- FALSE
  error_count <- 0

  n_time_steps <- 200

  while (TRUE) {

    tryCatch({

      timestamp <- timestamp_to_date(get_timestamp())

      if (param$wait_and_see) {

        timestep_duration <- as.integer(strsplit(param$interval_short, 'm')[[1]])

        tmp <- seq(0, 60, timestep_duration)
        tmp <- tmp[!tmp==60]
        tmp <- as.POSIXct(glue("{Sys.Date()} {hour(timestamp)}:{tmp}:00", origin = "1970-01-01", tz = "UTC"))

        t1 <- tmp[(tmp - timestamp) < 0]
        t1 <- t1[length(t1)]
        t2 <- t1 + (timestep_duration*60) * param$wait_and_see_prop

        if (timestamp < t2) {
          wait_time <- difftime(t2, timestamp, units='secs')
          message(glue(":: Waiting {wait_time} seconds ::"))
          Sys.sleep(wait_time)
        }

      }

      #-------------------------------------------------------------------------
      # Get current data
      #-------------------------------------------------------------------------

      d <- compile_data(param=param, limit=n_time_steps)
      t <- which.max(d$date_time)
      current_price <- d$close[t]


      #-------------------------------------------------------------------------
      # Get balances
      #-------------------------------------------------------------------------

      bal <- get_account_balance()
      bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
      bal_sym <- bal$USD[bal$asset == param$asset]
      if (length(bal_sym) == 0) bal_sym <- 0
      #message(paste0(capture.output(as.data.frame(bal[bal$asset %in% c(param$asset, 'USD'),])), collapse = "\n"))

      last_order <- get_last_order(param$symbol, status='FILLED')



      #-------------------------------------------------------------------------
      # Run trading strategy
      #-------------------------------------------------------------------------

      if (bal_usd > bal_sym & bal_usd > 1 | last_order$side == 'SELL') {

        #-------------------------------------------------------------------------
        # BUY strategy
        #-------------------------------------------------------------------------

        if (verbose) message(glue("{timestamp} | mode: BUY | {param$symbol} = {current_price}"))


        logic_buy <- get_buy_logic(d=d, t=t, param=param, live=TRUE, verbose=verbose)


        if (logic_buy) {

          tryCatch({

            # TRY 1

            cancel_all_orders(param$symbol)
            bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
            bid <- get_order_depth(param$symbol)$bid_price[2] + 0.01

            buy_order <- create_order(symbol = param$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 1e-05, 4),
                                      time_in_force = 'GTC',
                                      time_window = param$time_window)

            Sys.sleep(param$sleep_bt_orders)
            tmp <- get_order(param$symbol, buy_order$orderId)

            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) { # TRY 2

              cancel_order(param$symbol, buy_order$orderId); Sys.sleep(2)

              bal <- get_account_balance()
              bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
              bid <- get_order_depth(param$symbol)$bid_price[1] + 0.01

              buy_order <- create_order(symbol = param$symbol,
                                        side = 'BUY',
                                        type = 'LIMIT',
                                        price = bid,
                                        quantity = round(bal_usd/bid - 1e-05, 4),
                                        time_in_force = 'GTC',
                                        time_window = param$time_window)

              Sys.sleep(param$sleep_bt_orders)
              tmp <- get_order(param$symbol, buy_order$orderId)

              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) {

                # TRY 3

                cancel_order(param$symbol, buy_order$orderId); Sys.sleep(2)

                bal <- get_account_balance()
                bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)

                tmp <- get_order_depth(param$symbol, limit=1)
                bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.25

                buy_order <- create_order(symbol = param$symbol,
                                          side = 'BUY',
                                          type = 'LIMIT',
                                          price = bid,
                                          quantity = round(bal_usd/bid - 1e-05, 4),
                                          time_in_force = 'GTC',
                                          time_window = param$time_window)

                if (buy_order$status == 'FILLED') message("Try 3 BUY LIMIT order FILLED") else message("Try 3 BUY LIMIT order placed")


              } else {
                message("BUY LIMIT order FILLED (second attempt)")
              }



            } else {
              message("BUY LIMIT order FILLED (first attempt)")
            }


          }, error = function(e) {


            bal <- get_account_balance()
            bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)

            tmp <- get_order_depth(param$symbol, limit=1)
            bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.25

            buy_order <- create_order(symbol = param$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 1e-05, 4),
                                      time_in_force = 'GTC',
                                      time_window = param$time_window)

            if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")


          })

        }

      } else {

        #-------------------------------------------------------------------------
        # SELL strategy
        #-------------------------------------------------------------------------

        message(glue("{timestamp} | mode: SELL | {param$symbol} = {current_price}"))


        logic_sell <- get_sell_logic(d=d, t=t, param=param, live=TRUE, verbose=verbose)



        if (logic_sell) {

          tryCatch({

            # TRY 1

            cancel_all_orders(param$symbol)
            ask <- get_order_depth(param$symbol)$ask_price[2] - 0.01

            sell_order <- create_order(symbol = param$symbol,
                                       side = 'SELL',
                                       type = 'LIMIT',
                                       price = ask,
                                       quantity = round(bal$free[bal$asset == param$asset] - 1e-05, 4),
                                       time_in_force = 'GTC',
                                       time_window = param$time_window)

            Sys.sleep(param$sleep_bt_orders)
            tmp <- get_order(param$symbol, sell_order$orderId)


            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) {

              # TRY 2

              cancel_order(param$symbol, sell_order$orderId); Sys.sleep(2)
              bal <- get_account_balance()
              ask <- get_order_depth(param$symbol)$ask_price[1] - 0.01

              sell_order <- create_order(symbol = param$symbol,
                                         side = 'SELL',
                                         type = 'LIMIT',
                                         price = ask,
                                         quantity = round(bal$free[bal$asset == param$asset] - 1e-05, 4),
                                         time_in_force = 'GTC',
                                         time_window = param$time_window)

              Sys.sleep(param$sleep_bt_orders)
              tmp <- get_order(param$symbol, sell_order$orderId)

              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) {

                # TRY 3

                cancel_order(param$symbol, sell_order$orderId); Sys.sleep(2)

                bal <- get_account_balance()

                sell_order <- create_order(symbol = param$symbol,
                                           side = 'SELL',
                                           type = 'MARKET',
                                           quantity = round(bal$free[bal$asset == param$asset] - 1e-05, 4),
                                           time_in_force = 'IOC',
                                           time_window = param$time_window)

                if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")


              } else {
                message("SELL LIMIT order FILLED (second attempt)")
              }


            } else {
              message("SELL LIMIT order FILLED (first attempt)")
            }


          }, error = function(e) {

            bal <- get_account_balance()

            sell_order <- create_order(symbol = param$symbol,
                                       side = 'SELL',
                                       type = 'MARKET',
                                       quantity = round(bal$free[bal$asset == param$asset] - 1e-05, 4),
                                       time_in_force = 'IOC',
                                       time_window = param$time_window)

            if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")


          })

        }

      }


      if (display) {

        bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=20, sd=2))
        d$bb_avg <- bb$mavg
        d$bb_hi <- bb$up
        d$bb_lo <- bb$dn

        tmp <- get_all_orders(symbol = param$symbol)
        tmp <- tmp[tmp$status %in% c('FILLED', 'PARTIALLY FILLED'),]


        if ("RStudioGD" %in% names(dev.list())) dev.off(dev.list()["RStudioGD"])
        par(mar=c(3,3,2,5), xpd=FALSE)

        plot_candles(tail(d, n=n_time_steps),
                     main=glue("{param$symbol} | {d$date_time[t]} | Local: {format(Sys.time(), '%H:%M:%S')}"))
        lines(d$date_time, d$bb_hi, lwd=0.75, col='grey20', lty=3)
        lines(d$date_time, d$bb_lo, lwd=0.75, col='grey20', lty=3)
        lines(d$date_time, d$ema_buy_hold, lwd=2, col='cyan3')
        lines(d$date_time, d$ema_sell_hold, lwd=2, col='black')
        lines(d$date_time, d$supertrend_1, type='l', lwd=1.5, col='pink2')
        lines(d$date_time, d$supertrend_2, type='l', lwd=1.5, col='purple3')

        sel <- tmp$side == 'BUY'
        abline(v=tmp$date_time[sel], col='green3', lty=3)
        points(tmp$date_time[sel], y=tmp$price[sel], pch=24, col='green3')

        sel <- tmp$side == 'SELL'
        abline(v=tmp$date_time[sel], col='red3', lty=3)
        points(tmp$date_time[sel], y=tmp$price[sel], pch=25, col='red3')

        abline(h=d$close[t], lty=2, lwd=0.5)

        par(xpd=TRUE)
        text(x=max(d$date_time[t])+(60*10), y=d$close[t], label=d$close[t], pos=4)



      }


      error_state <-  FALSE
      error_count <- 0
      Sys.sleep(param$sleep_bt_runs)


    }, error = function(e) {

      message(paste(e, '\n'))
      error_state <- TRUE
      error_count <- error_count + 1


    })

    if (error_state & error_count >= 3) {

      # Email message

      stop('Stopping -- more than 3 sequential errors')

    }

  }


}
