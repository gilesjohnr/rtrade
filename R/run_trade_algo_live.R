run_trade_algo_live <- function(param, verbose=TRUE, display=TRUE) {

  error_state <- FALSE
  error_count <- 0

  while (TRUE) {

    tmp <- get_all_orders(symbol = param$symbol, limit=10)
    tmp <- tmp[tmp$status %in% c('FILLED', 'PARTIALLY FILLED'),]
    last_order <- tmp[which.max(tmp$date_time),]

    time_since_fit <- difftime(Sys.time(), param$run_date, units='mins')

    if (time_since_fit > 60*1 & last_order$side == 'SELL') {

      message(":: Refitting trade algo ::")
      tmp <- fit_trade_algo(param)
      param <- tmp$param_best
      rm(tmp)

    }

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
      # Get balances
      #-------------------------------------------------------------------------

      bal <- get_account_balance()
      bal_usd <- round(bal$free[bal$asset == 'USD'] - 6e-05, 4)
      bal_sym <- bal$USD[bal$asset == param$asset]
      if (length(bal_sym) == 0) bal_sym <- 0




      #-------------------------------------------------------------------------
      # Get current data
      #-------------------------------------------------------------------------

      d <- compile_data(param=param, limit=360+param$n_ema_long)
      t <- which.max(d$date_time)



      #-------------------------------------------------------------------------
      # Run trading strategy
      #-------------------------------------------------------------------------

      if (bal_usd > bal_sym & bal_usd > 1 | last_order$side == 'SELL') {

        #-------------------------------------------------------------------------
        # BUY strategy
        #-------------------------------------------------------------------------

        if (verbose) {
          if (param$hold) {
            message(glue("{timestamp} | mode: HOLD"))
          } else {
            message(glue("{timestamp} | mode: BUY"))
          }
        }

        logic_buy <- FALSE
        if (!param$hold) logic_buy <- get_buy_logic(d=d, t=t, param=param, live=TRUE, verbose=verbose)


        if (logic_buy) {

          tryCatch({

            # TRY 1

            cancel_all_orders(param$symbol)

            tmp <- get_order_depth(param$symbol, limit=1)
            bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.025


            buy_order <- create_order(symbol = param$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 6e-05, 4),
                                      time_in_force = 'GTC',
                                      time_window = param$time_window)

            Sys.sleep(param$sleep_bt_orders)
            tmp <- get_order(param$symbol, buy_order$orderId)

            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) { # TRY 2

              cancel_order(param$symbol, buy_order$orderId)

              bal <- get_account_balance()
              bal_usd <- round(bal$free[bal$asset == 'USD'] - 6e-05, 4)

              tmp <- get_order_depth(param$symbol, limit=1)
              bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.05

              buy_order <- create_order(symbol = param$symbol,
                                        side = 'BUY',
                                        type = 'LIMIT',
                                        price = bid,
                                        quantity = round(bal_usd/bid - 6e-05, 4),
                                        time_in_force = 'GTC',
                                        time_window = param$time_window)

              Sys.sleep(param$sleep_bt_orders)
              tmp <- get_order(param$symbol, buy_order$orderId)

              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) {

                # TRY 3

                cancel_order(param$symbol, buy_order$orderId)

                bal <- get_account_balance()
                bal_usd <- round(bal$free[bal$asset == 'USD'] - 6e-05, 4)

                tmp <- get_order_depth(param$symbol, limit=1)
                bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.05

                buy_order <- create_order(symbol = param$symbol,
                                          side = 'BUY',
                                          type = 'LIMIT',
                                          price = bid,
                                          quantity = round(bal_usd/bid - 6e-05, 4),
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

            cancel_all_orders(param$symbol)

            bal <- get_account_balance()
            bal_usd <- round(bal$free[bal$asset == 'USD'] - 6e-05, 4)

            tmp <- get_order_depth(param$symbol, limit=1)
            bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.05

            buy_order <- create_order(symbol = param$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 6e-05, 4),
                                      time_in_force = 'GTC',
                                      time_window = param$time_window)

            if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")


          })

        }

      } else {

        #-------------------------------------------------------------------------
        # SELL strategy
        #-------------------------------------------------------------------------

        if (verbose) {
          if (param$hold) {
            message(glue("{timestamp} | mode: HOLD"))
          } else {
            message(glue("{timestamp} | mode: SELL"))
          }
        }

        logic_sell <- FALSE
        if (!param$hold) logic_sell <- get_sell_logic(d=d, t=t, param=param, live=TRUE, verbose=verbose)


        if (logic_sell) {

          tryCatch({

            # TRY 1

            cancel_all_orders(param$symbol)

            tmp <- get_order_depth(param$symbol, limit=1)
            ask <- tmp$ask_price - (tmp$ask_price - tmp$bid_price)*0.025

            sell_order <- create_order(symbol = param$symbol,
                                       side = 'SELL',
                                       type = 'LIMIT',
                                       price = ask,
                                       quantity = round(bal$free[bal$asset == param$asset] - 6e-05, 4),
                                       time_in_force = 'GTC',
                                       time_window = param$time_window)

            Sys.sleep(param$sleep_bt_orders)
            tmp <- get_order(param$symbol, sell_order$orderId)


            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) {

              # TRY 2

              cancel_order(param$symbol, sell_order$orderId)
              bal <- get_account_balance()

              tmp <- get_order_depth(param$symbol, limit=1)
              ask <- tmp$ask_price - (tmp$ask_price - tmp$bid_price)*0.05

              sell_order <- create_order(symbol = param$symbol,
                                         side = 'SELL',
                                         type = 'LIMIT',
                                         price = ask,
                                         quantity = round(bal$free[bal$asset == param$asset] - 6e-05, 4),
                                         time_in_force = 'GTC',
                                         time_window = param$time_window)

              Sys.sleep(param$sleep_bt_orders)
              tmp <- get_order(param$symbol, sell_order$orderId)

              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) {

                # TRY 3

                cancel_order(param$symbol, sell_order$orderId)

                bal <- get_account_balance()

                sell_order <- create_order(symbol = param$symbol,
                                           side = 'SELL',
                                           type = 'MARKET',
                                           quantity = round(bal$free[bal$asset == param$asset] - 6e-05, 4),
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

            cancel_all_orders(param$symbol)
            bal <- get_account_balance()

            sell_order <- create_order(symbol = param$symbol,
                                       side = 'SELL',
                                       type = 'MARKET',
                                       quantity = round(bal$free[bal$asset == param$asset] - 6e-05, 4),
                                       time_in_force = 'IOC',
                                       time_window = param$time_window)

            if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")


          })

        }

      }


      if (display) {

        plot_live_chart(param=param, d=d[!is.na(d$ema_long),])

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
