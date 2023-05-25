run_trade_algo_live <- function(param, verbose=TRUE, display=TRUE) {

  error_state <- FALSE
  error_count <- 0

  while (TRUE) {


    last_order <- get_last_order(param$symbol, status='FILLED')

    time_since_fit <- as.POSIXct(Sys.time()) -  param$run_date

    if (time_since_fit > 3 & last_order$side == 'SELL') {

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

      d <- compile_data(param=param, limit=360)
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
            bid <- tmp$bid_price + 0.01

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
            ask <- tmp$ask_price - 0.01

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

        if (F) d <- compile_data(param_best, limit=300)

        bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=as.integer(param$n_bbands), sd=param$sd_bbands))
        d$bb_avg <- bb$mavg
        d$bb_hi <- bb$up
        d$bb_lo <- bb$dn

        d$candle_color <- ifelse(d$open < d$close, 'red3', 'darkgreen')

        tmp <- get_all_orders(symbol = param$symbol)
        tmp <- tmp[tmp$status %in% c('FILLED', 'PARTIALLY FILLED'),]
        tmp$price <- as.numeric(tmp$price)
        tmp$price[tmp$price == 0] <- NA
        tmp <- tmp[tmp$date_time >= min(d$date_time),]

        n <- pmin(param$n_quantile_rsi, nrow(d)) - 1
        threshold_buy_rsi <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_buy_rsi, na.rm=TRUE)
        threshold_sell_rsi <- quantile(d$rsi_smooth[(nrow(d)-n):nrow(d)], probs=param$quantile_sell_rsi, na.rm=TRUE)

        if ("RStudioGD" %in% names(dev.list())) dev.off(dev.list()["RStudioGD"])


        g <- ggplot(d, aes(x=date_time)) +
          geom_vline(xintercept=as.POSIXct(tmp[tmp$side == 'BUY', 'date_time']), linetype="dotted", linewidth=0.4, color='green') +
          geom_vline(xintercept=as.POSIXct(tmp[tmp$side == 'SELL', 'date_time']), linetype="dotted", linewidth=0.4, color='red') +
          xlab('') +
          theme_minimal() +
          theme(legend.position='none',
                panel.grid.minor = element_blank(),
                plot.margin = margin(t = 2, r = 2, b = 2, l = 2))


        p <- suppressWarnings(cowplot::plot_grid(

          g +
            ggtitle(glue("{param$symbol} | {d$date_time[nrow(d)]} | Local: {format(Sys.time(), '%H:%M:%S')}")) +
            geom_ribbon(aes(ymin=bb_lo, ymax=bb_hi), color='grey', linewidth=0.25, alpha=0.1) +
            geom_line(aes(y=bb_avg), color='grey25', linewidth=0.4, na.rm=TRUE) +
            geom_segment(aes(x=date_time, xend=date_time, y=open, yend=close, color=candle_color), linewidth=1) +
            geom_segment(aes(x=date_time, xend=date_time, y=low, yend=high, color=candle_color), linewidth=0.3) +
            geom_line(aes(y=supertrend_1), color='blue2', linewidth=0.4, na.rm=TRUE) +
            geom_line(aes(y=sar), color='orange3', linewidth=0.7, linetype='dashed', na.rm=TRUE) +
            geom_hline(yintercept=d$close[nrow(d)], linetype="dashed", linewidth=0.1) +
            geom_point(data=tmp[tmp$side == 'BUY',], aes(x=date_time, y=price), shape=24, size=2, color='green2') +
            geom_point(data=tmp[tmp$side == 'SELL',], aes(x=date_time, y=price), shape=25, size=2, color='red2') +
            scale_color_manual(values=c("red3", "darkgreen")) +
            ylab('Price') +
            theme(axis.text.x=element_blank(),
                  plot.title=element_text(size=12))
          ,


          g +
            geom_bar(aes(x=date_time, y=macd_diff, fill=factor(macd_diff > 0)), stat='identity') +
            geom_line(aes(y=macd_signal), color='black', linewidth=0.75, na.rm=TRUE) +
            geom_line(aes(y=macd), color='purple2', linewidth=0.75, na.rm=TRUE) +
            geom_hline(yintercept=0, linewidth=0.25) +
            ylab('MACD') +
            scale_fill_manual(values=c("red2", "green3")) +
            theme(axis.text.x=element_blank())
          ,


          g +
            geom_line(aes(y=rsi), color='royalblue2', linewidth=0.5, na.rm=TRUE) +
            geom_line(aes(y=rsi_smooth), color='black', linewidth=0.6, na.rm=TRUE) +
            geom_hline(yintercept=c(30,70), linetype="dashed", linewidth=0.25) +
            geom_hline(yintercept=threshold_buy_rsi, linetype="dotted", linewidth=0.5, color='green3') +
            geom_hline(yintercept=threshold_sell_rsi, linetype="dotted", linewidth=0.5, color='red2') +
            ylab('RSI') +
            theme(axis.text.x=element_blank())
          ,

          g +
            geom_line(aes(y=adx), linewidth=0.75, na.rm=TRUE) +
            geom_line(aes(y=adx_pos), color='green4', linewidth=0.25, na.rm=TRUE) +
            geom_line(aes(y=adx_neg), color='red4', linewidth=0.25, na.rm=TRUE) +
            ylab('ADX')
          ,

          ncol=1,
          align='v',
          rel_heights = c(3,1,1,1)

        ))

        print(p)



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
