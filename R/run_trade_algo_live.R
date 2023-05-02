run_trade_algo_live <- function(par, verbose=TRUE, display=TRUE) {
  
  error_state <- FALSE
  error_count <- 0
  
  while (TRUE) {
    
    tryCatch({
      
      timestamp <- timestamp_to_date(get_timestamp())
      
      if (par$wait_and_see) {
        
        timestep_duration <- as.integer(strsplit(par$interval_short, 'm')[[1]])
        
        tmp <- seq(0, 60, timestep_duration)
        tmp <- tmp[!tmp==60]
        tmp <- as.POSIXct(glue("{Sys.Date()} {hour(timestamp)}:{tmp}:00", origin = "1970-01-01", tz = Sys.timezone()))
        
        t1 <- tmp[(tmp - timestamp) < 0]
        t1 <- t1[length(t1)]
        t2 <- t1 + (timestep_duration*60) * par$wait_and_see_prop
        
        if (timestamp < t2) {
          wait_time <- difftime(t2, timestamp, units='secs')
          message(glue(":: Waiting {wait_time} seconds ::"))
          Sys.sleep(wait_time)
        }
        
      }
      
      #-------------------------------------------------------------------------
      # Get current data
      #-------------------------------------------------------------------------
      
      d <- compile_data(par)
      
      t <- which.max(d$date_time)
      current_price <-mean(c(d$mean[t], d$mid[t]))

      
      #-------------------------------------------------------------------------
      # Get balances
      #-------------------------------------------------------------------------
      
      bal <- get_account_balance()
      bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
      bal_sym <- bal$USD[bal$asset == par$asset]
      if (length(bal_sym) == 0) bal_sym <- 0
      #message(paste0(capture.output(as.data.frame(bal[bal$asset %in% c(par$asset, 'USD'),])), collapse = "\n"))
      
      last_order <- get_last_order(par$symbol, status='FILLED')
      
      #-------------------------------------------------------------------------
      # Run trading strategy
      #-------------------------------------------------------------------------
      
      if (bal_usd > bal_sym & bal_usd > 1 | last_order$side == 'SELL') {
        
        #-------------------------------------------------------------------------
        # BUY strategy
        #-------------------------------------------------------------------------
        
        if (verbose) message(glue("{timestamp} | mode: BUY | {par$symbol} = {current_price}"))

        
        logic_buy <- get_buy_logic(d, t, par, live=TRUE)

        
        if (logic_buy) {
          
          tryCatch({
            
            # TRY 1
            bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
            #bid <- get_order_depth(par$symbol)$bid_price[1]
            bid <- mean(c(d$close[t], d$mid[t]))
            
            buy_order <- create_order(symbol = par$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 1e-04, 4),
                                      time_in_force = 'GTC',
                                      time_window = par$time_window)
            
            Sys.sleep(par$sleep_bt_orders)
            tmp <- get_order(par$symbol, buy_order$orderId)
            
            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) { # TRY 2
              
              cancel_order(par$symbol, buy_order$orderId); Sys.sleep(2)
              
              bal <- get_account_balance()
              bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
              
              tmp <- get_order_depth(par$symbol)
              #bid <- tmp$bid_price[1] + ((tmp$bid_price[1] - tmp$bid_price[2])*0.25)
              bid <- get_order_depth(par$symbol)$bid_price[1]
              
              buy_order <- create_order(symbol = par$symbol,
                                        side = 'BUY',
                                        type = 'LIMIT',
                                        price = bid,
                                        quantity = round(bal_usd/bid - 1e-04, 4),
                                        time_in_force = 'GTC',
                                        time_window = par$time_window)
              
              Sys.sleep(par$sleep_bt_orders)
              tmp <- get_order(par$symbol, buy_order$orderId)
              
              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) { # TRY 3
                
                cancel_order(par$symbol, buy_order$orderId); Sys.sleep(2)
                
                buy_order <- create_order(symbol = par$symbol,
                                          side = 'BUY',
                                          type = 'MARKET',
                                          quantity = bal$free[bal$asset == par$asset],
                                          time_in_force = 'IOC',
                                          time_window = par$time_window)
                
                if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")
                
                
              } else {
                message("BUY LIMIT order FILLED (second attempt)")
              }
              
              
              
            } else {
              message("BUY LIMIT order FILLED (first attempt)")
            }
            
            
          }, error = function(e) {
            
            
            buy_order <- create_order(symbol = par$symbol,
                                      side = 'BUY',
                                      type = 'MARKET',
                                      quantity = bal$free[bal$asset == par$asset],
                                      time_in_force = 'IOC',
                                      time_window = par$time_window)
            
            if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")
            
            
          })
          
        }
        
      } else {
        
        #-------------------------------------------------------------------------
        # SELL strategy
        #-------------------------------------------------------------------------
        
        message(glue("{timestamp} | mode: SELL | {par$symbol} = {current_price}"))
        
        
        logic_sell <- get_sell_logic(d, t, par)
        
        
        
        if (logic_sell) {
          
          tryCatch({
            
            # TRY 1
            #ask <- get_order_depth(par$symbol)$ask_price[1]
            ask <- mean(c(d$close[t], d$mid[t]))
            
            sell_order <- create_order(symbol = par$symbol,
                                       side = 'SELL',
                                       type = 'LIMIT',
                                       price = ask,
                                       quantity = bal$free[bal$asset == par$asset],
                                       time_in_force = 'GTC',
                                       time_window = par$time_window)
            
            Sys.sleep(par$sleep_bt_orders)
            tmp <- get_order(par$symbol, sell_order$orderId)
            
            
            check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
            if (check) { # TRY 2
              
              cancel_order(par$symbol, sell_order$orderId); Sys.sleep(2)
              bal <- get_account_balance()
              #tmp <- get_order_depth(par$symbol)
              #ask <- tmp$ask_price[1] - ((tmp$ask_price[2] - tmp$ask_price[1])*0.25)
              ask <- get_order_depth(par$symbol)$ask_price[1]
              
              sell_order <- create_order(symbol = par$symbol,
                                         side = 'SELL',
                                         type = 'LIMIT',
                                         price = ask,
                                         quantity = bal$free[bal$asset == par$asset],
                                         time_in_force = 'GTC',
                                         time_window = par$time_window)
              
              Sys.sleep(par$sleep_bt_orders)
              tmp <- get_order(par$symbol, sell_order$orderId)
              
              check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
              if (check) { # TRY 3
                
                cancel_order(par$symbol, sell_order$orderId); Sys.sleep(2)
                
                sell_order <- create_order(symbol = par$symbol,
                                           side = 'SELL',
                                           type = 'MARKET',
                                           quantity = bal$free[bal$asset == par$asset],
                                           time_in_force = 'IOC',
                                           time_window = par$time_window)
                
                if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")
                
                
              } else {
                message("SELL LIMIT order FILLED (second attempt)")
              }
              
              
            } else {
              message("SELL LIMIT order FILLED (first attempt)")
            }
            
            
          }, error = function(e) {
            
            
            sell_order <- create_order(symbol = par$symbol,
                                       side = 'SELL',
                                       type = 'MARKET',
                                       quantity = bal$free[bal$asset == par$asset],
                                       time_in_force = 'IOC',
                                       time_window = par$time_window)
            
            if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")
            
            
          })
          
        }
        
      }
      
      
      tmp <- get_all_orders(symbol = par$symbol)
      tmp <- tmp[tmp$status %in% c('FILLED', 'PARTIALLY FILLED'),]
      
      
      if (display) {
        
        
        if ("RStudioGD" %in% names(dev.list())) dev.off(dev.list()["RStudioGD"]) 
        par(mar=c(3,3,2,5), xpd=FALSE)
        
        plot_candles(tail(d, n=12*6), 
                     main=glue("{par$symbol} | {d$date_time[t]} | Local: {format(Sys.time(), '%H:%M:%S')}"))
        lines(d$date_time, d$EMA_1, lwd=2)
        lines(d$date_time, d$EMA_2, lwd=2, col='grey50')
        lines(d$date_time, d$supertrend_1, type='l', lwd=1.5, col='pink')
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
      
      
      error_state <- FALSE
      Sys.sleep(par$sleep_bt_runs)
      
    }, error = function(e) {
      
      error_state <- TRUE
      error_count <- error_count + 1
      message("An error occurred...")
      traceback()
      
    })
    
    if (error_count > 10) stop('More than 10 sequential errors')
  }
  
  
  
  
}