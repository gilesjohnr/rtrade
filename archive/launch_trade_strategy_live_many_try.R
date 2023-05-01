
rm(list=ls())
pkg <- c("Nancy", "httr", "jsonlite", "RCurl", "magrittr", "kableExtra", "DT", "httr", 
         'glue', 'lubridate', 'anytime', 'arrow',
         'TTR', 'quantmod', 'candlesticks', 'lcyanalysis')
invisible(lapply(pkg, library, character.only=TRUE))

path <- 'funcs'
for (i in list.files(path, pattern = "\\.[Rr]$")) source(file.path(path, i))

if (TRUE) {
  
  set_credentials(key_api = 'PxJjdXqkgu8I7ZZCgYVA9ESOZn06zHv6GRO6CKgpVzhBf0ndUKaLeVdyCjrCsQGv', 
                  key_secret = 'tG0T0XmXz4YTfoMd05J57exYeENtVdWWdu8e2ZLMKICj2bqDglutS49SVw8J4jJk')
  
  check_credentials()
  
}

par <- list(
  
  asset = 'BTC',
  symbol = 'BTCUSD',
  interval_short = '3m', # in minutes
  interval_long = '30m', # in hours
  limit = pmin(1000, (60/3)*24),
  slope_threshold = -0.01,
  sell_trigger_high = NA,
  sell_trigger_low = NA,
  bid_ask_ecdf_threshold = 0.005,
  sleep_bt_runs = 15,
  sleep_bt_orders = 30
  
)



while (TRUE) {
  
  tryCatch({
    
    timestamp <- timestamp_to_date(get_timestamp())
    
    #-------------------------------------------------------------------------
    # Get current data
    #-------------------------------------------------------------------------
    
    # Get daily data
    d_long <- get_klines(symbol=par$symbol,
                         interval = par$interval_long,
                         limit = par$limit,
                         verbose = FALSE)
    
    d_long <- cbind(d_long, clean_dates(d_long$time_open))
    
    st <- calc_supertrend(HLC=d_long[,c("high","low","close")], n=12, f=1)
    d_long$supertrend_long <- st$supertrend
    d_long$supertrend_buy_long <- st$buy
    d_long$supertrend_sell_long <- st$sell
    
    d_long$supertrend_long_delta <- NA
    for (i in 2:nrow(d_long)) d_long$supertrend_long_delta[i] <- d_long$supertrend_long[i] - d_long$supertrend_long[i-1]
    
    
    # Get hourly (or other small-scale time interval)
    d <- get_klines(symbol=par$symbol,
                    interval = par$interval_short,
                    limit = par$limit,
                    verbose = FALSE)
    
    d <- cbind(d, clean_dates(d$time_open))
    d <- add_indicators(d)
    
    st <- calc_supertrend(HLC=d[,c("high","low","close")], n=20, f=0.5)
    d$supertrend <- st$supertrend
    d$supertrend_buy <- st$buy
    d$supertrend_sell <- st$sell
    
    # Combine
    d <- merge(d, d_long[,c('date_time', 'supertrend_long', 'supertrend_long_delta')], by='date_time', all.x=T)
    
    
    d$supertrend_long <- na.approx(d$supertrend_long, na.rm = FALSE)
    d$supertrend_long_delta <- NA
    for (i in 2:nrow(d_long)) d$supertrend_long_delta[i] <- d$supertrend_long[i] - d$supertrend_long[i-1]
    
    d$supertrend_long_delta <- na.approx(d$supertrend_long_delta, na.rm = FALSE)
    for (i in 2:nrow(d)) if (is.na(d$supertrend_long_delta[i])) d$supertrend_long_delta[i] <- d$supertrend_long_delta[i-1]
    
    t <- which.max(d$date_time)
    current_price <- d$mean[t]
    
    # High and low sell triggers are based on the ATR in the past 24hrs.
    sel <- which(d$date_time >= d$date_time[t] - 24*60*60)
    par$sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T) * 1.01
    par$sell_trigger_high <- -1*par$sell_trigger_low * 3 # Risk ratio of 3:1
    
    
    
    if (F) {
      
      par(mfrow=c(2,1))
      
      plot_candles(tail(d, n=480))
      lines(d$date_time, d$mean)
      lines(d$date_time, d$supertrend, type='l', lwd=2, col='purple', ylim=range(c(d$supertrend, d$mean), na.rm=T), main=par$symbol)
      
      abline(v=d$date_time[d$supertrend_buy == 1], col='green3')
      abline(v=d$date_time[d$supertrend_sell == 1], col='red3')
      
      tmp_buy <- d[d$supertrend_buy == 1,]
      tmp_sell <- d[d$supertrend_sell == 1,]
      
      if (nrow(tmp_buy) != nrow(tmp_sell)) {
        sel <- which.max(c(nrow(tmp_buy), nrow(tmp_sell)))
        if (sel == 1) tmp_buy <- tmp_buy[-nrow(tmp_buy),] else tmp_sell <- tmp_sell[-nrow(tmp_sell),]
      }
      
      rates <- tmp_sell$mean/tmp_buy$mean - 1
      
      hist(rates, breaks=50, 
           main = glue("Mean = {round(mean(rates), 4)} \n 
                       Total trades = {length(rates)} ({round(sum(rates >=0)/length(rates),2)} win probability)"))
      abline(v=0, lwd=2)
      abline(v=median(rates), lty=1, lwd=2, col='blue')
      abline(v=quantile(rates, probs=c(0.0275, 0.975)), lty=2, lwd=2, col='blue')
      
      par(mfrow=c(1,1))
      
      x <- 500
      x*(1+median(rates))^(length(rates))
      
      for (i in 1:length(rates)) x <- x*(1+rates[i]); x
      
    }
    
    
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
      
      message(glue("{timestamp} | mode: BUY | {par$symbol} = {current_price}"))
      
      logic_buy <- d$supertrend_buy[t] == 1
      #if (logic_buy) message(":: Logic BUY 1 ::")
      
      #logic_buy_2 <- d$supertrend_long_delta[t] >= par$slope_threshold
      #if (logic_buy_2) message(":: Logic BUY 2 ::")
      #logic_buy <- logic_buy & logic_buy_2
      
      # ADD LOGIC: If correlation with example macd golden cross obs in past X limit days is above ~0.8
      
      if (logic_buy) {
        
        tryCatch({
          
          # TRY 1
          tmp <- get_order_depth(par$symbol)
          tmp$ecdf <- NA
          for (i in 1:nrow(tmp)) tmp$ecdf[i] <- sum(tmp$bid_quantity[1:i])/sum(tmp$bid_quantity)
          candidates <- tmp[tmp$ecdf < par$bid_ask_ecdf_threshold,]
          if (nrow(candidates) == 0) bid <- tmp$bid_price[1] else bid <- min(candidates$bid_price)
          head(tmp)
          
          buy_order <- create_order(symbol = par$symbol,
                                    side = 'BUY',
                                    type = 'LIMIT',
                                    price = bid,
                                    quantity = round(bal_usd/bid - 1e-05, 5),
                                    time_in_force = 'GTC',
                                    time_window = 10000)
          
          Sys.sleep(par$sleep_bt_orders+10)
          tmp <- get_order(par$symbol, buy_order$orderId)
          
          
          if (tmp$status != "FILLED") { # TRY 2
            
            cancel_order(par$symbol, buy_order$orderId); Sys.sleep(1)
            tmp <- get_order_depth(par$symbol)
            bid <- (tmp$bid_price[1] + tmp$bid_price[2]) / 2
            
            buy_order <- create_order(symbol = par$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 1e-05, 5),
                                      time_in_force = 'GTC',
                                      time_window = 10000)
            
            Sys.sleep(par$sleep_bt_orders)
            tmp <- get_order(par$symbol, buy_order$orderId)
            
            
            if (tmp$status != "FILLED") { # TRY 3
              
              cancel_order(par$symbol, buy_order$orderId); Sys.sleep(1)
              tmp <- get_order_depth(par$symbol)
              bid <- tmp$bid_price[1] + ((tmp$bid_price[1] - tmp$bid_price[2])*0.25)
              
              buy_order <- create_order(symbol = par$symbol,
                                        side = 'BUY',
                                        type = 'LIMIT',
                                        price = bid,
                                        quantity = round(bal_usd/bid - 1e-05, 5),
                                        time_in_force = 'GTC',
                                        time_window = 10000)
              
              Sys.sleep(par$sleep_bt_orders+15)
              tmp <- get_order(par$symbol, buy_order$orderId)
              
              
              if (tmp$status != "FILLED") { # TRY 4
                
                cancel_order(par$symbol, buy_order$orderId); Sys.sleep(1)
                
                buy_order <- create_order(symbol = par$symbol,
                                          side = 'BUY',
                                          type = 'MARKET',
                                          quantity = bal$free[bal$asset == par$asset],
                                          time_in_force = 'IOC',
                                          time_window = 10000)
                
                if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")
                
                
              } else {
                message("BUY LIMIT order FILLED (third attempt)")
              }
              
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
                                    time_window = 10000)
          
          if (buy_order$status == 'FILLED') message(" BUY MARKET order FILLED") else message(" BUY MARKET order placed")
          
          
        })
        
        
      }
      
    } else {
      
      #-------------------------------------------------------------------------
      # SELL strategy
      #-------------------------------------------------------------------------
      
      message(glue("{timestamp} | mode: SELL | {par$symbol} = {current_price}"))
      
      # Basic trigger from supertrend
      logic_sell <- d$supertrend_sell[t] == 1 
      if (logic_sell) message(":: Logic SELL 1 ::")
      
      # Second sell trigger: if price moves outside predetermined ranges (based on distribution of trade percent growth)
      
      tmp <- get_all_orders(par$symbol)
      tmp <- tmp[tmp$status == 'FILLED' & tmp$side == 'BUY',]
      tmp <- tmp[which.max(tmp$date_time),]  
      buy_price <- as.numeric(tmp$price)
      
      gain <- current_price / buy_price - 1
      logic_sell_2 <- gain > par$sell_trigger_high | gain < par$sell_trigger_low
      if (logic_sell_2) logic_sell <- logic_sell_2
      if (is.na(logic_sell)) logic_sell <- logic_sell_2
      if (logic_sell_2) message(":: Logic SELL 2 ::")
      
      ## Third sell trigger: if longer-range supertrend detects an extended downward trend
      logic_sell_3 <- d$supertrend_long_delta[t] < par$slope_threshold
      if (logic_sell_3) logic_sell <- logic_sell_3
      if (logic_sell_3) message(":: Logic SELL 3 ::")
      
      logic_sell_4 <- current_price > d$bb_hi[i] & d$supertrend_long_delta[i] < par$slope_threshold
      if (logic_sell_4) logic_sell <- logic_sell_4
      if (logic_sell_4) message(":: Logic SELL 4 ::")
      
      if (logic_sell) {
        
        tryCatch({
          
          # TRY 1
          
          tmp <- get_order_depth(par$symbol)
          tmp$ecdf <- NA
          for (i in 1:nrow(tmp)) tmp$ecdf[i] <- sum(tmp$ask_quantity[1:i])/sum(tmp$ask_quantity)
          candidates <- tmp[tmp$ecdf < par$bid_ask_ecdf_threshold,]
          if (nrow(candidates) == 0) ask <- tmp$ask_price[1] else ask <- max(candidates$ask_price)
          
          sell_order <- create_order(symbol = par$symbol,
                                     side = 'SELL',
                                     type = 'LIMIT',
                                     price = ask,
                                     quantity = round(bal$free[bal$asset == par$asset] - 1e-06, 5),
                                     time_in_force = 'GTC',
                                     time_window = 10000)
          
          Sys.sleep(par$sleep_bt_orders+10)
          tmp <- get_order(par$symbol, sell_order$orderId)
          
          
          if (tmp$status != "FILLED") { # TRY 2
            
            cancel_order(par$symbol, sell_order$orderId); Sys.sleep(1)
            tmp <- get_order_depth(par$symbol)
            
            sell_order <- create_order(symbol = par$symbol,
                                       side = 'SELL',
                                       type = 'LIMIT',
                                       price = ((tmp$ask_price[2] + tmp$ask_price[1]) / 2),
                                       quantity = round(bal$free[bal$asset == par$asset] - 1e-06, 5),
                                       time_in_force = 'GTC',
                                       time_window = 10000)
            
            Sys.sleep(par$sleep_bt_orders)
            tmp <- get_order(par$symbol, sell_order$orderId)
            
            
            if (tmp$status != "FILLED") { # TRY 3
              
              cancel_order(par$symbol, sell_order$orderId); Sys.sleep(1)
              tmp <- get_order_depth(par$symbol)
              
              sell_order <- create_order(symbol = par$symbol,
                                         side = 'SELL',
                                         type = 'LIMIT',
                                         price = tmp$ask_price[1] - ((tmp$ask_price[2] - tmp$ask_price[1])*0.25),
                                         quantity = round(bal$free[bal$asset == par$asset] - 1e-06, 5),
                                         time_in_force = 'GTC',
                                         time_window = 10000)
              
              Sys.sleep(par$sleep_bt_orders)
              tmp <- get_order(par$symbol, sell_order$orderId)
              
              
              if (tmp$status != "FILLED") { # TRY 4
                
                cancel_order(par$symbol, sell_order$orderId); Sys.sleep(1)
                
                sell_order <- create_order(symbol = par$symbol,
                                           side = 'SELL',
                                           type = 'MARKET',
                                           quantity = bal$free[bal$asset == par$asset],
                                           time_in_force = 'IOC',
                                           time_window = 10000)
                
                if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")
                
                
                
              } else {
                message("SELL LIMIT order FILLED (third attempt)")
              }
              
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
                                     time_window = 10000)
          
          if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")
          
          
        })
        
        
      }
      
    }
    
    Sys.sleep(par$sleep_bt_runs)
    
  }, error = function(e) {
    
    message("An error occurred...")
    traceback()
    
  })
  
}

