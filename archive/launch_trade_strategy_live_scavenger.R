
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
  
  trading_mode = 'default', # 'default', 'scavenger'
  asset = 'BTC',
  symbol = 'BTCUSD',
  interval_short = '5m', # in minutes
  interval_long = '1h', # in hours
  limit = pmin(1000, (60/3)*24),
  
  slope_threshold = -0.05,
  risk_ratio = 2, # Only on when in sideways trend (consolidating)
  bid_ask_ecdf_threshold = 0.005, 
  
  sleep_bt_runs = 30,
  sleep_bt_orders = 30,
  
  n_supertrend_long = 12,
  f_supertrend_long = 0.5,
  n_supertrend_short = 12,
  f_supertrend_short = 1.25,
  n_atr = 30,
  
  manual_sell_triggers = FALSE,
  manual_sell_trigger_low = -0.0025,
  manual_sell_trigger_high = 0.005
  
)

error_state <- FALSE


while (TRUE) {
  
  tryCatch({
    
    
    timestamp <- timestamp_to_date(get_timestamp())
    timestep_duration <- as.integer(strsplit(par$interval_short, 'm')[[1]])
    
    tmp <- seq(0, 60, timestep_duration)
    tmp <- tmp[!tmp==60]
    tmp <- as.POSIXct(glue("{Sys.Date()} {hour(timestamp)}:{tmp}:00", origin = "1970-01-01", tz = Sys.timezone()))
    
    t1 <- tmp[(tmp - timestamp) < 0]
    t1 <- t1[length(t1)]
    t2 <- t1 + (timestep_duration*60) * (3/5)
    
    if (timestamp < t2) {
      wait_time <- difftime(t2, timestamp, units='secs')
      message(glue(":: Waiting {wait_time} seconds ::"))
      Sys.sleep(wait_time)
    }
    
    
  
    #-------------------------------------------------------------------------
    # Get current data
    #-------------------------------------------------------------------------
    
    
    # Get daily data
    d_day <- get_klines(symbol=par$symbol,
                         interval = '1d',
                         limit = 60,
                         verbose = FALSE)
    
    d_day <- cbind(d_day, clean_dates(d_day$time_open))
    
    st <- calc_supertrend(HLC=d_day[,c("high","low","close")], n=7, f=1)
    d_day$supertrend_day <- st$supertrend
    d_day$supertrend_buy_day <- st$buy
    d_day$supertrend_sell_day <- st$sell

    
    # Get long term data
    d_long <- get_klines(symbol=par$symbol,
                         interval = par$interval_long,
                         limit = par$limit,
                         verbose = FALSE)
    
    d_long <- cbind(d_long, clean_dates(d_long$time_open))
    
    st <- calc_supertrend(HLC=d_long[,c("high","low","close")], n=par$n_supertrend_long, f=par$f_supertrend_long)
    d_long$supertrend_long <- st$supertrend
    d_long$supertrend_buy_long <- st$buy
    d_long$supertrend_sell_long <- st$sell
    
    d_long$supertrend_long_delta <- NA
    for (i in 2:nrow(d_long)) d_long$supertrend_long_delta[i] <- d_long$supertrend_long[i] - d_long$supertrend_long[i-1]
    
    
    # Get short term data
    d <- get_klines(symbol=par$symbol,
                    interval = par$interval_short,
                    limit = par$limit,
                    verbose = FALSE)
    
    d <- cbind(d, clean_dates(d$time_open))
    
    st <- calc_supertrend(HLC=d[,c("high","low","close")], n=par$n_supertrend_short, f=par$f_supertrend_short)
    d$supertrend <- st$supertrend
    d$supertrend_buy <- st$buy
    d$supertrend_sell <- st$sell
    
    d$atr <- as.data.frame(ATR(d[,c("high","low","close")], n=par$n_atr))$atr
    
    # Combine
    d <- merge(d, d_long[,c('date_time', 'supertrend_long', 'supertrend_long_delta')], by='date_time', all.x=T)
    
    
    d$supertrend_long <- na.approx(d$supertrend_long, na.rm = FALSE)
    d$supertrend_long_delta <- NA
    for (i in 2:nrow(d)) d$supertrend_long_delta[i] <- d$supertrend_long[i] - d$supertrend_long[i-1]
    
    d$supertrend_long_delta <- na.approx(d$supertrend_long_delta, na.rm = FALSE)
    for (i in 2:nrow(d)) if (is.na(d$supertrend_long_delta[i])) d$supertrend_long_delta[i] <- d$supertrend_long_delta[i-1]
    
    t <- which.max(d$date_time)
    current_price <- d$mean[t]
    
    
    if (par$manual_sell_triggers) {
      
      sell_trigger_low <- par$manual_sell_trigger_low
      sell_trigger_high <- par$manual_sell_trigger_high
      
    } else {
      
      # High and low sell triggers are based on the ATR in the past 24hrs.
      sel <- which(d$date_time >= max(d$date_time) - 24*60*60)
      sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T)*1.05
      sell_trigger_high <- -1*sell_trigger_low * par$risk_ratio
      
    }
    
    
    if (F) {

      par(mfrow=c(3,1))
      
      plot_candles(d_day)
      lines(d_day$date_time, d_day$mid)
      lines(d_day$date_time, d_day$supertrend_day, type='l', lwd=2, col='cyan3', ylim=range(c(d_day$supertrend_day, d_day$mean), na.rm=T), main=par$symbol)
      abline(v=min(d$date_time), lty=2, col='grey50')
      
      plot_candles(tail(d_long, n=96))
      lines(d_long$date_time, d_long$mid)
      lines(d_long$date_time, d_long$supertrend_long, type='l', lwd=2, col='purple2', ylim=range(c(d_day$supertrend_day, d_day$mean), na.rm=T), main=par$symbol)
      abline(v=min(d$date_time), lty=2, col='grey50')
      
      
      plot_candles(tail(d, n=300))
      lines(d$date_time, d$mean)
      lines(d$date_time, d$supertrend, type='l', lwd=2, col='orange2', ylim=range(c(d$supertrend, d$mean), na.rm=T), main=par$symbol)
      lines(d$date_time, d$supertrend_long, type='l', lwd=2, col='purple2', ylim=range(c(d$supertrend, d$mean), na.rm=T), main=par$symbol)
      
      sel <- d$supertrend_long_delta > par$slope_threshold
      abline(v=d$date_time[sel][d$supertrend_buy[sel] == 1], col='green3')
      abline(v=d$date_time[sel][d$supertrend_sell[sel] == 1], col='red3')
      
      abline(v=d$date_time[!sel][d$supertrend_buy[!sel] == 1], col='green3', lty=2, lwd=0.25)
      abline(v=d$date_time[!sel][d$supertrend_sell[!sel] == 1], col='red3', lty=2, lwd=0.25)
      
      tmp_buy <- d[d$supertrend_buy == 1 & sel,]
      tmp_sell <- d[d$supertrend_sell == 1 & sel,]
      
      if (nrow(tmp_buy) != nrow(tmp_sell)) {
        sel <- which.max(c(nrow(tmp_buy), nrow(tmp_sell)))
        if (sel == 1) tmp_buy <- tmp_buy[-nrow(tmp_buy),] else tmp_sell <- tmp_sell[-nrow(tmp_sell),]
      }
      
      par(mfrow=c(1,1))

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
      if (error_state) logic_buy <- logic_buy | d$supertrend_buy[t-1] == 1
      if (logic_buy) message(":: Logic BUY 1 (short term buy) ::")
      
      logic_buy_2 <- d$supertrend_long_delta[t] >= par$slope_threshold
      if (logic_buy_2) message(":: Logic BUY 2 (not in long term downtrend) ::")
      logic_buy <- logic_buy & logic_buy_2
      
      if (!is.na(d$supertrend_long_delta[t])) {
        
        logic_buy_3 <- d$supertrend_long_delta[t] >= par$slope_threshold & d$supertrend_long_delta[t-1] < par$slope_threshold & any(d$supertrend_buy[(t-2):t] == 1)
        
        if (logic_buy_3) {
          logic_buy <- logic_buy_3
          message(":: Logic BUY 3 (short buy just before long buy) ::")
        }
        
      }
      
      if (!is.na(d$supertrend_long_delta[t])) {
        
        logic_buy_4 <- d$supertrend_long_delta[t-1] < 0 & d$supertrend_long_delta[t] > 0
        
        if (logic_buy_4) {
          logic_buy <- logic_buy_4
          message(":: Logic BUY 4 (long term inflexion point) ::")
        }
        
      }
      
      if (logic_buy) {
        
        tryCatch({
          
          # TRY 1
          tmp <- get_order_depth(par$symbol)
          bid <- tmp$bid_price[2]
          
          buy_order <- create_order(symbol = par$symbol,
                                    side = 'BUY',
                                    type = 'LIMIT',
                                    price = bid,
                                    quantity = round(bal_usd/bid - 1e-04, 4),
                                    time_in_force = 'GTC',
                                    time_window = 10000)
          
          Sys.sleep(par$sleep_bt_orders+10)
          tmp <- get_order(par$symbol, buy_order$orderId)
          

          if (tmp$status != "FILLED") { # TRY 2
            
            cancel_order(par$symbol, buy_order$orderId); Sys.sleep(1)
            
            bal <- get_account_balance()
            bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)
            
            tmp <- get_order_depth(par$symbol)
            bid <- tmp$bid_price[1] + ((tmp$bid_price[1] - tmp$bid_price[2])*0.25)
            
            buy_order <- create_order(symbol = par$symbol,
                                      side = 'BUY',
                                      type = 'LIMIT',
                                      price = bid,
                                      quantity = round(bal_usd/bid - 1e-04, 4),
                                      time_in_force = 'GTC',
                                      time_window = 10000)
            
            Sys.sleep(par$sleep_bt_orders)
            tmp <- get_order(par$symbol, buy_order$orderId)
            
            
            if (tmp$status != "FILLED") { # TRY 3
              
              cancel_order(par$symbol, buy_order$orderId); Sys.sleep(1)
              
              buy_order <- create_order(symbol = par$symbol,
                                        side = 'BUY',
                                        type = 'MARKET',
                                        quantity = bal$free[bal$asset == par$asset],
                                        time_in_force = 'IOC',
                                        time_window = 10000)
              
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
      if (error_state) logic_sell <- logic_sell | d$supertrend_sell[t-1] == 1
      if (logic_sell) message(":: Logic SELL 1 (short sell) ::")

      # SELL IF price moves outside set risk ratio bounds
      x <- get_all_orders(par$symbol)
      x <- x[x$status == 'FILLED' & x$side == 'BUY',]
      x <- x[which.max(x$date_time),]  
      buy_price <- as.numeric(x$price)
      
      gain <- current_price / buy_price - 1
      x <- gain > sell_trigger_high | gain < sell_trigger_low
      if (x | is.na(logic_sell)) {
        logic_sell <- x
        message(glue(":: Logic SELL 2 (gain = {round(gain, 4)}) ::"))
      }
      
      ## SELL IF longer-range supertrend detects an extended downward trend
      #x <- d$supertrend_long_delta[t] < par$slope_threshold & d$supertrend_long_delta[t-1] >= par$slope_threshold
      #if (x) {
      #  logic_sell <- x
      #  message(":: Logic SELL 3 (downtrend detected) ::")
      #}
      
      # DO NOT SELL IF there is a strong longer term trend, hold even when there are short term dips
      if (!is.na(d$supertrend_long_delta[t])) {
        x <- d$supertrend_long_delta[t] > abs(par$slope_threshold)
        if (x) {
          logic_sell <- !x 
          message(":: Logic SELL 4 (uptrend, ignore sell) ::")
        }
      }
      
      
      
      if (logic_sell) {
        
        tryCatch({
          
          # TRY 1
        
          sell_order <- create_order(symbol = par$symbol,
                                     side = 'SELL',
                                     type = 'LIMIT',
                                     price = get_order_depth(par$symbol)$ask_price[2],
                                     quantity = bal$free[bal$asset == par$asset],
                                     time_in_force = 'GTC',
                                     time_window = 10000)
          
          Sys.sleep(par$sleep_bt_orders+10)
          tmp <- get_order(par$symbol, sell_order$orderId)
          
          

            if (tmp$status != "FILLED") { # TRY 2
              
              cancel_order(par$symbol, sell_order$orderId); Sys.sleep(1)
              bal <- get_account_balance()
              tmp <- get_order_depth(par$symbol)
              
              sell_order <- create_order(symbol = par$symbol,
                                         side = 'SELL',
                                         type = 'LIMIT',
                                         price = tmp$ask_price[1] - ((tmp$ask_price[2] - tmp$ask_price[1])*0.25),
                                         quantity = bal$free[bal$asset == par$asset],
                                         time_in_force = 'GTC',
                                         time_window = 10000)
              
              Sys.sleep(par$sleep_bt_orders)
              tmp <- get_order(par$symbol, sell_order$orderId)
              
              
              if (tmp$status != "FILLED") { # TRY 3
                
                cancel_order(par$symbol, sell_order$orderId); Sys.sleep(1)
                
                sell_order <- create_order(symbol = par$symbol,
                                           side = 'SELL',
                                           type = 'MARKET',
                                           quantity = bal$free[bal$asset == par$asset],
                                           time_in_force = 'IOC',
                                           time_window = 10000)
                
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
                                     time_window = 10000)
          
          if (sell_order$status == 'FILLED') message(" SELL MARKET order FILLED") else message(" SELL MARKET order placed")
          
          
        })
        
      }
      
    }
    
    error_state <- FALSE
    Sys.sleep(par$sleep_bt_runs)
    
  }, error = function(e) {
    
    error_state <- TRUE
    message("An error occurred...")
    traceback()
    
  })
  
}

