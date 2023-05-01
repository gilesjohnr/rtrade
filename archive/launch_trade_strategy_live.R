
rm(list=ls())
pkg <- c("rbinanceus", "httr", "jsonlite", "RCurl", "magrittr", "kableExtra", "DT", "httr", 
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
  interval_short = '5m', # in minutes
  interval_long = '1h', # in hours
  limit = pmin(1000, (60/3)*24),
  
  slope_threshold = 0,
  risk_ratio = 4, # Only on when in sideways trend (consolidating)
  bid_ask_ecdf_threshold = 0.005, 
  
  sleep_bt_runs = 15,
  sleep_bt_orders = 30,
  
  n_supertrend_long = 10,
  f_supertrend_long = 0.75,
  n_supertrend_short = 10,
  f_supertrend_short_buy = 0.8,
  f_supertrend_short_sell = 1.2,
  n_atr = 10,
  
  manual_sell_triggers = FALSE,
  manual_sell_trigger_low = -0.0025,
  manual_sell_trigger_high = 0.005,
  
  time_window = 10000,
  wait_and_see = TRUE
  
)

error_state <- FALSE


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
      t2 <- t1 + (timestep_duration*60) * (3/5)
      
      if (timestamp < t2) {
        wait_time <- difftime(t2, timestamp, units='secs')
        message(glue(":: Waiting {wait_time} seconds ::"))
        Sys.sleep(wait_time)
      }
      
    }
    
    #-------------------------------------------------------------------------
    # Get current data
    #-------------------------------------------------------------------------
    
    # Get daily data
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
    
    
    # Get hourly (or other small-scale time interval)
    d <- get_klines(symbol=par$symbol,
                    interval = par$interval_short,
                    limit = par$limit,
                    verbose = FALSE)
    
    d <- cbind(d, clean_dates(d$time_open))
    
    st <- calc_supertrend(HLC=d[,c("high","low","close")], n=par$n_supertrend_short, f=par$f_supertrend_short_buy)
    d$supertrend_1 <- st$supertrend
    d$supertrend_1_buy <- st$buy
    d$supertrend_1_sell <- st$sell
    
    st <- calc_supertrend(HLC=d[,c("high","low","close")], n=par$n_supertrend_short, f=par$f_supertrend_short_sell)
    d$supertrend_2 <- st$supertrend
    d$supertrend_2_buy <- st$buy
    d$supertrend_2_sell <- st$sell
    
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
      
      # High and low sell triggers are based on the ATR in the past 30 minutes.
      sel <- which(d$date_time >= max(d$date_time) - 60*60*1)
      sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T)*1.05
      sell_trigger_high <- -1*sell_trigger_low * par$risk_ratio
      
    }
    
    
    if (T) {
      
      dev.off(dev.list()["RStudioGD"]) 
      plot_candles(tail(d, n=5*12*2))
      lines(d$date_time, d$supertrend_1, type='l', lwd=1,5, col='pink2')
      lines(d$date_time, d$supertrend_2, type='l', lwd=1,5, col='purple3')
      #lines(d$date_time, d$supertrend_long, type='l', lwd=2, col='purple2', ylim=range(c(d$supertrend_2, d$mean), na.rm=T), main=par$symbol)
      
      abline(v=d$date_time[d$supertrend_1_buy == 1], col='green3')
      abline(v=d$date_time[d$supertrend_1_sell == 1], col='orange', lty=2)
      abline(v=d$date_time[d$supertrend_2_sell == 1], col='red3')
      abline(h=d$close[t], lty=2, lwd=0.5)
      
      #sel <- d$supertrend_long_delta > par$slope_threshold
      #abline(v=d$date_time[sel][d$supertrend_1_buy[sel] == 1], col='green3')
      #abline(v=d$date_time[sel][d$supertrend_2_sell[sel] == 1], col='red3')
      #
      #abline(v=d$date_time[!sel][d$supertrend_1_buy[!sel] == 1], col='green3', lty=2, lwd=0.25)
      #abline(v=d$date_time[!sel][d$supertrend_2_sell[!sel] == 1], col='red3', lty=2, lwd=0.25)
      
      #tmp_buy <- d[d$supertrend_1_buy == 1 & sel,]
      #tmp_sell <- d[d$supertrend_2_sell == 1 & sel,]
      #
      #if (nrow(tmp_buy) != nrow(tmp_sell)) {
      #  sel <- which.max(c(nrow(tmp_buy), nrow(tmp_sell)))
      #  if (sel == 1) tmp_buy <- tmp_buy[-nrow(tmp_buy),] else tmp_sell <- tmp_sell[-nrow(tmp_sell),]
      #}
      
      
      
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
      
      # BUY IF short term signal from supertrends
      logic_buy <- d$supertrend_1_buy[t] == 1 
      if (error_state) logic_buy <- logic_buy | d$supertrend_1_buy[t-1] == 1
      if (logic_buy) message(":: Logic BUY 1 (short term buy) ::")
      
      # Allow BUY IF not in long term downward trend
      #logic_buy_2 <- d$supertrend_long_delta[t] >= par$slope_threshold
      #if (logic_buy_2) message(":: Logic BUY 2 (not in long term downtrend) ::")
      #logic_buy <- logic_buy & logic_buy_2
      #
      ## BUY IF shorterm buy directly preceded longterm uptrend shift
      #if (!is.na(d$supertrend_long_delta[t])) {
      #  
      #  logic_buy_3 <- d$supertrend_long_delta[t] >= par$slope_threshold & d$supertrend_long_delta[t-1] < par$slope_threshold & any(d$supertrend_buy[(t-2):t] == 1)
      #  
      #  if (logic_buy_3) {
      #    logic_buy <- logic_buy_3
      #    message(":: Logic BUY 3 (short buy just before long buy) ::")
      #  }
      #  
      #}
      
      # BUY IF longterm inflexion point (be carefule when longterm is < 1h)
      #if (!is.na(d$supertrend_long_delta[t])) {
      #  
      #  logic_buy_4 <- d$supertrend_long_delta[t-1] < 0 & d$supertrend_long_delta[t] > 0
      #  
      #  if (logic_buy_4) {
      #    logic_buy <- logic_buy_4
      #    message(":: Logic BUY 4 (long term inflexion point) ::")
      #  }
      #  
      #}
      
      if (logic_buy) {
        
        tryCatch({
          
          # TRY 1
          tmp <- get_order_depth(par$symbol)
          bid <- tmp$bid_price[1]
          
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
            bid <- tmp$bid_price[1] + ((tmp$bid_price[1] - tmp$bid_price[2])*0.25)
            
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
      
      # Basic trigger from supertrend
      
      
      if (d$supertrend_2_sell[t] < d$mid[t]) {
        logic_sell <- d$supertrend_2_sell[t] == 1
      } else {
        logic_sell <- d$supertrend_1_sell[t] == 1
      }
      
      if (error_state) {
        
        if (d$supertrend_2_sell[t] < d$mid[t]) {
          logic_sell <- logic_sell | d$supertrend_2_sell[t-1] == 1
        } else {
          logic_sell <- logic_sell | d$supertrend_1_sell[t-1] == 1
        }
        
      }
      
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
      #if (!is.na(d$supertrend_long_delta[t])) {
      #  x <- d$supertrend_long_delta[t] > abs(par$slope_threshold)
      #  if (x) {
      #    logic_sell <- !x 
      #    message(":: Logic SELL 4 (uptrend, ignore sell) ::")
      #  }
      #}
      
      
      
      if (logic_sell) {
        
        tryCatch({
          
          # TRY 1
          
          sell_order <- create_order(symbol = par$symbol,
                                     side = 'SELL',
                                     type = 'LIMIT',
                                     price = get_order_depth(par$symbol)$ask_price[1],
                                     quantity = bal$free[bal$asset == par$asset],
                                     time_in_force = 'GTC',
                                     time_window = par$time_window)
          
          Sys.sleep(par$sleep_bt_orders)
          tmp <- get_order(par$symbol, sell_order$orderId)
          
          
          check <- !(tmp$status %in% c("FILLED", "PARTIALLY_FILLED")) | !('status' %in% names(tmp))
          if (check) { # TRY 2
            
            cancel_order(par$symbol, sell_order$orderId); Sys.sleep(2)
            bal <- get_account_balance()
            tmp <- get_order_depth(par$symbol)
            
            sell_order <- create_order(symbol = par$symbol,
                                       side = 'SELL',
                                       type = 'LIMIT',
                                       price = tmp$ask_price[1] - ((tmp$ask_price[2] - tmp$ask_price[1])*0.25),
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
    
    error_state <- FALSE
    error_count <- 0
    Sys.sleep(par$sleep_bt_runs)
    
  }, error = function(e) {
    
    error_state <- TRUE
    error_count <- error_count + 1
    message("An error occurred...")
    traceback()
    
  })
  
  if (error_count > 10) stop('More than 10 sequential errors')
}

