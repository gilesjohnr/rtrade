
rm(list=ls())
pkg <- c("rbinanceus", "httr", "jsonlite", "RCurl", "magrittr", "kableExtra", "DT", "httr", 
         'glue', 'lubridate', 'anytime', 'arrow',
         'TTR', 'quantmod', 'candlesticks', 'lcyanalysis')
invisible(lapply(pkg, library, character.only=TRUE))

path <- 'funcs'
for (i in list.files(path, pattern = "\\.[Rr]$")) source(file.path(path, i))

par <- list(
  
  trade_mode = 'default', # 'default' or 'scavenger'
  symbol = 'BTCUSD',
  interval_short = '5m', # in minutes
  interval_long = '1h', # in hours
  limit = pmin(20*24, 1000),
  slope_threshold = -0.01,
  risk_ratio = 2,
  manual_sell_triggers = F,
  manual_sell_trigger_low = -0.002,
  manual_sell_trigger_high = 0.004
  
)

#-------------------------------------------------------------------------
# Get current data
#-------------------------------------------------------------------------

# Get daily data
d_long <- get_klines(symbol=par$symbol,
                     interval = par$interval_long,
                     limit = par$limit,
                     verbose = FALSE)

d_long <- cbind(d_long, clean_dates(d_long$time_open))

# Adjust for real time lag
d_long$date_time <- d_long$date_time + 60*60

st <- calc_supertrend(HLC=d_long[,c("high","low","close")], n=10, f=1)
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

atr <- as.data.frame(ATR(d[,c("high","low","close")], n=10))
d$atr <- atr$atr

st <- calc_supertrend(HLC=d[,c("high","low","close")], n=10, f=0.5)
d$supertrend_1 <- st$supertrend
d$supertrend_1_buy <- st$buy
d$supertrend_1_sell <- st$sell

st <- calc_supertrend(HLC=d[,c("high","low","close")], n=10, f=1.5)
d$supertrend_2 <- st$supertrend
d$supertrend_2_buy <- st$buy
d$supertrend_2_sell <- st$sell

# Combine
d <- merge(d, d_long[,c('date_time', 'supertrend_long', 'supertrend_long_delta')], by='date_time', all.x=T)


d$supertrend_long <- na.approx(d$supertrend_long, na.rm = FALSE)
d$supertrend_long_delta <- NA
for (i in 2:nrow(d)) d$supertrend_long_delta[i] <- d$supertrend_long[i] - d$supertrend_long[i-1]

d$supertrend_long_delta <- na.approx(d$supertrend_long_delta, na.rm = FALSE)
for (i in 2:nrow(d)) if (is.na(d$supertrend_long_delta[i])) d$supertrend_long_delta[i] <- d$supertrend_long_delta[i-1]


if (par$manual_sell_triggers) {
  
  sell_trigger_low <- par$manual_sell_trigger_low
  sell_trigger_high <- par$manual_sell_trigger_high
  
} else {
  
  sel <- which(d$date_time >= max(d$date_time) - 60*60*1)
  sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T)*1.1
  sell_trigger_high <- -1*sell_trigger_low * par$risk_ratio
  
}

#-------------------------------------------------------------------------
# Run trading strategy
#-------------------------------------------------------------------------

trades <- data.frame()
state <- NA



for (i in 3:(nrow(d)-1)) {
  
  if (is.na(state) | state == 'sell') {
    
    # BUY IF short term signal from supertrends
    logic_buy <- d$supertrend_1_buy[i] == 1
    #logic_buy <- F
    #if (!is.na(d$supertrend_1[i])) logic_buy <- d$mid[i] >= d$supertrend_1[i] & d$mid[i-1] < d$supertrend_1[i-1]
    #if (!is.na(d$macd_signal[i-1])) logic_buy <- d$macd[i] >= d$macd_signal[i] & d$macd[i-1] < d$macd_signal[i-1]
    
    
    # ONLY BUY IF not in a longer term downward trend
    #if (!is.na(d$supertrend_long_delta[i])) logic_buy <- logic_buy & d$supertrend_long_delta[i] >= par$slope_threshold
    #
    ## BUY IF shorterm buy directly preceded longterm uptrend shift
    #if (!is.na(d$supertrend_long_delta[i])) {
    #  
    #  logic_buy_3 <- d$supertrend_long_delta[i] >= par$slope_threshold & d$supertrend_long_delta[i-1] < par$slope_threshold & any(d$supertrend_buy[(i-2):i] == 1)
    #  if (logic_buy_3) logic_buy <- logic_buy_3
    #  
    #}
  
    # BUY IF longterm inflexion point (be carefule when longterm is < 1h)
    #if (!is.na(d$supertrend_long_delta[i-1])) {
    #  
    #  logic_buy_4 <- d$supertrend_long_delta[i-1] < 0 & d$supertrend_long_delta[i] > 0
    #  if (logic_buy_4) logic_buy <- logic_buy_4
    #  
    #}
    
    
    if (logic_buy) {
      
      #trade_limit_price <- get_best_order_price(par$symbol)$bid_price*1.0001
      #trade_units <- get_account_balance('USD')$free / trade_limit_price
      trade_units <- 0.003
      
      if (nrow(trades) > 0) trade_units <- trades[nrow(trades), 'total_value']/d$mean[i]
      
      # Submit buy order
      
      # Confirm buy order
      
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
    
    tmp_buy_order <- trades[which.max(trades$date_time),]
    current_price <- d$mid[i]
    
    logic_sell <- d$supertrend_1_sell[i] == 1
    
    #if (!is.na(d$macd[i])) 
      #logic_sell <- d$macd[i] <= d$macd_signal[i] & d$macd[i-1] > d$macd_signal[i-1]
    
    
    # Basic trigger from supertrend
    #if (d$supertrend_2_sell[i] < d$mid[i]) {
    #logic_sell <- d$supertrend_2_sell[i] == 1 
    ##if (!is.na(d$supertrend_2[i])) logic_sell <- d$mid[i] <= d$supertrend_2[i] & d$mid[i-1] > d$supertrend_2[i-1]
#
    #} else {
    #  logic_sell <- d$supertrend_1_sell[i] == 1
    #  #if (!is.na(d$supertrend_1[i])) logic_sell <- d$mid[i] <= d$supertrend_1[i] & d$mid[i-1] > d$supertrend_1[i-1]
    #}
    
    # Second sell trigger if price moves outside predetermined ranges (based on distribution of trade percent growth)
    logic_sell_2 <- (current_price / tmp_buy_order$price - 1) > sell_trigger_high | (current_price / tmp_buy_order$price - 1) < sell_trigger_low
    if (logic_sell_2) logic_sell <- logic_sell_2
    if (is.na(logic_sell)) logic_sell <- logic_sell_2
    
    # If there is a strong longer term upward trend, hold even when there are short term dips
    #if (!is.na(d$supertrend_long_delta[i])) {
    #  tmp <- d$supertrend_long_delta[i] > abs(par$slope_threshold)
    #  if (tmp) logic_sell <- !tmp
    #}
    
    ### Third sell trigger if longer range (daily) supertrend detects am extended downward trend
    #if (!is.na(d$supertrend_long_delta[i])) {
    #  logic_sell_3 <- d$supertrend_long_delta[i] < par$slope_threshold & d$supertrend_long_delta[i-1] > par$slope_threshold
    #  if (logic_sell_3) logic_sell <- logic_sell_3
    #} 
    
    
    
    if (logic_sell) {
      
      # Submit sell order
      
      # Confirm sell order
      
      tmp <- data.frame(trade_id = tmp_buy_order$trade_id,
                        date_time = d$date_time[i],
                        action = 'sell',
                        price = d$close[i],
                        units = tmp_buy_order$units)
      
      tmp$total_value <- tmp$price*tmp$units
      trades <- rbind(trades, tmp)
      state <- 'sell'
      
    }
    
  } else {
    
    stop('Unrecognized previous buy/sell state')
    
  }
  
  
}





trades$profit <- trades$pips <- trades$rate <- NA

for (i in unique(trades$trade_id)) {
  
  sel <- trades$trade_id == i
  tmp <- trades[sel,]
  
  if (nrow(tmp) == 2) {
    trades$profit[sel] <- tmp$total_value[tmp$action == 'sell'] - tmp$total_value[tmp$action == 'buy']
    
    trades$rate[sel] <- tmp$total_value[tmp$action == 'sell'] / tmp$total_value[tmp$action == 'buy'] - 1
    trades$pips[sel] <- trades$rate[sel]/0.0001
  }
  
}

trades[trades$action == 'buy', c('profit', 'rate', 'pips')] <- NA

tot_days <- max(as.Date(trades$date_time)) - min(as.Date(trades$date_time))
tot_trades <- sum(trades$action == 'sell')
tot_prop_change <- round((trades$total_value[nrow(trades)] / trades$total_value[1] - 1)*100, 2)

msg <- glue("{tot_trades} trades in {tot_days} days ({round(tot_trades/as.numeric(tot_days),2)} per day)
              {tot_prop_change}% growth ({round(tot_prop_change/tot_trades, 2)}% per trade)")

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(3,1))

plot(d$date_time, d$supertrend_long, type='l', col='blue2', ylim=range(c(d$supertrend_long, d$mean), na.rm=T), main=par$symbol)
lines(d$date_time, d$high)
abline(v=trades$date_time[trades$action == 'buy'], col='green3')
abline(v=trades$date_time[trades$action == 'sell'], col='red3')

plot(trades$date_time, trades$total_value, pch=19, cex=1, xlim=range(d$date_time), main=msg)
lines(trades$date_time, trades$total_value)

tmp <- trades[trades$action == 'sell',]
hist(tmp$rate, breaks=30, main=glue("Median rate = {format(round(median(tmp$rate),5), scientific=F)}
                                    Median pips = {round(median(tmp$pips),2)}
                                      Win probability = {round(sum(tmp$rate > 0)/nrow(tmp),2)}"))
abline(v=0)
abline(v=median(tmp$rate), lty=1, col='blue')
abline(v=quantile(tmp$rate, probs=c(0.0275, 0.975)), lty=2, col='blue')

par(mfrow=c(1,1))



if (F) {
  plot_candles(d)
  lines(d$date_time, d$mean)
  abline(v=trades$date_time[trades$action == 'buy'], col='green3')
  abline(v=trades$date_time[trades$action == 'sell'], col='red3')
}









