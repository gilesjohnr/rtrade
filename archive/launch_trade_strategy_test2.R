
rm(list=ls())
pkg <- c("rbinanceus", "httr", "jsonlite", "RCurl", "magrittr", "kableExtra", "DT", "httr", 
         'glue', 'lubridate', 'anytime', 'arrow',
         'TTR', 'quantmod', 'candlesticks', 'lcyanalysis', 'smoothr')
invisible(lapply(pkg, library, character.only=TRUE))

path <- 'funcs'
for (i in list.files(path, pattern = "\\.[Rr]$")) source(file.path(path, i))

par <- list(
  
  symbol = 'BTCUSD',
  interval_short = '5m', # in minutes
  limit = pmin(20*24, 1000),
  prob_slope_threshold_buy = 0.5, # below this, do not buy
  prob_slope_threshold_sell = 0.6, # above this, do not sell
  risk_ratio = 2,
  manual_sell_triggers = F,
  manual_sell_trigger_low = -0.002,
  manual_sell_trigger_high = 0.004,
  
  n_ema = 20
  
)

#-------------------------------------------------------------------------
# Get current data
#-------------------------------------------------------------------------


# Get hourly (or other small-scale time interval)
d <- get_klines(symbol = par$symbol,
                interval = par$interval_short,
                limit = par$limit,
                verbose = FALSE)

d <- cbind(d, clean_dates(d$time_open))
d <- add_indicators(d)

atr <- as.data.frame(ATR(d[,c("high","low","close")], n=10))
d$atr <- atr$atr

st <- calc_supertrend(HLC=d[,c("high","low","close")], n=10, f=0.75)
d$supertrend_1 <- st$supertrend
d$supertrend_1_buy <- st$buy
d$supertrend_1_sell <- st$sell

st <- calc_supertrend(HLC=d[,c("high","low","close")], n=10, f=3)
d$supertrend_2 <- st$supertrend
d$supertrend_2_buy <- st$buy
d$supertrend_2_sell <- st$sell

d$EMA <- EMA(d[,"close"], n=par$n_ema) # Exponential Moving Average
k <- 1
for (i in (k+1):nrow(d)) d$EMA[i] <- mean(d$EMA[(i-k):i], na.rm=T)

#plot(d$EMA, type='l')
#lines(rollmean(d$EMA, k=5), col='red')

for (i in 2:nrow(d)) d$EMA_slope[i] <- d$EMA[i] - d$EMA[i-1]


if (par$manual_sell_triggers) {
  
  sell_trigger_low <- par$manual_sell_trigger_low
  sell_trigger_high <- par$manual_sell_trigger_high
  
} else {
  
  sel <- which(d$date_time >= max(d$date_time) - 60*60*1)
  sell_trigger_low <- -1*max(d$atr[sel]/d$mean[sel], na.rm=T)*1.01
  sell_trigger_high <- -1*sell_trigger_low * par$risk_ratio
  
}

slope_threshold_buy <- quantile(d$EMA_slope, na.rm=TRUE, probs=par$prob_slope_threshold_buy) # below this, do not buy
slope_threshold_sell <- quantile(d$EMA_slope, na.rm=TRUE, probs=par$prob_slope_threshold_sell) # above this, do not sell

slope_threshold_buy <- -1
slope_threshold_sell <- 1


#-------------------------------------------------------------------------
# Run trading strategy
#-------------------------------------------------------------------------

trades <- data.frame()
state <- NA

d <- d[!is.na(d$EMA_slope),]

for (i in 3:(nrow(d)-1)) {
  
  if (is.na(state) | state == 'sell') {
    
    # BUY IF short term signal from supertrends
    logic_buy <- d$supertrend_1_buy[i] == 1
    
    # ONLY BUY IF not in a longer term downward trend
    #if (!is.na(d$EMA_slope[i])) logic_buy <- logic_buy & d$EMA_slope[i] >= slope_threshold_buy
    
    #logic_buy <- FALSE
    # BUY IF longterm inflexion point
    #if (!is.na(d$EMA_slope[i-1])) {
    #  
    #  logic_buy_4 <- d$EMA_slope[i-1] < 0 & d$EMA_slope[i] > 0
    #  
    #  if (logic_buy_4) {
    #    logic_buy <- logic_buy_4
    #    message(":: Logic BUY 4 (long term inflexion point) ::")
    #  }
    #  
    #}
    
    # BUY IF downward trend ends and short term buy within 2 time steps
    #if (!is.na(d$EMA_slope[i-1])) {
    #  
    #  logic_buy_4 <- d$EMA_slope[i-1] < slope_threshold_buy & d$EMA_slope[i] >= slope_threshold_buy & any(d$supertrend_1_buy[(i-2):i] == 1)
    #  
    #  if (logic_buy_4) {
    #    logic_buy <- logic_buy_4
    #    message(":: Logic BUY 4 (downtrend ends and recent short buy) ::")
    #  }
    #  
    #}
    
    if (logic_buy) {
      
      trade_units <- 0.003
      
      if (nrow(trades) > 0) trade_units <- trades[nrow(trades), 'total_value']/d$mid[i]
      
      
      tmp <- data.frame(trade_id = paste(unlist(strsplit(as.character(d$date_time[i]), '[^0-9]')), collapse = ''),
                        date_time = d$date_time[i],
                        action = 'buy',
                        price = d$mid[i], # Or actual execution price from ticket
                        units = trade_units)
      
      tmp$total_value <- tmp$price*tmp$units
      trades <- rbind(trades, tmp)
      state <- 'buy' 
      
    }
    
  } else if (state == 'buy') { 
    
    tmp_buy_order <- trades[which.max(trades$date_time),]
    current_price <- d$close[i]
    
    # Basic trigger from supertrend
    if (d$supertrend_2_sell[i] < d$mid[i]) {
      logic_sell <- d$supertrend_2_sell[i] == 1 
    } else {
      logic_sell <- d$supertrend_1_sell[i] == 1
    }
    
    # Second sell trigger if price moves outside predetermined ranges (based on distribution of trade percent growth)
    logic_sell_2 <- (current_price / tmp_buy_order$price - 1) > sell_trigger_high | (current_price / tmp_buy_order$price - 1) < sell_trigger_low
    if (logic_sell_2) logic_sell <- logic_sell_2
    if (is.na(logic_sell)) logic_sell <- logic_sell_2
    
    
    # If there is a strong longer term upward trend, hold even when there are short term dips
    if (!is.na(d$EMA_slope[i])) {
      tmp <- d$EMA_slope[i] > slope_threshold_sell
      if (tmp) logic_sell <- !tmp
    }
    
    #### Third sell trigger if longer range (daily) supertrend detects am extended downward trend
    #if (!is.na(d$EMA_slope[i-1])) {
    #  logic_sell_3 <- d$EMA_slope[i] < slope_threshold_sell & d$EMA_slope[i-1] > slope_threshold_sell
    #  if (logic_sell_3) logic_sell <- logic_sell_3
    #} 
    
    ## BUY IF longterm inflexion point
    #if (!is.na(d$EMA_slope[i-1])) {
    #  
    #  logic_sell_4 <- d$EMA_slope[i-1] > 0 & d$EMA_slope[i] < 0
    #  
    #  if (logic_sell_4) {
    #    logic_sell <- logic_sell_4
    #    message(":: Logic SELL 4 (long term inflexion point) ::")
    #  }
    #  
    #}
    

    if (logic_sell) {
      
      tmp <- data.frame(trade_id = tmp_buy_order$trade_id,
                        date_time = d$date_time[i],
                        action = 'sell',
                        price = d$mid[i],
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
              {tot_prop_change}% growth ({round(tot_prop_change/tot_trades, 2)}% per trade)
            Actual = {round( d$close[nrow(d)]/d$close[1] - 1,3)}")

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(3,1))

plot(d$date_time, d$supertrend_1, type='l', col='blue2', ylim=range(c(d$supertrend_1, d$mean), na.rm=T), main=par$symbol)
lines(d$date_time, d$close)
abline(v=trades$date_time[trades$action == 'buy'], col='green3')
abline(v=trades$date_time[trades$action == 'sell'], col='red3')

plot(trades$date_time, trades$total_value, pch=19, cex=1, xlim=range(d$date_time), main=msg)
lines(trades$date_time, trades$total_value)

tmp <- trades[trades$action == 'sell',]
hist(tmp$rate, breaks=30, main=glue("Median rate = {format(round(median(tmp$rate),5), scientific=F)}
                                      Mean rate = {format(round(mean(tmp$rate),5), scientific=F)}
                                      Win probability = {round(sum(tmp$rate > 0)/nrow(tmp),2)}"))
abline(v=0)
abline(v=median(tmp$rate), lty=1, col='blue')
abline(v=quantile(tmp$rate, probs=c(0.0275, 0.975)), lty=2, col='blue')

par(mfrow=c(1,1))



if (T) {
  
  plot_candles(d)
  lines(d$date_time, d$EMA, lwd=2)
  lines(d$date_time, d$supertrend_1, col='purple')
  lines(d$date_time, d$supertrend_2, col='purple3')
  abline(v=trades$date_time[trades$action == 'buy'], col='green3')
  abline(v=trades$date_time[trades$action == 'sell'], col='red3')
  
}











