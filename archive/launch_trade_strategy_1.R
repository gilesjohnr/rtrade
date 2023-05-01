if (FALSE) {
  
  pkg <- c("Nancy", "httr", "jsonlite", "RCurl", "magrittr", "kableExtra", "DT", "httr", 
           'glue', 'lubridate', 'anytime', 'arrow',
           'TTR', 'quantmod', 'candlesticks', 'lcyanalysis')
  invisible(lapply(pkg, library, character.only=TRUE))
  
  path <- 'funcs'
  for (i in list.files(path, pattern = "\\.[Rr]$")) source(file.path(path, i))
  
  par <- list(
    
    instrument = 'BTCUSD',
    
    slope_treshold <- -0.000001
    
  )
  
  
  
  
}



d_day <- get_oanda_data(par, 
                        instrument=instrument, 
                        granularity='D',
                        date_start = Sys.Date()-1 - 200,
                        date_stop = Sys.Date()-1)
head(d_day)

d_day <- add_indicators(d_day)

st <- calc_supertrend(HLC=d_day[,c("high","low","close")], n=20, f=1.5)
d_day$supertrend_day <- st$supertrend
d_day$supertrend_buy_day <- st$buy
d_day$supertrend_sell_day <- st$sell


d <- get_oanda_data(par, 
                    instrument=instrument, 
                    granularity='H1',
                    date_start = Sys.Date()-1 - 100,
                    date_stop = Sys.Date()-1)
head(d)

d <- add_indicators(d)

st <- calc_supertrend(HLC=d[,c("high","low","close")], n=24*5, f=0.5)
d$supertrend <- st$supertrend
d$supertrend_buy <- st$buy
d$supertrend_sell <- st$sell

d <- merge(d, d_day[,c('date_time', 'supertrend_day')], by='date_time', all.x=T)
d$supertrend_day <- na.approx(d$supertrend_day, na.rm = FALSE)
d$supertrend_day_delta <- NA
for (i in 2:nrow(d)) d$supertrend_day_delta[i] <- d$supertrend_day[i] - d$supertrend_day[i-1]



if (F) {
  plot(d$date_time, d$supertrend_day, type='l', col='green3')
  lines(d$date_time, d$high)
}

# Create list to hold orders made
trades <- data.frame()
state <- NA

get_trade_units <- function(par, instrument) {
  
  bal <- as.numeric(get_account_info(par)$account$balance)
  floor(bal/get_current_price(par, instrument)$ask)
  
}
get_trade_units <- function(par, instrument) 100


for (i in 1:nrow(d)) {
  
  if (is.na(state) | state == 'sell') {
    
    logic_buy <- d$supertrend_buy[i] == 1
    if (!is.na(d$supertrend_day_delta[i])) logic_buy <- logic_buy & d$supertrend_day_delta[i] >= slope_treshold
    #if (!is.na(d$high[i]) & !is.na(d$supertrend_day[i])) logic_buy <- logic_buy & d$high[i] >= d$supertrend_day[i]
    #if (i > 6 & !is.na(d$rsi[i])) logic_buy <- logic_buy & d$rsi[i] > d$rsi[i-1]
    #if (!is.na(d$adx[i]) & !is.na(d$adx_neg[i])) logic_buy <- logic_buy & d$adx[i] > 10
    #if (!is.na(d$sma_200[i]) & !is.na(d$sma_50[i])) logic_buy <- d$supertrend_buy[i] == 1 & d$sma_200[i] > d$sma_50[i]
    #if (!is.na(d$macd[i]) & !is.na(d$macd_signal[i])) logic_buy <- logic_buy & d$macd_signal[i] > d$macd[i]
    
    if (logic_buy) {
      
      trade_units <- get_trade_units(par, instrument)
      if (nrow(trades) > 0) trade_units <- trades[nrow(trades), 'total_value']/(d$mean[i] + spread)
      #trade_limit_price <- get_current_price(par, instrument)$ask
      
      # Submit buy order
      
      # Confirm buy order
      
      tmp <- data.frame(trade_id = paste(unlist(strsplit(as.character(d$date_time[i]), '[^0-9]')), collapse = ''),
                        date_time = d$date_time[i],
                        action = 'buy',
                        price = d$mean[i] + spread, # Or actual execution price from ticket
                        units = trade_units)
      
      tmp$total_value <- tmp$price*tmp$units
      trades <- rbind(trades, tmp)
      state <- 'buy' 
      
    }
    
  } else if (state == 'buy') { 
    
    tmp_buy_order <- trades[which.max(trades$date_time),]
    current_price <- d$mean[i]
    
    # Basic trigger from supertrend
    logic_sell <- d$supertrend_sell[i] == 1
    
    # Second sell trigger if price moves outside predetermined ranges (based on distribution of trade percent growth)
    logic_sell_2 <- (current_price / tmp_buy_order$price - 1) > 0.015 | (current_price / tmp_buy_order$price - 1) < -0.0015
    if (logic_sell_2) logic_sell <- logic_sell_2
    if (is.na(logic_sell)) logic_sell <- logic_sell_2
    
    
    # Third sell trigger if longer range (daily) supertrend detects am extended downward trend
    if (!is.na(d$supertrend_day_delta[i])) {
      logic_sell_3 <- d$supertrend_day_delta[i] < slope_treshold
      if (logic_sell_3) logic_sell <- logic_sell_3
    } 
    
    
    if (logic_sell) {
      
      # Submit sell order
      
      # Confirm sell order
      
      tmp <- data.frame(trade_id = tmp_buy_order$trade_id,
                        date_time = d$date_time[i],
                        action = 'sell',
                        price = d$mean[i],
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


tot_days <- max(as.Date(trades$date_time)) - min(as.Date(trades$date_time))
tot_trades <- sum(trades$action == 'sell')
tot_prop_change <- round((trades$total_value[nrow(trades)] / trades$total_value[1] - 1)*100, 2)

msg <- glue("{tot_trades} trades in {tot_days} days ({round(tot_trades/as.numeric(tot_days),2)} per day)
              {tot_prop_change}% growth ({round(tot_prop_change/tot_trades, 2)}% per trade)")

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(3,1))

plot(d$date_time, d$supertrend_day, type='l', col='blue2', ylim=range(c(d$supertrend_day, d$mean), na.rm=T), main=instrument)
lines(d$date_time, d$high)
abline(v=trades$date_time[trades$action == 'buy'], col='green3')
abline(v=trades$date_time[trades$action == 'sell'], col='red3')

plot(trades$date_time, trades$total_value, pch=19, cex=1, xlim=range(d$date_time), main=msg)
lines(trades$date_time, trades$total_value)

tmp <- trades[trades$action == 'sell',]
hist(tmp$rate, breaks=30, main=glue("Mean rate = {format(round(mean(tmp$rate),5), scientific=F)}
                                    Mean pips = {round(mean(tmp$pips),2)}"))
abline(v=0)
abline(v=mean(tmp$rate), lty=1, col='blue')
abline(v=quantile(tmp$rate, probs=c(0.0275, 0.975)), lty=2, col='blue')

par(mfrow=c(1,1))



if (F) {
  plot_candles(d)
  lines(d$date_time, d$mean)
  abline(v=trades$date_time[trades$action == 'buy'], col='green3')
  abline(v=trades$date_time[trades$action == 'sell'], col='red3')
}



#}