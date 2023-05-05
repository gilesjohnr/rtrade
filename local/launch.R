#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

rm(list=ls())

pkg <- c("rtrade", "rbinanceus", "httr", "jsonlite", 'glue', 'lubridate', 'anytime', 'TTR', 'lhs')
invisible(lapply(pkg, library, character.only=TRUE))

if (FALSE) {

  set_credentials(key_api = '',
                  key_secret = '')

  check_credentials()

}

Sys.setenv(TZ='UTC')
data("handle")



#-------------------------------------------------------------------------------
# Define default parameters
#-------------------------------------------------------------------------------

param_default <- list(

  asset = 'BTC',
  symbol = 'BTCUSD',
  interval_short = '5m', # in minutes
  limit = 1000, # pmin(1000, 12*72),

  sleep_bt_runs = 10,
  sleep_bt_orders = 30,

  n_supertrend_1 = 20,    # Supertrend for BUY signals
  f_supertrend_1 = 1,
  n_supertrend_2 = 10,    # Supertrend for SELL signals
  f_supertrend_2 = 1.05,

  n_atr = 12,
  f_atr = 1.5, # factor to multiple ATR by when determining sell stop
  risk_ratio = 10,

  n_ema_short = 10, # Short-term Exponential Moving Average
  n_ema_long = 45, # Long-term Exponential Moving Average

  n_ema_buy_hold = 12,
  n_ema_sell_hold = 12,

  slope_threshold_buy_hold = -4, # below this, do not buy
  slope_threshold_sell_hold = 6, # above this, do not sell

  slope_threshold_bband_mode = 4,

  n_bbands_1 = 20,    # Number of time steps to use in Bollinger bands
  sd_bbands_1 = 1, # Number of standard deviations in Bollinger bands
  seq_bbands_1 = 1,

  n_bbands_2 = 20,    # Number of time steps to use in Bollinger bands
  sd_bbands_2 = 2, # Number of standard deviations in Bollinger bands
  seq_bbands_2= 3,

  time_window = 4000,        # Window of time orders are good for on the server (milliseconds)
  wait_and_see = TRUE,      # wait until time step is a portion complete before acting
  wait_and_see_prop = 0.5/5,
  double_check = TRUE,       # when short buy/sell triggered, wait X seconds then double check the logic
  double_check_wait = 10     # in seconds

)



#-------------------------------------------------------------------------------
# Run latin-hypercube sampling
#-------------------------------------------------------------------------------

t_start <- proc.time()

n <- 2000 # number of LHS replicates
Y <- geneticLHS(n=n, k=20, pop=50, gen=10, pMut=.25, verbose=T)

Y[,1] <- qunif(Y[,1], 5, 25)     # n_supertrend_1
Y[,2] <- qunif(Y[,2], 0.75, 3)   # f_supertrend_1
Y[,3] <- qunif(Y[,3], 5, 25)     # n_supertrend_2
Y[,4] <- qunif(Y[,4], 0.75, 3)   # f_supertrend_2

Y[,5] <- qunif(Y[,5], 6, 25)     # n_ema_short
Y[,6] <- qunif(Y[,6], 25, 75)    # n_ema_long

Y[,7] <- qunif(Y[,7], 6, 25)     # n_ema_buy_hold
Y[,8] <- qunif(Y[,8], 6, 25)     # n_ema_sell_hold

Y[,9] <- qunif(Y[,9], -20, -6)     # slope_threshold_buy_hold
Y[,10] <- qunif(Y[,10], 6, 20)       # slope_threshold_sell_hold

Y[,11] <- qunif(Y[,11], 10, 30)        # n_bbands_1
Y[,12] <- qunif(Y[,12], 0.5, 1.5)    # sd_bbands_1
Y[,13] <- qunif(Y[,13], 1, 2)        # seq_bbands_1

Y[,14] <- qunif(Y[,14], 10, 30)      # n_bbands_2
Y[,15] <- qunif(Y[,15], 1.75, 3)  # sd_bbands_2
Y[,16] <- qunif(Y[,16], 3, 5)        # seq_bbands_2

Y[,17] <- qunif(Y[,17], 6, 25)    # n_atr
Y[,18] <- qunif(Y[,18], 1, 3)     # f_atr
Y[,19] <- qunif(Y[,19], 1, 20)    # risk_ratio

Y[,20] <- qunif(Y[,20], 0.5, 6)    # slope_threshold_bband_mode


map_lhs_to_param <- function(Y, param, i) {

  param$n_supertrend_1 <- Y[i,1]
  param$f_supertrend_1 <- Y[i,2]
  param$n_supertrend_2 <- Y[i,3]
  param$f_supertrend_2 <- Y[i,4]

  param$n_ema_short <- Y[i,5]
  param$n_ema_long <- Y[i,6]

  param$n_ema_buy_hold <- Y[i,7]
  param$n_ema_sell_hold <- Y[i,8]

  param$slope_threshold_buy_hold <- Y[i,9]
  param$slope_threshold_sell_hold <- Y[i,10]

  param$n_bbands_1 <- Y[i,11]
  param$sd_bbands_1 <- Y[i,12]
  param$seq_bbands_1 <- Y[i,13]

  param$n_bbands_2 <- Y[i,14]
  param$sd_bbands_2 <- Y[i,15]
  param$seq_bbands_2 <- Y[i,16]

  param$n_atr <- Y[i,17]
  param$f_atr <- Y[i,18]
  param$risk_ratio <- Y[i,19]

  param$slope_threshold_bband_mode <- Y[i,20]

  return(param)

}


out <- data.frame()

for (i in 1:n) {

  message(i)
  tmp_param <- map_lhs_to_param(Y, param=param_default, i)

  tmp <- run_trade_algo(param=tmp_param, live=FALSE, verbose=FALSE)

  out <- rbind(out,
               data.frame(i = i,
                          n_trades = tmp$n_trades,
                          win_prob = tmp$win_prob,
                          percent_change = tmp$percent_change,
                          per_trade = round(tmp$percent_change/tmp$n_trades, 2),
                          actual = round(tmp$data$close[nrow(tmp$data)]/tmp$data$close[1] - 1,3)*100,
                          as.data.frame(tmp_param))
  )

}

t_stop <- proc.time() - t_start
message(paste('Runtime:', round(as.numeric(t_stop["elapsed"])/60, 2), 'minutes'))



#-------------------------------------------------------------------------------
# Clean up
#-------------------------------------------------------------------------------

out <- out[!is.nan(out$win_prob) & !is.na(out$win_prob),]
out$percent_change <- round(out$percent_change, 1)
out$per_trade <- round(out$per_trade, 1)
sel <- out$win_prob > min_win_prob
if (!any(sel)) stop('No runs with win probability above threshold')

out_best <- out[out$percent_change > out$actual,]
out_best <- out[out$win_prob > 0.5,]
out_best <- out_best[out_best$n_trades > 1,]
out_best <- out_best[rev(rank(order(out_best$percent_change, out_best$per_trade), ties.method='first')),]

ranks <- sapply(out_best[,c('percent_change', 'n_trades', 'per_trade', 'win_prob')], rank, ties.method='average')
out_best$score <- rowMeans(ranks)
out_best <- out_best[order(out_best$score, decreasing=TRUE),]
out_best[1:10,]



param_best <- map_lhs_to_param(Y, param=param_default, i=out_best$i[1])

if (FALSE) {

  param_best <- map_lhs_to_param(Y, param=param_default, i=9745) # Manual override

}

#best <- run_trade_algo(param=param_default, live=FALSE)
best <- run_trade_algo(param=param_best, live=FALSE)


#-------------------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------------------


d <- best$data
trades <- best$trades

msg <- glue("{best$n_trades} trades, {best$percent_change}% growth ({round(best$percent_change/best$n_trades, 2)}% per trade)
            Actual = {round( d$close[nrow(d)]/d$close[1] - 1,3)*100}%")

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(3,1), xpd=F)

plot(d$date_time, d$close, type='l', main=param_default$symbol)
lines(d$date_time, d$bb_avg, lwd=0.75, col='darkorange')
lines(d$date_time, d$bb_hi, lwd=0.5, col='goldenrod')
lines(d$date_time, d$bb_lo, lwd=0.5, col='goldenrod')
lines(d$date_time, d$supertrend_1, col='green3')
lines(d$date_time, d$supertrend_2, col='red3')
lines(d$date_time, d$ema_short, col='cyan4')
lines(d$date_time, d$ema_long, col='darkblue')

abline(v=trades$date_time[trades$action == 'buy'], col='green3')
abline(v=trades$date_time[trades$action == 'sell'], col='red3')

plot(trades$date_time, trades$total_value, pch=19, cex=1, xlim=range(d$date_time), main=msg)
lines(trades$date_time, trades$total_value)

if (best$n_trades > 1) {
  tmp <- trades[trades$action == 'sell',]
  hist(tmp$rate, breaks=30, main=glue("Median rate = {format(round(median(tmp$rate),5), scientific=F)}, Mean rate = {format(round(mean(tmp$rate),5), scientific=F)}
                                      Win probability = {round(sum(tmp$rate > 0)/nrow(tmp),2)}"))
  abline(v=0)
  abline(v=median(tmp$rate), lty=1, col='blue')
  abline(v=quantile(tmp$rate, probs=c(0.0275, 0.975)), lty=2, col='blue')
}

par(mfrow=c(1,1))




#-------------------------------------------------------------------------------
# Cache best fit parameters
#-------------------------------------------------------------------------------


saveRDS(param_best, file.path(getwd(), 'output', 'param_best.rds'))


if (FALSE) {

  param_best <- readRDS(file.path(getwd(), 'output', 'param_best.rds'))

}



#-------------------------------------------------------------------------------
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(param=param_best, live=TRUE)



