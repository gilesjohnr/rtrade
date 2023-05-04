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
  limit = pmin(1000, 12*48),

  sleep_bt_runs = 10,
  sleep_bt_orders = 30,

  n_supertrend_1 = 10,    # Supertrend for BUY signals
  f_supertrend_1 = 0.75,
  n_supertrend_2 = 10,    # Supertrend for SELL signals
  f_supertrend_2 = 0.5,

  n_atr = 12,
  f_atr = 1.5, # factor to multiple ATR by when determining sell stop
  risk_ratio = 20,

  n_ema_short = 10, # Short-term Exponential Moving Average
  n_ema_long = 45, # Long-term Exponential Moving Average

  slope_threshold_short_buy = -1, # below this, do not buy
  slope_threshold_short_sell = 2, # above this, do not sell
  slope_threshold_long_buy = -1, # below this, do not buy
  slope_threshold_long_sell = 2, # above this, do not sell
  slope_threshold_bband_mode = 0.1,

  n_bbands = 20,    # Number of time steps to use in Bollinger bands
  sd_bbands = 2, # Number of standard deviations in Bollinger bands

  time_window = 4000,        # Window of time orders are good for on the server (milliseconds)
  wait_and_see = TRUE,      # wait until time step is a portion complete before acting
  wait_and_see_prop = 0.5/5,
  double_check = TRUE,       # when short buy/sell triggered, wait X seconds then double check the logic
  double_check_wait = 10     # in seconds

)


n <- 1000 # number of LHS replicates
min_win_prob <- 0.5 # Minimum win probability

Y <- randomLHS(n, 16)

Y[,1] <- qunif(Y[,1], 5, 25)     # n_supertrend_1
Y[,2] <- qunif(Y[,2], 0.75, 3)   # f_supertrend_1
Y[,3] <- qunif(Y[,3], 5, 25)     # n_supertrend_2
Y[,4] <- qunif(Y[,4], 0.75, 3)   # f_supertrend_2

Y[,5] <- qunif(Y[,5], 6, 25)     # n_ema_short
Y[,6] <- qunif(Y[,6], 25, 75)    # n_ema_long

Y[,7] <- qunif(Y[,7], -20, -0.25)   # slope_threshold_short_buy
Y[,8] <- qunif(Y[,8], 0.25, 20)    # slope_threshold_short_sell
Y[,9] <- qunif(Y[,9], -20, 0)   # slope_threshold_long_buy
Y[,10] <- qunif(Y[,10], 0, 20)  # slope_threshold_long_sell

Y[,11] <- qunif(Y[,11], 10, 30)     # n_bbands
Y[,12] <- qunif(Y[,12], 0.5, 2.5)    # sd_bbands

Y[,13] <- qunif(Y[,13], 6, 25)    # n_atr
Y[,14] <- qunif(Y[,14], 1, 3)     # f_atr
Y[,15] <- qunif(Y[,15], 1, 20)    # risk_ratio

Y[,16] <- qunif(Y[,16], 0, 0.75)    # slope_threshold_bband_mode


map_lhs_to_param <- function(Y, param, i) {

  param$n_supertrend_1 <- Y[i,1]
  param$f_supertrend_1 <- Y[i,2]
  param$n_supertrend_2 <- Y[i,3]
  param$f_supertrend_2 <- Y[i,4]

  param$n_ema_short <- Y[i,5]
  param$n_ema_long <- Y[i,6]

  param$slope_threshold_short_buy <- Y[i,7]
  param$slope_threshold_short_sell <- Y[i,8]
  param$slope_threshold_long_buy <- Y[i,9]
  param$slope_threshold_long_sell <- Y[i,10]

  param$n_bbands <- Y[i,11]
  param$sd_bbands <- Y[i,12]

  param$n_atr <- Y[i,13]
  param$f_atr <- Y[i,14]
  param$risk_ratio <- Y[i,15]

  param$slope_threshold_bband_mode <- Y[i,16]

  return(param)

}



#-------------------------------------------------------------------------------
# Run latin-hypercube sampling
#-------------------------------------------------------------------------------

out <- data.frame()
t_start <- proc.time()

for (i in 1:n) {

  message(i)
  tmp_param <- map_lhs_to_param(Y, param=param_default, i)

  tmp <- run_trade_algo(param=tmp_param, live=FALSE, verbose=FALSE)

  out <- rbind(out,
               data.frame(i = i,
                          n_trades = tmp$n_trades,
                          win_prob = tmp$win_prob,
                          percent_change = tmp$percent_change)
  )

}

t_stop <- proc.time() - t_start
message(paste('Runtime:', round(as.numeric(t_stop["elapsed"])/60, 2), 'minutes'))



#-------------------------------------------------------------------------------
# Clean up
#-------------------------------------------------------------------------------

out <- out[!is.nan(out$win_prob) & !is.na(out$win_prob),]
out$percent_change <- round(out$percent_change, 1)
out$per_trade <- out$percent_change/out$n_trades
sel <- out$win_prob > min_win_prob
if (!any(sel)) stop('No runs with win probability above threshold')

out_best <- out[out$win_prob > min_win_prob,]
out_best <- out_best[out_best$n_trades > 1,]
out_best <- out_best[rev(rank(order(out_best$percent_change, out_best$win_prob), ties.method='first')),]
out_best[1:10,]



param_best <- map_lhs_to_param(Y, param=param_default, i=out_best$i[1])

if (FALSE) {

  param_best <- map_lhs_to_param(Y, param=param_default, i=186) # Manual override

}

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

path_cached_params <- file.path(getwd(), 'output', 'param_best.rds')
saveRDS(param_best, path_cached_params)


if (FALSE) {

  param_best <- readRDS(path_cached_params)

}



#-------------------------------------------------------------------------------
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(param=param_best, live=TRUE)



