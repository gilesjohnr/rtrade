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
  interval_short = '1m', # in minutes
  limit = pmin(1000, 60*(24+3)),

  sleep_bt_runs = 2,
  sleep_bt_orders = 30,

  n_supertrend_1 = 20,    # Supertrend for BUY signals
  f_supertrend_1 = 1,
  n_supertrend_2 = 10,    # Supertrend for SELL signals
  f_supertrend_2 = 1.05,

  n_ema_buy_hold = 16,
  n_ema_sell_hold = 18,

  slope_threshold_buy_hold = 17, # below this, do not buy
  slope_threshold_sell_hold = 16, # above this, do not sell

  n_atr = 12,
  f_atr = 1.5, # factor to multiple ATR by when determining sell stop
  risk_ratio = 50,

  n_bband = 20,
  sd_bband = 2,
  slope_threshold_bband = 1.5,

  time_window = 5000,        # Window of time orders are good for on the server (milliseconds)
  wait_and_see = TRUE,      # wait until time step is a portion complete before acting
  wait_and_see_prop = 5/60,
  double_check = TRUE,       # when short buy/sell triggered, wait X seconds then double check the logic
  double_check_wait = 5     # in seconds

)



#-------------------------------------------------------------------------------
# Run latin-hypercube sampling
#-------------------------------------------------------------------------------

t_start <- proc.time()

n <- 5000 # number of LHS replicates
Y <- geneticLHS(n=n, k=13, pop=50, gen=10, pMut=0.25, verbose=T)

Y[,1] <- qunif(Y[,1], 5, 30)      # n_supertrend_1
Y[,2] <- qunif(Y[,2], 1, 3)   # f_supertrend_1
Y[,3] <- qunif(Y[,3], 5, 30)      # n_supertrend_2
Y[,4] <- qunif(Y[,4], 1, 3)   # f_supertrend_2

Y[,5] <- qunif(Y[,5], 6, 30)     # n_ema_buy_hold
Y[,6] <- qunif(Y[,6], 6, 30)     # n_ema_sell_hold

Y[,7] <- qunif(Y[,7], -15, -1)   # slope_threshold_buy_hold
Y[,8] <- qunif(Y[,8], 1, 15)    # slope_threshold_sell_hold

Y[,9] <- qunif(Y[,9], 6, 30)    # n_atr
Y[,10] <- qunif(Y[,10], 1, 2)   # f_atr

Y[,11] <- qunif(Y[,11], 15, 25)       # n_bband
Y[,12] <- qunif(Y[,12], 1.75, 2.25)   # sd_bband
Y[,13] <- qunif(Y[,13], 0.5, 2.5)     # slope_threshold_bband




map_lhs_to_param <- function(Y, param, i) {

  param$n_supertrend_1 <- Y[i,1]
  param$f_supertrend_1 <- Y[i,2]
  param$n_supertrend_2 <- Y[i,3]
  param$f_supertrend_2 <- Y[i,4]

  param$n_ema_buy_hold <- Y[i,5]
  param$n_ema_sell_hold <- Y[i,6]

  param$slope_threshold_buy_hold <- Y[i,7]
  param$slope_threshold_sell_hold <- Y[i,8]

  param$n_atr <- Y[i,9]
  param$f_atr <- Y[i,10]

  param$n_bband <- Y[i,11]
  param$sd_bband <- Y[i,12]
  param$slope_threshold_bband <- Y[i,13]

  return(param)

}


out <- data.frame()
time_pin <- get_timestamp() # Pin the stop time so that all simulations have same data

for (i in 1:n) {

  message(i)

  tmp_param <- map_lhs_to_param(Y, param=param_default, i)
  tmp <- run_trade_algo_paper(param=tmp_param, time_stop=time_pin, verbose=FALSE)

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
out$times_baseline <- out$percent_change/out$actual

out <- out[rev(rank(order(out$percent_change, out$per_trade), ties.method='first')),]
ranks <- sapply(out[,c('percent_change', 'per_trade', 'win_prob')], rank, ties.method='average')
out$score <- rowMeans(ranks)
#out <- out[order(out$score, decreasing=TRUE),]
out <- out[order(out$percent_change, out$win_prob, decreasing=TRUE),]


out[1:10,]


param_best <- map_lhs_to_param(Y, param=param_default, i=out$i[1])

if (FALSE) {

  param_best <- map_lhs_to_param(Y, param=param_default, i=1589) # Manual override

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

plot(d$date_time, d$close, type='l', main=best$param$symbol)

lines(d$date_time, d$supertrend_1, col='green3')
lines(d$date_time, d$supertrend_2, col='red3')

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

saveRDS(out, file.path(getwd(), 'output', 'lhs_results.rds'))
saveRDS(param_best, file.path(getwd(), 'output', 'param_best.rds'))


if (FALSE) {

  out <- readRDS(file.path(getwd(), 'output', 'lhs_results.rds'))
  param_best <- readRDS(file.path(getwd(), 'output', 'param_best.rds'))

}



#-------------------------------------------------------------------------------
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(param=param_best, live=TRUE)




#-------------------------------------------------------------------------------
# Use with caution: quick BUY and SELL orders
#-------------------------------------------------------------------------------

if (FALSE) {


  qb() # Quick BUY


  qs() # Quick SELL


}

