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

  sleep_bt_runs = 3,
  sleep_bt_orders = 25,

  n_supertrend_1 = 23,    # Supertrend for BUY signals
  f_supertrend_1 = 0.96,

  n_supertrend_2 = 18,    # Supertrend for SELL signals
  f_supertrend_2 = 1.4,

  n_supertrend_3 = 5,    # Supertrend for SELL signals
  f_supertrend_3 = 1.88,

  n_supertrend_4 = 15,    # Supertrend for SELL signals
  f_supertrend_4 = 2.1,

  n_ema_short = 22,
  n_ema_long = 31,

  n_atr = 18,
  f_atr = 1.1, # factor to multiple ATR by when determining sell stop
  risk_ratio = 50,

  n_bband = 20,
  sd_bband = 2.9,
  n_above_bband_hi = 1,

  slope_threshold_buy_hold = -1.5,
  slope_threshold_sell_hold = 2.7,

  time_window = 5000,        # Window of time orders are good for on the server (milliseconds)
  wait_and_see = TRUE,      # wait until time step is a portion complete before acting
  wait_and_see_prop = 10/60,
  double_check = TRUE,       # when short buy/sell triggered, wait X seconds then double check the logic
  double_check_wait = 5     # in seconds

)



#-------------------------------------------------------------------------------
# Run latin-hypercube sampling
#-------------------------------------------------------------------------------

t_start <- proc.time()

n <- 1000 # number of LHS replicates
Y <- geneticLHS(n=n, k=17, pop=50, gen=10, pMut=0.25, verbose=T)

Y[,1] <- qunif(Y[,1], 5, 30)         # n_supertrend_1
Y[,2] <- qunif(Y[,2], 0.75, 1)     # f_supertrend_1

Y[,3] <- qunif(Y[,3], 5, 30)      # n_supertrend_2
Y[,4] <- qunif(Y[,4], 1, 1.25)     # f_supertrend_2

Y[,5] <- qunif(Y[,5], 5, 30)      # n_supertrend_3
Y[,6] <- qunif(Y[,6], 1.25, 1.5)     # f_supertrend_3

Y[,7] <- qunif(Y[,7], 5, 30)      # n_supertrend_4
Y[,8] <- qunif(Y[,8], 1.5, 1.75)     # f_supertrend_4

Y[,9] <- qunif(Y[,9], 5, 30)        # n_ema_short
Y[,10] <- qunif(Y[,10], 35, 90)     # n_ema_long

Y[,11] <- qunif(Y[,11], 10, 30)    # n_atr
Y[,12] <- qunif(Y[,12], 1, 2)      # f_atr

Y[,13] <- qunif(Y[,13], 15, 25)         # n_bband
Y[,14] <- qunif(Y[,14], 1.75, 2.5)     # sd_bband
Y[,15] <- qunif(Y[,15], 1.5, 4.5)       # n_above_bband_hi

Y[,16] <- qunif(Y[,16], -3, -1)     # slope_threshold_buy_hold
Y[,17] <- qunif(Y[,17], 1, 6)       # slope_threshold_sell_hold




map_lhs_to_param <- function(Y, param, i) {

  param$n_supertrend_1 <- Y[i,1]
  param$f_supertrend_1 <- Y[i,2]

  param$n_supertrend_2 <- Y[i,3]
  param$f_supertrend_2 <- Y[i,4]

  param$n_supertrend_3 <- Y[i,5]
  param$f_supertrend_3 <- Y[i,6]

  param$n_supertrend_4 <- Y[i,7]
  param$f_supertrend_4 <- Y[i,8]

  param$n_ema_short <- Y[i,9]
  param$n_ema_long <- Y[i,10]

  param$n_atr <- Y[i,11]
  param$f_atr <- Y[i,12]

  param$n_bband <- Y[i,13]
  param$sd_bband <- Y[i,14]
  param$n_above_bband_hi <- Y[i,15]

  param$slope_threshold_buy_hold <- Y[i,16]
  param$slope_threshold_sell_hold <- Y[i,17]

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
out <- out[order(out$score, decreasing=TRUE),]
#out <- out[order(out$percent_change, out$win_prob, decreasing=TRUE),]


out[1:10,]


#-------------------------------------------------------------------------------
# Set best parameters
#-------------------------------------------------------------------------------

param_best <- map_lhs_to_param(Y, param=param_default, i=out$i[1])

if (FALSE) {

  param_best <- map_lhs_to_param(Y, param=param_default, i=68) # Manual override

}


#-------------------------------------------------------------------------------
# Cache best fit parameters
#-------------------------------------------------------------------------------

saveRDS(out, file.path(getwd(), 'output', 'lhs_results.rds'))
saveRDS(param_best, file.path(getwd(), 'output', 'param_best.rds'))


#-------------------------------------------------------------------------------
# Optional: rerun previous results on recent data
#-------------------------------------------------------------------------------

if (FALSE) {

  out <- readRDS(file.path(getwd(), 'output', 'lhs_results.rds'))
  param_best <- readRDS(file.path(getwd(), 'output', 'param_best.rds'))


  # Re-run top X number of models on recent data
  n_best <- 10
  out_best <- data.frame()
  time_pin <- get_timestamp() # Pin the stop time so that all simulations have same data

  for (i in 1:n_best) {

    message(i)

    tmp_param <- as.list(out[i,colnames(out) %in% names(param_best)])
    tmp <- run_trade_algo_paper(param=tmp_param, time_stop=time_pin, verbose=FALSE)

    out_best <- rbind(
      out_best,
      data.frame(i = out$i[i],
                 n_trades = tmp$n_trades,
                 win_prob = tmp$win_prob,
                 percent_change = tmp$percent_change,
                 per_trade = round(tmp$percent_change/tmp$n_trades, 2),
                 actual = round(tmp$data$close[nrow(tmp$data)]/tmp$data$close[1] - 1,3)*100,
                 as.data.frame(tmp_param)
      )
    )

  }

  out_best <- out_best[order(out_best$percent_change, out_best$win_prob, decreasing=TRUE),]

  # Reset best parameters
  param_best <- as.list(out_best[1, colnames(out_best) %in% names(param_best)])

}



#-------------------------------------------------------------------------------
# Plot paper run
#-------------------------------------------------------------------------------

#best <- run_trade_algo(param=param_default, live=FALSE)
best <- run_trade_algo(param=param_best, live=FALSE)


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
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(param=param_best, live=TRUE)




#-------------------------------------------------------------------------------
# Use with caution: quick BUY and SELL orders
#-------------------------------------------------------------------------------

if (FALSE) {


  cancel_all_orders(param_best$symbol)


  qb() # Quick BUY


  qs() # Quick SELL


}

