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

par <- list(

  asset = 'BTC',
  symbol = 'BTCUSD',
  interval_short = '5m', # in minutes
  limit = pmin(1000, 12*12),

  slope_threshold_buy = -1, # below this, do not buy
  slope_threshold_sell = 2, # above this, do not sell
  risk_ratio = 2, # Only on when in sideways trend (consolidating)

  sleep_bt_runs = 10,
  sleep_bt_orders = 30,

  n_supertrend_short = 10,
  f_supertrend_short_buy = 1,
  f_supertrend_short_sell = 1.1,
  n_atr = 10,
  f_atr = 1.05, # factor to multiple ATR by when determining sell stop
  n_ema_1 = 20, # For buy triggers
  n_ema_2 = 10, # For sell triggers

  manual_sell_triggers = FALSE,
  manual_sell_trigger_low = -0.0025,
  manual_sell_trigger_high = 0.005,

  time_window = 4000,
  wait_and_see = TRUE, # wait until time step is a portion complete before acting
  wait_and_see_prop = 1/5,
  double_check = TRUE

)

n <- 1000 # number of LHS replicates
min_win_prob <- 0.5 # Minimum win probability



Y <- randomLHS(n, 10)
Y[,1] <- qunif(Y[,1], -10, 0)  # slope_threshold_buy
Y[,2] <- qunif(Y[,2], 0, 10)   # slope_threshold_sell
Y[,3] <- qunif(Y[,3], 1, 20)   # risk_ratio
Y[,4] <- qunif(Y[,4], 6, 30)   # n_supertrend_short
Y[,5] <- qunif(Y[,5], 0.5, 3)   # f_supertrend_short_buy
Y[,6] <- qunif(Y[,6], 0.5, 3)   # f_supertrend_short_sell
Y[,7] <- qunif(Y[,7], 3, 24)   # n_atr
Y[,8] <- qunif(Y[,8], 1, 2)   # f_atr
Y[,9] <- qunif(Y[,9], 6, 24)   # n_ema_1
Y[,10] <- qunif(Y[,10], 6, 24)   # n_ema_2


map_lhs_to_par <- function(Y, par, i) {

  tmp_par <- par
  tmp_par$slope_threshold_buy <- Y[i,1]
  tmp_par$slope_threshold_sell <- Y[i,2]
  tmp_par$risk_ratio <- Y[i,3]
  tmp_par$n_supertrend_short <- Y[i,4]
  tmp_par$f_supertrend_short_buy <- Y[i,5]
  tmp_par$f_supertrend_short_sell <- Y[i,6]
  tmp_par$n_atr <- Y[i,7]
  tmp_par$f_atr <- Y[i,8]
  tmp_par$n_ema_1 <- Y[i,9]
  tmp_par$n_ema_2 <- Y[i,10]
  return(tmp_par)

}



#-------------------------------------------------------------------------------
# Run latin-hypercube sampling
#-------------------------------------------------------------------------------

out <- data.frame()
t_start <- proc.time()

for (i in 1:n) {

  message(i)
  tmp_par <- map_lhs_to_par(Y, par, i)

  tmp <- run_trade_algo(par=tmp_par, live=FALSE)

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
sel <- out$win_prob > min_win_prob
if (!any(sel)) stop('No runs with win probability above threshold')

out_best <- out[out$win_prob > min_win_prob,]
out_best <- out_best[out_best$n_trades > 1,]
out_best <- out_best[rev(rank(order(out_best$percent_change, out_best$win_prob), ties.method='first')),]
out_best[1:10,]



par_best <- map_lhs_to_par(Y, par, i=out_best$i[1])

if (FALSE) {

  par_best <- map_lhs_to_par(Y, par, i=4255) # Manual override

}

best <- run_trade_algo(par=par_best, live=FALSE)



#-------------------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------------------

msg <- glue("{best$n_trades} trades, {best$percent_change}% growth ({round(best$percent_change/best$n_trades, 2)}% per trade)
            Actual = {round( best$data$close[nrow(best$data)]/best$data$close[1] - 1,3)}")

d <- best$data
trades <- best$trades

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(3,1), xpd=F)

plot(d$date_time, d$supertrend_1, type='l', col='blue2', ylim=range(c(d$supertrend_1, d$mean), na.rm=T), main=par$symbol)
lines(d$date_time, d$close)
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

path_cached_params <- file.path(getwd(), 'output', 'par_best.rds')
saveRDS(par_best, path_cached_params)


if (FALSE) {

  par_best <- readRDS(path_cached_params)

}



#-------------------------------------------------------------------------------
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(par=par_best, live=TRUE)



