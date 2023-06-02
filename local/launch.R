#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

rm(list=ls())

invisible(
  lapply(
    c("rtrade", "rbinanceus", "httr", "jsonlite", 'glue', 'lubridate', 'anytime',
      'TTR', 'lhs', 'zoo', 'ggplot2', 'gridExtra', 'cowplot', 'RColorBrewer',
      'data.table', 'scales'),
    library,
    character.only=TRUE
  )
)

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

  hold = FALSE, # When true, all BUY and SELL triggers are paused and engine is in WATCH mode

  asset = 'BTC',
  symbol = 'BTCUSD',
  interval_short = '1m', # in minutes
  limit = pmin(1000, 60*16),

  #time_window_anchor_date = as.POSIXct("2023-05-31 21:00:00"),
  #time_window_anchor_date = as.POSIXct(Sys.time()) - 60*60*3,
  time_window_anchor_date = as.POSIXct(Sys.time()),
  time_window_train = 12, # in hours
  time_window_test = 4,

  n_lhs_samp = 200,  # Number of Latin Hyper Cube samples to run

  sleep_bt_runs = 5,
  sleep_bt_orders = 30,

  n_supertrend_1 = 20,    # Supertrend for BUY signals
  f_supertrend_1 = 1.25,

  accel_sar = 0.02,       # Parabolic SAR
  max_accel_sar = 0.2,

  n_ema_short = 20, # Short-term Exponential Moving Average
  n_ema_mid = 75,
  n_ema_long = 200, # Long-term Exponential Moving Average

  slope_threshold_buy = -1.5, # below this, do not buy
  slope_threshold_sell = 6, # above this, do not sell

  n_atr = 12,
  f_atr = 1.5, # factor to multiple ATR by when determining sell stop
  risk_ratio = 100,

  n_rsi = 14,
  n_quantile_rsi = 14,

  quantile_buy_rsi = 0.3, # must be below this threshold for buy signal
  quantile_sell_rsi = 0.7, # must be above this threshold for sell signal

  quantile_overbought_rsi = 0.9,
  quantile_oversold_rsi = 0.1,

  n_fast_macd = 12,
  n_slow_macd = 26,
  n_signal_macd = 9,

  n_adx = 15,

  n_bbands = 20,
  sd_bbands = 2,

  n_keltner = 20,
  atr_keltner = 2,

  time_window = 5000,        # Window of time orders are good for on the server (milliseconds)
  wait_and_see = FALSE,      # wait until time step is a portion complete before acting
  wait_and_see_prop = 5/60,
  double_check = FALSE,       # when short buy/sell triggered, wait X seconds then double check the logic
  double_check_wait = 10     # in seconds

)



#-------------------------------------------------------------------------------
# Define training and testing time windows
#-------------------------------------------------------------------------------

# training time frame
time_stop_train <- param_default$time_window_anchor_date
time_start_train <- time_stop_train - (60*60*param_default$time_window_train)

# testing time frame
time_start_test <- time_stop_train
time_stop_test <- time_start_test + (60*60*param_default$time_window_test)



#-------------------------------------------------------------------------------
# Fit trade algo paramters
#-------------------------------------------------------------------------------

tmp <- fit_trade_algo(param=param_default)

out <- tmp[[2]]
param_best <- tmp[[1]]
rm(tmp)



#-------------------------------------------------------------------------------
# Load best fit parameters
#-------------------------------------------------------------------------------

out <- readRDS(file.path(getwd(), 'output', 'lhs_results.rds'))
param_best <- readRDS(file.path(getwd(), 'output', 'param_best.rds'))



#-------------------------------------------------------------------------------
# Plot paper run
#-------------------------------------------------------------------------------

if (F) {

  best_train <- run_trade_algo_paper(param=param_default,
                                     time_start=time_start_train,
                                     time_stop=time_stop_train)

}


best_train <- run_trade_algo_paper(param=param_best,
                                   time_start=time_start_train,
                                   time_stop=time_stop_train)

best_test <- run_trade_algo_paper(param=param_best,
                                  time_start=time_start_test,
                                  #time_stop=time_stop_test,
                                  time_stop=as.POSIXct(Sys.time()) - 60,
                                  last_trade = best_train$trades[nrow(best_train$trades), ])


trade_plots <- "Train"
trade_plots <- c("Train", "Test")

par(mfrow=c(3,2), oma=c(0,0,0,0), xpd=F)
layout(mat=matrix(1:6, ncol=2, byrow = F),
       heights = as.integer(c(2,1,1)),
       widths = as.integer(c(3, 2)))

for (i in trade_plots) {

  if (i == "Train") best <- best_train
  if (i == "Test") best <- best_test
  if (length(trade_plots) == 1) best_test <- best_train

  d <- best$data
  trades <- best$trades


  par(mar=c(2,2,1,1))

  d$green <- d$open < d$close
  sel <- as.logical(d$green)

  plot(d$date_time, d$mid, type='l', xlab='', ylab='Price', lwd=0.5,
       ylim=range(rbind(best_train$data[,c('high', 'low')], best_test$data[,c('high', 'low')]), na.rm=T),
       main=i)

  segments(x0=d$date_time[sel], x1=d$date_time[sel],
           y0=d$low[sel], y1=d$high[sel],
           col='darkgreen')

  segments(x0=d$date_time[sel], x1=d$date_time[sel],
           y0=d$open[sel], y1=d$close[sel],
           col='darkgreen', lwd=2.5)

  segments(x0=d$date_time[!sel], x1=d$date_time[!sel],
           y0=d$low[!sel], y1=d$high[!sel],
           col='red3')

  segments(x0=d$date_time[!sel], x1=d$date_time[!sel],
           y0=d$open[!sel], y1=d$close[!sel],
           col='red3', lwd=2.5)

  lines(d$date_time, d$supertrend_1, col='blue')
  lines(d$date_time, d$sar, col='orange3', lty=2)

  lines(d$date_time, d$ema_long)
  lines(d$date_time, d$ema_mid, col='purple')
  lines(d$date_time, d$ema_short, col='cyan3')

  sel <- trades$action == 'buy'
  abline(v=trades$date_time[sel], col='green2', lwd=0.75, lty=3)
  points(trades$date_time[sel], trades$price[sel], col='green2', pch=24, cex=1.25)

  sel <- trades$action == 'sell'
  abline(v=trades$date_time[sel], col='red', lwd=0.75, lty=3)
  points(trades$date_time[sel], trades$price[sel], col='red', pch=25, cex=1.25)



  par(mar=c(2,2,4,1))

  msg <- glue("{best$percent_change}% growth in {best$n_trades} trade(s) ({round(best$percent_change/best$n_trades, 2)}% per trade)
            Actual = {round( d$close[nrow(d)]/d$close[1] - 1,3)*100}%")

  plot(trades$date_time, trades$total_value, pch=19, cex=1, main=msg,
       xlim=range(d$date_time),
       ylim=range(c(best_train$trades$total_value, best_test$trades$total_value), na.rm=T))

  lines(trades$date_time, trades$total_value)


  tmp <- trades[trades$action == 'sell',]

  if (nrow(tmp) < 2) {

    plot(NA, NA, xlim=c(0,1), ylim=c(0,1), ylab="", xlab="", xaxt='n', yaxt='n')

  } else {

    hist(tmp$rate, breaks=30, main=glue("Mean rate = {format(round(mean(tmp$rate, na.rm=T),5), scientific=F)}
                                      Win probability = {round(sum(tmp$rate > 0, na.rm=T)/nrow(tmp),2)}"))

    abline(v=0)
    abline(v=median(tmp$rate), lty=1, col='blue')
    abline(v=quantile(tmp$rate, probs=c(0.0275, 0.975), na.rm=T), lty=2, col='blue')

  }



}

par(mfrow=c(1,1))





#-------------------------------------------------------------------------------
# Run calibrated trading algorithm LIVE
#-------------------------------------------------------------------------------

run_trade_algo(param=param_best, live=TRUE)



#-----------------------------------------------------------------------------
# Use with caution: quick BUY and SELL orders
#-------------------------------------------------------------------------------

if (FALSE) {


  cancel_all_orders(param_best$symbol)


  qb() # Quick BUY


  qs() # Quick SELL


  get_account_balance()

}

