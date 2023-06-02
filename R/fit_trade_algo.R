fit_trade_algo <- function(param) {


  #-------------------------------------------------------------------------------
  # Define training and testing time windows
  #-------------------------------------------------------------------------------

  # training time frame
  time_stop_train <- param$time_window_anchor_date
  time_start_train <- time_stop_train - (60*60*param$time_window_train)

  # testing time frame
  time_start_test <- time_stop_train
  time_stop_test <- time_start_test + (60*60*param$time_window_test)


  d <- tryCatch({

    compile_data(param=param,
                 time_stop=date_to_timestamp(time_stop_train),
                 limit=1000)

  }, error = function(e) {

    Sys.sleep(2)

    compile_data(param=param,
                 time_stop=date_to_timestamp(time_stop_train),
                 limit=1000)

  })

  if (!(time_start_train >= min(d$date_time))) stop ("time_start out of bounds")
  if (!((time_stop_train - 60) <= max(d$date_time))) stop ("time_stop out of bounds")

  d <- d[d$date_time >= time_start_train & d$date_time <= time_stop_train,]



  #-------------------------------------------------------------------------------
  # Run latin-hypercube sampling
  #-------------------------------------------------------------------------------

  t_start <- proc.time()

  set.seed(1234)
  Y <- geneticLHS(n=param$n_lhs_samp, k=19, pop=50, gen=10, pMut=0.25, verbose=TRUE)

  Y[,1] <- qunif(Y[,1], 6, 30)       # n_supertrend_1
  Y[,2] <- qunif(Y[,2], 1, 3)     # f_supertrend_1

  Y[,3] <- qunif(Y[,3], 0.01, 0.03)    # parabolic SAR accelerator
  Y[,4] <- qunif(Y[,4], 0.2, 1)         # parabolic SAR maximum accelerator

  Y[,5] <- qunif(Y[,5], 15, 25)         # n_atr
  Y[,6] <- qunif(Y[,6], 1.5, 2.5)          # f_atr

  Y[,7] <- qunif(Y[,7], 5, 30)         # n_rsi
  Y[,8] <- qunif(Y[,8], 60, 60*6)      # n_quantile_rsi

  Y[,9] <- qunif(Y[,9], 0.1, 0.45)      # quantile_buy_rsi
  Y[,10] <- qunif(Y[,10], 0.55, 0.9)    # quantile_sell_rsi

  Y[,11] <- qunif(Y[,11], 0.9, 0.99)      # quantile_overbought_rsi
  Y[,12] <- qunif(Y[,12], 0.01, 0.1)     # quantile_oversold_rsi

  Y[,13] <- qunif(Y[,13], 10, 45)     # n_ema_short
  Y[,14] <- qunif(Y[,14], -10, -0.5)    # slope_threshold_buy
  Y[,15] <- qunif(Y[,15], 0.5, 10)     # slope_threshold_sell

  Y[,16] <- qunif(Y[,16], 10, 30)     # n_keltner
  Y[,17] <- qunif(Y[,17], 1, 3)     # atr_keltner

  Y[,18] <- qunif(Y[,18], 10, 30)     # n_bbands
  Y[,19] <- qunif(Y[,19], 1, 3)     # sd_bbands

  #Y[,16] <- qunif(Y[,16], 11, 13)     # n_fast_macd = 12
  #Y[,17] <- qunif(Y[,17], 24, 28)     # n_slow_macd = 26
  #Y[,18] <- qunif(Y[,18], 8, 10)     # n_signal_macd = 9

  #Y[,19] <- qunif(Y[,19], 10, 15)     # n_adx


  map_lhs_to_param <- function(Y, param, i) {

    param$n_supertrend_1 <- round(Y[i,1], 0)
    param$f_supertrend_1 <- round(Y[i,2], 2)

    param$accel_sar <- round(Y[i,3], 3)
    param$max_accel_sar <- round(Y[i,4], 3)

    param$n_atr <- round(Y[i,5], 0)
    param$f_atr <- round(Y[i,6], 2)

    param$n_rsi <- round(Y[i,7], 0)
    param$n_quantile_rsi <- round(Y[i,8], 0)

    param$quantile_buy_rsi <- round(Y[i,9], 3)
    param$quantile_sell_rsi <- round(Y[i,10], 3)

    param$quantile_overbought_rsi <- round(Y[i,11], 3)
    param$quantile_oversold_rsi <- round(Y[i,12], 3)

    param$n_ema_short <- round(Y[i,13], 0)
    param$slope_threshold_buy <- round(Y[i,14], 2)
    param$slope_threshold_sell <- round(Y[i,15], 2)

    param$n_keltner <- round(Y[i,16], 0)
    param$atr_keltner <- round(Y[i,17], 2)

    param$n_bbands <- round(Y[i,18], 0)
    param$sd_bbands <- round(Y[i,19], 2)

    #param$n_fast_macd <- round(Y[i,16], 0)
    #param$n_slow_macd <- round(Y[i,17], 0)
    #param$n_signal_macd <- round(Y[i,18], 0)

    #param$n_adx <- round(Y[i,19], 0)

    return(param)

  }



  out <- data.frame()

  for (i in 1:param$n_lhs_samp) {

    if (i/param$n_lhs_samp == 0.25) message("25%")
    if (i/param$n_lhs_samp == 0.5) message("50%")
    if (i/param$n_lhs_samp == 0.75) message("75%")
    if (i/param$n_lhs_samp == 1) message("100%")

    tmp_param <- map_lhs_to_param(Y, param=param, i)

    tmp <- run_trade_algo_paper(param=tmp_param,
                                time_start = time_start_train,
                                time_stop = time_stop_train,
                                verbose=FALSE)

    out <- rbind(out,
                 data.frame(i = i,
                            n_trades = tmp$n_trades,
                            win_prob = tmp$win_prob,
                            percent_change = tmp$percent_change,
                            per_trade = round(tmp$percent_change/tmp$n_trades, 3),
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
  out$percent_change <- round(out$percent_change, 2)
  out$diff_baseline <- out$percent_change - out$actual
  out$tmp_n_trades <- -1*out$n_trades
  #out <- out[order(out$percent_change, out$win_prob, out$per_trade,  decreasing=TRUE),]

  out$rank <- frank(out[,c('percent_change', 'win_prob', 'per_trade', 'tmp_n_trades')], ties.method = "average", na.last=TRUE)
  out <- out[order(out$rank, decreasing=T),]
  #out <- out[order(out$win_prob, out$per_trade, out$percent_change,  decreasing=TRUE),]

  out[1:10,1:6]


  #-------------------------------------------------------------------------------
  # Set best parameters
  #-------------------------------------------------------------------------------

  param_best <- map_lhs_to_param(Y, param=param, i=out$i[1])

  if (FALSE) {

    param_best <- map_lhs_to_param(Y, param=param, i=121) # Manual override

  }





  #-------------------------------------------------------------------------------
  # Cache best fit parameters
  #-------------------------------------------------------------------------------

  param_best$run_date <- as.POSIXct(Sys.time())
  saveRDS(param_best, file.path(getwd(), 'output', 'param_best.rds'))
  saveRDS(out, file.path(getwd(), 'output', 'lhs_results.rds'))

  return(list(param_best=param_best, out=out))

}
