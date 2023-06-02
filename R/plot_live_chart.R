plot_live_chart <- function(param, d=NULL) {

  if (is.null(d)) d <- compile_data(param, limit=600)

  d$candle_color <- ifelse(d$open < d$close, 'red3', 'darkgreen')

  tmp <- get_all_orders(symbol = param$symbol)
  tmp <- tmp[tmp$status %in% c('FILLED', 'PARTIALLY FILLED'),]
  tmp$price <- as.numeric(tmp$price)
  tmp$price[tmp$price == 0] <- NA
  tmp <- tmp[tmp$date_time >= min(d$date_time),]

  n <- pmin(param$n_quantile_rsi, nrow(d)) - 1

  threshold_buy_rsi <- quantile(d$rsi[(nrow(d)-n):nrow(d)], probs=param$quantile_buy_rsi, na.rm=TRUE)
  threshold_sell_rsi <- quantile(d$rsi[(nrow(d)-n):nrow(d)], probs=param$quantile_sell_rsi, na.rm=TRUE)

  threshold_overbought_rsi <- quantile(d$rsi[(nrow(d)-n):nrow(d)], probs=param$quantile_overbought_rsi, na.rm=TRUE)
  threshold_oversold_rsi <- quantile(d$rsi[(nrow(d)-n):nrow(d)], probs=param$quantile_oversold_rsi, na.rm=TRUE)


  if ("RStudioGD" %in% names(dev.list())) dev.off(dev.list()["RStudioGD"])


  g <- ggplot(d, aes(x=date_time)) +
    geom_vline(xintercept=as.POSIXct(tmp[tmp$side == 'BUY', 'date_time']), linetype="dotted", linewidth=0.4, color='green') +
    geom_vline(xintercept=as.POSIXct(tmp[tmp$side == 'SELL', 'date_time']), linetype="dotted", linewidth=0.4, color='red') +
    xlab('') +
    theme_minimal() +
    theme(legend.position='none',
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 1, r = 1, b = 1, l = 1))


  p <- suppressWarnings(cowplot::plot_grid(

    g +
      ggtitle(glue("{param$symbol} | {d$date_time[nrow(d)]} | Local: {Sys.time()}")) +
      geom_ribbon(aes(ymin=bb_lo, ymax=bb_hi), color='grey', linewidth=0.25, alpha=0.09) +
      geom_line(aes(y=bb_avg), color='grey50', linewidth=0.4, na.rm=TRUE) +
      geom_segment(aes(x=date_time, xend=date_time, y=open, yend=close, color=candle_color), linewidth=1) +
      geom_segment(aes(x=date_time, xend=date_time, y=low, yend=high, color=candle_color), linewidth=0.3) +
      geom_line(aes(y=ema_long), color='black', linewidth=0.6, na.rm=TRUE) +
      geom_line(aes(y=ema_mid), color='darkmagenta', linewidth=0.6, na.rm=TRUE) +
      geom_line(aes(y=ema_short), color='cyan3', linewidth=0.6, na.rm=TRUE) +
      geom_line(aes(y=supertrend_1), color='blue2', linewidth=0.4, na.rm=TRUE) +
      geom_line(aes(y=sar), color='orange3', linewidth=0.7, linetype='dashed', na.rm=TRUE) +
      geom_hline(yintercept=d$close[nrow(d)], linetype="dashed", linewidth=0.1) +
      geom_point(data=tmp[tmp$side == 'BUY',], aes(x=date_time, y=price), shape=24, size=2, color='green2') +
      geom_point(data=tmp[tmp$side == 'SELL',], aes(x=date_time, y=price), shape=25, size=2, color='red2') +
      scale_color_manual(values=c("red3", "darkgreen")) +
      ylab('Price') +
      theme(axis.text.x=element_blank(),
            plot.title=element_text(size=12))
    ,


    g +
      geom_bar(aes(x=date_time, y=macd_diff, fill=factor(macd_diff > 0)), stat='identity') +
      geom_line(aes(y=macd_signal), color='black', linewidth=0.75, na.rm=TRUE) +
      geom_line(aes(y=macd), color='purple2', linewidth=0.75, na.rm=TRUE) +
      geom_hline(yintercept=0, linewidth=0.25) +
      ylab('MACD') +
      scale_fill_manual(values=c("red2", "green3")) +
      theme(axis.text.x=element_blank())
    ,


    g +
      geom_line(aes(y=rsi), color='royalblue2', linewidth=0.5, na.rm=TRUE) +
      geom_line(aes(y=rsi_smooth), color='black', linewidth=0.6, na.rm=TRUE) +
      geom_hline(yintercept=50, linewidth=0.25, color='black') +
      geom_hline(yintercept=c(30,70), linewidth=0.25, color='grey50') +
      geom_hline(yintercept=threshold_buy_rsi, linetype="dashed", linewidth=0.25, color='green3') +
      geom_hline(yintercept=threshold_sell_rsi, linetype="dashed", linewidth=0.25, color='red2') +
      geom_hline(yintercept=threshold_overbought_rsi, linetype="dashed", linewidth=0.25, color='orange2') +
      geom_hline(yintercept=threshold_oversold_rsi, linetype="dashed", linewidth=0.25, color='dodgerblue') +
      #scale_y_continuous(breaks = c(30, 50, 70)) +
      ylab('RSI') +
      theme(axis.text.x=element_blank(),
            panel.grid.major = element_blank())
    ,

    g +
      geom_line(aes(y=adx), linewidth=0.75, na.rm=TRUE) +
      geom_line(aes(y=adx_pos), color='green4', linewidth=0.25, na.rm=TRUE) +
      geom_line(aes(y=adx_neg), color='red4', linewidth=0.25, na.rm=TRUE) +
      ylab('ADX') + theme(axis.text.x=element_blank())
    ,

    g +
      geom_line(aes(y=accum), color='darkolivegreen3', linewidth=0.4, na.rm=TRUE) +
      geom_line(aes(y=accum_smooth), color='darkgreen', linewidth=0.6, na.rm=TRUE) +
      geom_hline(yintercept=0, linewidth=0.25, color='black') +
      ylab('Acc/Dist') +
      scale_x_datetime(date_breaks = "60 min", date_labels = "%H:%M")
    ,

    ncol=1,
    align='v',
    rel_heights = c(3,1,1,1,1)

  ))

  print(p)


}
