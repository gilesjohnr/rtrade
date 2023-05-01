#' Plot candelsticks
#' 
#' @param d Dataframe with high, low, open, close
#' 

plot_candles <- function(d, main=NULL) {
  
  d$green <- d$open < d$close
  sel <- as.logical(d$green)
  
  plot(d$date_time, d$mid, type='l', xlab='', ylab='Price', lwd=0.5,
       ylim=range(c(d$low, d$high), na.rm=TRUE),
       main=main)
  
  segments(x0=d$date_time[sel], 
           x1=d$date_time[sel],
           y0=d$low[sel], 
           y1=d$high[sel],
           col='blue3')
  
  segments(x0=d$date_time[sel], 
           x1=d$date_time[sel],
           y0=d$open[sel], 
           y1=d$close[sel],
           col='blue3',
           lwd=2.5)
  
  segments(x0=d$date_time[!sel], 
           x1=d$date_time[!sel],
           y0=d$low[!sel], 
           y1=d$high[!sel],
           col='red3')
  
  segments(x0=d$date_time[!sel], 
           x1=d$date_time[!sel],
           y0=d$open[!sel], 
           y1=d$close[!sel],
           col='red3',
           lwd=2.5)
  
}
