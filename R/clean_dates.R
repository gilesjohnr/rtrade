#' Produce cleaned date information
#' 
#' @param x Vector. Expects a date or date-time format
#' 

clean_dates <- function(x) {

  tmp <- as.POSIXct(x, tz=Sys.timezone())
  
  out <- data.frame(date_time = tmp,
                    date = as.Date(tmp),
                    year = year(tmp),
                    month = month(tmp),
                    day = day(tmp),
                    hour = hour(tmp),
                    minute = minute(tmp),
                    woy = week(tmp),
                    doy = yday(tmp),
                    dow = wday(tmp))
  
  return(out)
  
}
