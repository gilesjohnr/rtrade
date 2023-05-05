tmp <- get_instruments()
tmp <- sort(tmp$instruments$name)
saveRDS(as.vector(tmp), file = glue("{getwd()}/data/instrument_names.rds"))

################################################################################
# Build historic data

instruments <- c('EUR_USD', 'AUD_USD', 'GBP_USD')

all_dates <- seq(as.Date("2010-01-01"), as.Date("2023-03-23"), 1)

for (i in instruments) {

  i
  tmp <- get_oanda_data(param,
                        instrument=i,
                        granularity='M',
                        date_start=all_dates[1],
                        date_stop=all_dates[length(all_dates)])
  head(tmp, n=3)

  arrow::write_paramquet(tmp, sink=file.path(getwd(), glue('data/historical_data_{i}_month.paramquet')))


}






################################################################################


# Get data from Oanda server
d <- get_oanda_data(param,
                    instrument='EUR_USD',
                    granularity='H1',
                    date_start='2020-08-01',
                    date_stop='2021-02-01')
head(d)


d$sma <- SMA(d[,"close"], n=30) # Simple Moving Average
d$ema <- EMA(d[,"close"], n=8) # Exponential Moving Average

# MACD Oscillator
macd <- as.data.frame(MACD(d[,"close"], 12, 36, 9, maType="EMA"))
d$macd <- macd$macd
d$macd_signal <- macd$signal

# Relative Strength Index (RSI)
d$rsi <- RSI(d[,"close"], n=14)

# Average true range
atr <- as.data.frame(ATR(d[,c("high","low","close")], n=14))
d$atr <- atr$atr

# Average Directional movement index
adx <- as.data.frame(ADX(d[,c("high","low","close")], n=14))
d$adx <- adx$ADX
d$adx_pos <- adx$DIp
d$adx_neg <- adx$DIn

# Bollinger bands
bb <- as.data.frame(BBands(HLC=d[,c("high","low","close")], n=20, sd=1.96))
#bb <- as.data.frame(BBands(HLC=d[,"close"], n=20, sd=2))
d$bb_avg <- bb$mavg
d$bb_hi <- bb$up
d$bb_lo <- bb$dn

plot_candles(d)
lines(d$date_time, d$mean)

lines(d$date_time, d$bb_avg, type='l')
lines(d$date_time, d$bb_hi, type='l')
lines(d$date_time, d$bb_lo, type='l')

plot(d$date_time, d$atr, type='l')

plot(d$date_time, d$adx, type='l', lwd=1.5,
     ylim=range(c(d$adx, d$adx_pos, d$adx_neg), na.rm=T))
lines(d$date_time, d$adx_pos, type='l', col='green3')
lines(d$date_time, d$adx_neg, type='l', col='red3')

plot(d$date_time, d$sma, type='l', ylim=range(c(d$sma, d$ema), na.rm=T))
lines(d$date_time, d$ema, type='l', col='purple2')

steps <- 90
sel <- (nrow(d)-steps):nrow(d)
plot(d$date_time[sel], d$macd[sel], type='l')
lines(d$date_time[sel], d$macd_signal[sel], col='orange2', type='l')



st <- calc_supertrend(HLC=d[,c("high","low","close")], n=12, f=0.25)

d$supertrend <- st$supertrend
d$buy <- st$buy
d$sell <- st$sell

plot_candles(d)
lines(d$date_time, st$supertrend)
abline(v=d$date_time[which(d$buy==1)], col='green3')
abline(v=d$date_time[which(d$sell==1)], col='red3')

sum(d$buy)
sum(d$sell)


tmp <- d[d$buy == 1 | d$sell == 1,]
test <- data.frame()

for (i in 1:(nrow(tmp)-1)) {

  if (tmp$buy[i] == 1) {

    test <- rbind(test,
                  data.frame(date_time=tmp$date_time[i],
                             trade=row.names(tmp[i,]),
                             price_buy = tmp$mean[i],
                             price_sell = tmp$mean[i+1],
                             rate = (tmp$mean[i+1]/tmp$mean[i])-1,
                             pips = (tmp$mean[i+1] - tmp$mean[i]) / 0.0001))

  }

}

param(mfrow=c(1,2))
hist(test$rate, breaks=30, main=round(mean(test$rate),5))
abline(v=0)
abline(v=mean(test$rate), lty=2, col='blue')

hist(test$pips, breaks=30, main=round(mean(test$pips), 2))
abline(v=0)
abline(v=mean(test$pips), lty=2, col='purple')
param(mfrow=c(1,1))

spread <- 0.0001 * 3
principal <- 5000
r <- mean(test$rate)-spread
per_day <- sum(d$buy)/184

tot <- principal*(1+r)^(per_day*365)
format(tot, big.mark=',')





############

r <- 0.01
spread <- 0.0001 * 0.2

tab <- data.frame()

for (i in 1:nrow(d)) {

  if (nrow(tab) == 0 & d$buy[i] == 1 & d$rsi[i] < 80 & d$adx_neg[i] < 20 & d$volume[i] > 1000) {

    tab <- rbind(tab, d[i,])

  } else if (nrow(tab) > 0) {

    if (tab$sell[nrow(tab)] == 1 & d$buy[i] == 1 & d$rsi[i] < 80 & d$adx_neg[i] < 20 & d$volume[i] > 1000) tab <- rbind(tab, d[i,])

    if (tab$buy[nrow(tab)] == 1 & d$mean[i] >= tab$mean[nrow(tab)]*(1+r)) {

      tmp <- d[i,]
      tmp$sell <- 1
      tab <- rbind(tab, tmp)

    } else if (tab$buy[nrow(tab)] == 1 & d$sell[i] == 1) {

      tab <- rbind(tab, d[i,])
    }

  }

}

sells <- tab$mean[tab$sell == 1]
buys <- tab$mean[tab$buy == 1]
gains <- rep(NA, length(buys))

for (i in 1:length(buys)) gains[i] <- (sells[i]/buys[i])-1


hist(gains)
mean(gains, na.rm = T)
mean(gains, na.rm = T) - spread
max(gains, na.rm=T) - spread


spread <- 0.0003
principal <- 10000
r <- mean(gains, na.rm = T) - spread
per_day <- length(na.omit(buys))/184

tot <- principal*(1+r)^(per_day*365)
format(tot, big.mark=',')






create_order(symbol = param$symbol,
             side = 'BUY',
             type = 'MARKET',
             quantity = round(bal_usd/get_ticker_price(param$symbol)$price, 4) - 0.0001,
             time_in_force = 'IOC',
             time_window = 10000)


create_order(symbol = param$symbol,
             side = 'SELL',
             type = 'MARKET',
             quantity = bal$free[bal$asset == param$asset],
             time_in_force = 'IOC',
             time_window = 10000)





#timestep_duration <- as.integer(strsplit(param$interval_short, 'm')[[1]])
#
#tmp <- seq(0, 60, timestep_duration)
#tmp <- tmp[!tmp==60]
#tmp <- as.POSIXct(glue("{Sys.Date()} {hour(timestamp)}:{tmp}:00", origin = "1970-01-01", tz = Sys.timezone()))
#
#t1 <- tmp[(tmp - timestamp) < 0]
#t1 <- t1[length(t1)]
#t2 <- t1 + (timestep_duration*60) * (1/5)
#
#if (timestamp < t2) {
#  wait_time <- difftime(t2, timestamp, units='secs')
#  message(glue(":: Waiting {wait_time} seconds ::"))
#  Sys.sleep(wait_time)
#}



#set config
usethis::use_git_config(user.name = "John Giles", user.email = "gilesjohnr@gmail.com")

#Go to github page to generate token
usethis::create_github_token()

#paste your PAT into pop-up that follows...
credentials::set_github_pat()

#now remotes::install_github() will work
remotes::install_github("gilesjohnr/rbinanceus")



d <- compile_data(param=param_default)

d <- compile_data(param=param_best)

hist(d$peak_seq)
table(d$peak_seq)

par(mfrow=c(1,1), xpd=F)
plot_candles(d)
lines(d$date_time, d$bb_avg, lwd=0.75, col='darkorange')
lines(d$date_time, d$bb_hi, lwd=0.5, col='goldenrod')
lines(d$date_time, d$bb_lo, lwd=0.5, col='goldenrod')

x <- 5
abline(v=d$date_time[d$bb_slope > -x & d$bb_slope < x])





n <- 100 # number of LHS replicates
Y <- randomLHS(n, 20)
A <- maximinLHS(n=n, k=20, method="build", dup=3)
G <- geneticLHS(n=n, k=20, pop=100, gen=10, pMut=.25, verbose=T)
plot(Y[,1], Y[,2])
plot(A[,1], A[,2])
plot(G[,1], G[,2])





