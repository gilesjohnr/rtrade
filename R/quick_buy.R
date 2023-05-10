qb <- function() {

  param <- get('param_best', envir=.GlobalEnv)

  bal <- get_account_balance()
  bal_usd <- round(bal$free[bal$asset == 'USD'] - 1e-04, 4)

  bid <- get_order_depth(param_best$symbol)$bid_price[2]*(1.0001)

  buy_order <- create_order(symbol = param$symbol,
                            side = 'BUY',
                            type = 'LIMIT',
                            price = bid,
                            quantity = round(bal_usd/bid - 1e-04, 4),
                            time_in_force = 'GTC',
                            time_window = param$time_window)


}
