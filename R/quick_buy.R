qb <- function() {

  param <- get('param_best', envir=.GlobalEnv)

  cancel_all_orders(param$symbol)

  bal <- get_account_balance()
  bal_usd <- round(bal$free[bal$asset == 'USD'] - 6e-05, 4)

  tmp <- get_order_depth(param$symbol, limit=1)
  bid <- tmp$bid_price + (tmp$ask_price - tmp$bid_price)*0.1

  buy_order <- create_order(symbol = param$symbol,
                            side = 'BUY',
                            type = 'LIMIT',
                            price = bid,
                            quantity = round(bal_usd/bid - 6e-05, 4),
                            time_in_force = 'GTC',
                            time_window = param$time_window)


}
