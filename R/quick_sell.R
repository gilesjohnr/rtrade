qs <- function() {

  param <- get('param_best', envir=.GlobalEnv)

  cancel_all_orders(param$symbol)

  bal <- get_account_balance()

  tmp <- get_order_depth(param$symbol, limit=1)
  #ask <- tmp$ask_price - 0.01
  ask <- tmp$ask_price - (tmp$ask_price - tmp$bid_price)*0.05


  sell_order <- create_order(symbol = param$symbol,
                             side = 'SELL',
                             type = 'LIMIT',
                             price = ask,
                             quantity = round(bal$free[bal$asset == param$asset] - 6e-05, 4),
                             time_in_force = 'GTC',
                             time_window = param$time_window)

}
