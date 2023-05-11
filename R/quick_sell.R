qs <- function() {

  param <- get('param_best', envir=.GlobalEnv)

  bal <- get_account_balance()
  ask <- get_order_depth(param$symbol)$ask_price[2]*(1-0.0001)

  sell_order <- create_order(symbol = param$symbol,
                             side = 'SELL',
                             type = 'LIMIT',
                             price = ask,
                             quantity = round(bal$free[bal$asset == param$asset] - 1e-05, 4),
                             time_in_force = 'GTC',
                             time_window = param$time_window)

}
