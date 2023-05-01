run_trade_algo <- function(par, live=FALSE) {

  if (live) {

    run_trade_algo_live(par) # does not return anything

  } else {

    return(run_trade_algo_paper(par)) # returns trade record

  }

}
