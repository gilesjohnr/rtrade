run_trade_algo <- function(param, live=FALSE, verbose=TRUE) {

  if (live) {

    run_trade_algo_live(param=param, verbose=verbose) # does not return anything

  } else {

    return(run_trade_algo_paper(param=param, verbose=verbose)) # returns trade record

  }

}
