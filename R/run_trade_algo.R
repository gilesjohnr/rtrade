run_trade_algo <- function(param, live=FALSE) {

  if (live) {

    run_trade_algo_live(param) # does not return anything

  } else {

    return(run_trade_algo_paper(param)) # returns trade record

  }

}
