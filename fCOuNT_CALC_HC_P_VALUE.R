#' Calculation of p value for higher criticism
#'
#' This routine  uses comparison to theoretic null to determine p value for HC
#'
#' @param hc HC statistic (numeric vector)
#' @param n_test number of tests (integer)
#' @param n_sim number of simulations to run (integer)
#' @param k1 fraction or number of p values to keep (numeric)
#' @param parallel_opts list of options for running in parallel
#' @param emp flag for using variance of empirical distribution rather than theoretical distribution (boolean)

fCOuNT_CALC_HC_P_VALUE = function(hc, n_test, n_sim, k1, emp, parallel_opts) {

  if(!parallel_opts$parallel) {

    # serial computation
    hc_vals = rep(NA, n_sim)
    for(i in 1 : n_sim) {
      hc_vals[i] = fCOuNT_HIGHER_CRITICISM(p=runif(n_test),
                                           k1=k1,
                                           emp=emp) %>%
        max(na.rm=T)
    }

  } else {

    # determine number of nodes to used for parallel computation
    if(is.na(parallel_opts$nodes)) {
      nodes = detectCores()
    } else {
      nodes = parallel_opts$nodes
    }

    if(parallel_opts$systype == "unix") {
      hc_vals = unlist(mclapply(1:n_sim, function(i) {
        fCOuNT_HIGHER_CRITICISM(p=runif(n_test),
                                k1=k1,
                                emp=emp) %>%
          max(na.rm=T)
        }, mc.cores = nodes))
    } else {
      #TODO: implement parallel for windows
    }
  }

  p = rep(NA, length(hc))
  for(i in 1 : length(hc)) {
    p[i] = 1 - sum(hc[i] > hc_vals) / n_sim
  }

  return(list(p=p, hc_crit=quantile(hc_vals, 0.95)))

}

