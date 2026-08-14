#' Calculation of p value for higher criticism
#'
#' This routine  uses comparison to theoretic null to determine p value for HC
#'
#' @param hc HC statistic(s) (numeric vector)
#' @param n_test number of tests (integer)
#' @param n_sim number of simulations to run (integer)
#' @param k1 fraction or number of p values to keep (numeric)
#' @param parallel_opts list of options for running in parallel
#' @param emp flag for using variance of empirical distribution rather than theoretical distribution (boolean)
#'
#' @return p p-value(s) for provided HC statistic(s) (numeric vector)
#' @return hc_crit critical HC value for p = 0.05 given n_test and options
#'
#' @export

fCOuNT_CALC_HC_P_VALUE = function(hc, n_test, n_sim, k1, emp, parallel_opts) {

  hc_fun = function(i) {

    fCOuNT_HIGHER_CRITICISM(p = runif(n_test),
                            k1 = k1,
                            emp = emp) %>%
      max(na.rm = TRUE)

  }

  if(!parallel_opts$parallel) {

    hc_vals = unlist(lapply(seq_len(n_sim),
                            function(i) {
                              fCOuNT_HIGHER_CRITICISM(p = runif(n_test),
                                                      k1 = k1,
                                                      emp = emp) %>%
                                max(na.rm = TRUE)} ))

  } else {

    plan(multisession)
    hc_vals = unlist(future_lapply(seq_len(n_sim),
                                   function(i) {
                                     fCOuNT_HIGHER_CRITICISM(p = runif(n_test),
                                                             k1 = k1,
                                                             emp = emp) %>%
                                       max(na.rm = TRUE)},
                                   future.seed = TRUE))

    plan(sequential)

  }

  p = rep(NA, length(hc))
  for(i in 1 : length(hc)) {
    p[i] = 1 - sum(hc[i] > hc_vals) / n_sim
  }

  return(list(p=p, hc_crit=quantile(hc_vals, 0.95)))

}
