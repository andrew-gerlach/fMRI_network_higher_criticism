#' Generate list of higher criticism options
#'
#' @param k1 fraction or number of p values to keep (numeric)
#' @param emp flag for using variance of empirical distribution rather than theoretical distribution (boolean)
#' @param n_sim number of simulations to run (integer)

fCOuNT_GEN_HC_OPTIONS = function(k1, emp, nsim) {

  # initialize
  hc_opts = list(k1=NA,
                 emp=NA,
                 nsim=NA)

  # set defaults for missing
  if(missing(k1) | is.null(k1)) {
    hc_opts$k1 = 0.5
  } else {
    hc_opts$k1 = k1
  }
  if(missing(emp) | is.null(emp)) {
    hc_opts$emp = T
  } else {
    hc_opts$emp = emp
  }
  if(missing(nsim) | is.null(nsim)) {
    hc_opts$nsim = 1E5
  } else {
    hc_opts$nsim = nsim
  }

  return(hc_opts)

}
