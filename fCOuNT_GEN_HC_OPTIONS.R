
fCOuNT_GEN_HC_OPTIONS = function(k1, emp, nsim, qc_plot) {

  # initialize
  hc_opts = list(k1=NA,
                 emp=NA,
                 nsim=NA,
                 qc_plot=NA)

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
  if(missing(qc_plot) | is.null(qc_plot)) {
    hc_opts$qc_plot = T
  } else {
    hc_opts$qc_plot = qc_plot
  }

  return(hc_opts)

}
