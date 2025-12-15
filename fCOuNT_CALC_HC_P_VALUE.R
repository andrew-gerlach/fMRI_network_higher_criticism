
################################################################################
# XXX_CALC_HC_P_VALUE uses comparison to theoretic null to determine p value for HC
# In:  hc - HC statistic
#      n_test - number of tests
#      n_sim - number of simulations to run
#      k1 - fraction or number of p values to keep
#      emp - flag for using variance of empirical distribution
# Out: p - p value
################################################################################

XXX_CALC_HC_P_VALUE = function(hc, n_test, n_sim, k1, emp) {

  if(missing(n_sim)) { n_sim = 10000 }
  
  hc_vals = unlist(mclapply(1:n_sim, function(i) {
    XXX_HIGHER_CRITICISM(
      p=runif(n_test),
      k1=k1,
      emp=emp,
      plot=FALSE)
    }, mc.cores = detectCores()))
  p = rep(NA, length(hc))
  for(i in 1:length(hc)) {
    p[i] = 1 - sum(hc[i] > hc_vals) / n_sim
  }
    
  return(list(p=p, hc_crit=quantile(hc_vals, 0.95)))
    
}

