
################################################################################
# fCOuNT_CALC_HC_P_VALUE uses comparison to theoretic null to determine p value for HC
# In:  hc - HC statistic
#      n_test - number of tests
#      n_sim - number of simulations to run
#      k1 - fraction or number of p values to keep
#      emp - flag for using variance of empirical distribution
# Out: p - p value
################################################################################

fCOuNT_CALC_HC_P_VALUE = function(hc, n_test, n_sim, k1, emp) {
  require(parallel)
  
  if(missing(n_sim)) { n_sim = 10000 }
  
  cl = makeCluster(detectCores()-1)
  
  # Export necessary objects and functions
  script_path = normalizePath(
    file.path(getwd(), "fCOuNT.R"),
    mustWork = TRUE
  )
  
  
  clusterExport(cl, c("k1", "emp", "n_test", "script_path", "fCOuNT_HIGHER_CRITICISM"), 
                envir = environment())
 
   clusterEvalQ(cl,source(script_path))
  
  hc_vals = unlist(parLapply(cl, 1:n_sim, function(i) {
    fCOuNT_HIGHER_CRITICISM(
      p = runif(n_test),
      k1 = k1,
      emp = emp,
      qc_plot = FALSE)
  }))
  
  stopCluster(cl)
  
  p = rep(NA, length(hc))
  for(i in 1:length(hc)) {
    p[i] = 1 - sum(hc[i] > hc_vals) / n_sim
  }
  
  return(list(p=p, hc_crit=quantile(hc_vals, 0.95)))
}

