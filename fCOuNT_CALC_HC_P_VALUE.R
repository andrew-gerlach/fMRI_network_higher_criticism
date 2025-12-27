
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
<<<<<<< HEAD
  require(parallel)
  
  if(missing(n_sim)) { n_sim = 10000 }
  
  cl = makeCluster(detectCores()-1)
  
  # Export necessary objects and functions
  clusterEvalQ(cl, {
    source("C:\\Users\\arvin\\Documents\\fMRI_network_higher_criticism\\fCOuNT_HIGHER_CRITICISM.R")
  })
  
  clusterExport(cl, c("k1", "emp", "n_test"), 
                envir = environment())
  
  hc_vals = unlist(parLapply(cl, 1:n_sim, function(i) {
    fCOuNT_HIGHER_CRITICISM(
      p = runif(n_test),
      k1 = k1,
      emp = emp,
      plot = FALSE)
  }))
  
  stopCluster(cl)
  
=======

  hc_vals = unlist(mclapply(1:n_sim, function(i) {
    fCOuNT_HIGHER_CRITICISM(
      p=runif(n_test),
      k1=k1,
      emp=emp,
      qc_plot=FALSE)
    }, mc.cores = detectCores()))
>>>>>>> 9266ad53c2c910aaa55b295181fa25091e80ceb9
  p = rep(NA, length(hc))
  for(i in 1:length(hc)) {
    p[i] = 1 - sum(hc[i] > hc_vals) / n_sim
  }
  
  return(list(p=p, hc_crit=quantile(hc_vals, 0.95)))
}

