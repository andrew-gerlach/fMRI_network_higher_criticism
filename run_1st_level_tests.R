# ARoG: temporary script for running 1st level tests

XXX_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, form) {
  
  k = dim(fc)[2]
  K = k * (k - 1) / 2
  
  first_level_results = data.frame(node1=numeric(K),
                                   node2=numeric(K),
                                   direction=character(K),
                                   test_statistic=numeric(K),
                                   p=numeric(K))
  
  idx = 0
  for(i in 1 : (k - 1)) {
    for(j in (i + 1) : k) {
      
      # fill in table
      idx = idx + 1
      first_level_results$node1[idx] = i
      first_level_results$node2[idx] = j
      
      # add fc to data
      data$fc = fc[, i, j]
      
      # perform tests
      if(test_type == "t.one") {
        
        # one sample t test
        mod = t.test(data$fc)
        first_level_results$test_statistic = mod$statistic
        first_level_results$p[idx] = mod$p.value
        
      } else if(test_type == "t.two") {
        
        # two sample t test
        mod = t.test(form, data)
        first_level_results$test_statistic = mod$statistic
        first_level_results$p[idx] = mod$p.value
        
      } else if(test_type == "anova") {
        
        # ANOVA
        
      } else if(test_type == "regression") {
        
        # linear regression
        
      } else {
        stop(paste("Test type", test_type, "is not supported!"))
      }
    }
  }
  
  return(first_level_results)

}