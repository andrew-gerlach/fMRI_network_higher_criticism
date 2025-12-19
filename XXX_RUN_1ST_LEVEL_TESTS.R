#' XXX_RUN_1st_LEVEL_TESTS
#'
#' @param data subject information/subject number (n subject)
#' @param fc a large 3 dimensional data set that contains the correlation matrix of brain nodes
#' of each subject(n x 50 x 50) (ADD SPECIFICS LATER)
#'
#' @returns a list of of summary statistics about the fc matrices and subject (ADD SPECIFCS LATER)
#' @export
#'
#' @examples
XXX_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, form, var){
  #Step 0: Load packages
  require(dplyr)
  require(parallel)
  
  #Step 1: Flatten FC matrix and set up results
  
  # Number of subjects
  n = nrow(data)
  # Number of nodes in FC matrices
  k = dim(fc)[2]
  
  # Error checking
  if(dim(fc)[1] != n) { stop(sprintf("%i subjects in data, but %i FC matrices provided!", n, dim(fc)[1])) }
  if(dim(fc)[3] != k) { stop("FC matrices provided are not square!") }
  
  # Boolean array for upper triangular matrix
  upper = upper.tri(diag(dim(fc)[2]), diag = FALSE)
  
  # Flatten FC matrix into a 2D array (K x n)
  K = sum(upper)
  flat_fc = apply(fc, 1, function(mat) mat[upper])
  flat_fc = as.data.frame(t(flat_fc))
  
  # Get list of unique node pairs
  node_pairs = which(upper, arr.ind = TRUE)
  colnames(flat_fc)[1:nrow(node_pairs)] =  paste0("Node", node_pairs[,1], "_Node", node_pairs[,2])
  
  # Merge group and fc data into one
  data = cbind(data, flat_fc)
  # Clear memory intensive data that's no longer needed
  rm(flat_fc)
  
  #Step 2: Run tests
  feature_cols = names(data %>% select(starts_with("Node")))
  
  cl = makeCluster(detectCores()-1)
  clusterExport(cl, c("data", "form", "test_type", "var"), envir = environment())
  
  results = parLapply(cl,1:length(feature_cols), function(i){
    
    first_level = run_tests(data, form, test_type, var)
    result$node1 = node_paris[i, 1]
    result$node2 = node_pairs[i, 2]
    return(result)
  })
  
  stopCluster(cl)
  
  # Extract vectors
  stats = sapply(results, function(x) x["stat"])
  pvals = sapply(results, function(x) x["pval"])
  
  names(stats) = names(pvals) = feature_cols
  
  node_stats = data.frame(stats = stats, pvals = pvals)
  
  split_names = do.call(rbind, strsplit(rownames(node_stats), "_"))
  node_stats$first_node  = as.integer(gsub("Node", "", split_names[, 1]))
  node_stats$second_node = as.integer(gsub("Node", "", split_names[, 2]))
  
  return(node_stats)
}


run_tests = function(data, form, test_type, var, K) {
  
  first_level_results = data.frame(node1=numeric(K),
                                   node2=numeric(K),
                                   direction=character(K),
                                   test_statistic=numeric(K),
                                   p_low=numeric(K),
                                   p_high=numeric(K))
  
  test = switch(as.character(test_type),
                # 1: t-test
                "t.one" = try(mod = t.test(form, data = data), silent = TRUE),
                
                # 2: Two sample t-test
                "t.two" = try(mod = t.test(form, data = data), silent = TRUE),
                
                # 3: Linear model
                "lm" = try(mod = lm(form, data = data), silent = TRUE),
                
                # 4: ANOVA
                "anova" = try(mod = aov(form, data = data), silent = TRUE),
                
                stop("Invalid test_type")
  )
  
  # Extract statistic and p-value safely depending on object type
  if (inherits(test, "try-error")) {
    return(NA_real_)
  }
  
  if (test_type == "t.one") {
    # One sample t.test
    first_level_results$test_statistic[idx] = mod$statistic
    first_level_results$p_low[idx] = pt(mod$statistic, mod$parameter)
    first_level_results$p_high[idx] = pt(-mod$statistic, mod$parameter)
    
  } else if (test_type == "lm") {
    # Linear regression
    mod = lm(form, data)
    coefs = coef(summary(mod))
    # TODO: CANNOT ASSUME THIS INDEX!
    first_level_results$test_statistic[idx] = coefs[var + 1, 3]
    first_level_results$p_low[idx] = pt(coefs[var + 1, 3], mod$df.residual)
    first_level_results$p_high[idx] = pt(-coefs[var + 1, 3], mod$df.residual)
    
  } else if (test_type == "anova") {
  
    # TO DO: ANOVA
    
  } else if(test_type == "t.two"){
    # Two sample t.test
    mod = t.test(form, data)
    first_level_results$test_statistic[idx] = mod$statistic
    first_level_results$p_low[idx] = pt(mod$statistic, mod$parameter)
    first_level_results$p_high[idx] = pt(-mod$statistic, mod$parameter)
  } else{
    
    stop(paste("Test type", test_type, "is not supported!"))
  }
  
    return(first_level_results)
  }
