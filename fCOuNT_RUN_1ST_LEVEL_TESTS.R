#' XXX_RUN_1st_LEVEL_TESTS
#'
#' @param data subject information/subject number (n subject)(data.frame, n rows)
#' @param fc functional connectivity matrices (3D array, n x k x k)
#' @param test_type first level test type (t.one, t.two, regression, anova)(string)
#' @param form formula for fist level test (string)
#' @param var variable of interest (string)
#' @returns first level summary results (p-values, test statistic, node connections)
#' @export
#'
#' @examples
fCOuNT_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, form, var_idx){
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
  clusterExport(cl, c("data", "feature_cols", "test_type", "form", "var"
                      ,"node_pairs", "test_one_feature"), envir = environment())
  
  results_list = parLapply(cl, 1:length(feature_cols), test_one_feature,
                           data = data, feature_cols = feature_cols, 
                           test_type = test_type, form = form, var = var,
                           node_pairs = node_pairs)
  
  stopCluster(cl)
  
  # Combine results into data frame
  first_level_results = do.call(rbind, lapply(results_list, function(x) {
    data.frame(
      node1 = x$node1,
      node2 = x$node2,
      direction = x$direction,
      test_statistic = x$test_statistic,
      p_low = x$p_low,
      p_high = x$p_high,
      stringsAsFactors = FALSE
    )
  }))
  return(first_level_results)
}
test_one_feature = function(idx, data, feature_cols, test_type, form, var, node_pairs){
  #Get the name of the feature columns
  fc_col = feature_cols[idx]
  
  #Rename to fc and copy data for each parallel process
  data_copy = data
  data_copy$fc = data_copy[[fc_col]]
  
  result = list(
    node1 = node_pairs[idx, 1],
    node2 = node_pairs[idx, 2],
    direction = NA,
    test_statistic = NA,
    p_low = NA,
    p_high = NA
  )
  
  if(test_type == "t.one"){
    # One sample t.test
    mod = try(t.test(data_copy$fc), silent = TRUE)
    if(!inherits(mod, "try-error")){
      result$test_statistic = mod$statistic
      result$p_low = pt(mod$statistic, mod$parameter)
      result$p_high = pt(-mod$statistic, mod$parameter)
    }
  }
  else if (test_type == "t.two"){
    # Two sample t.test
    mod = try(t.test(as.formula(form), data_copy), silent = TRUE)
    if(!inherits(mod, "try-error")){
      result$test_statistic = mod$statistic
      result$p_low = pt(mod$statistic, mod$parameter)
      result$p_high = pt(-mod$statistic, mod$parameter)
    }
  }
  else if(test_type == "anova"){
    # ANOVA Test
    mod = try(aov(as.formula(form), data_copy), silent = TRUE)
    if(!inherits(mod, "try-error")){
      result$test_statistic = summary(mod)[[1]]$`F value`[1]
      #ANOVA test is non-directional so only one p-value
      results$p_low = summary(mod)[[1]]$`Pr(>F)`[1] 
      results$p_high = NA
    }
  }
  else if(test_type == "ancova"){
    # ANCOVA Test (ANOVA with covariates)
    mod = try(aov(as.formula(form), data_copy), silent = TRUE)
    if(!inherits(mod, "try-error")){
      result$test_statistic = summary(mod)[[1]]$`F value`[1]
      result$p_low = summary(mod)[[1]]$`Pr(>F)`[1]
      result$p_high = NA
    }
  }
  else if(test_type == "regression"){
    # Linear Regression
    mod = try(lm(as.formula(form), data_copy), silent = TRUE)
    if(!inherits(mod, "try-error")){
      coefs = coef(summary(mod))
      result$test_statistic = coefs[var + 1, 3]
      result$p_low = pt(coefs[var + 1, 3], mod$df.residual)
      result$p_high = pt(-coefs[var + 1, 3], mod$df.residual)
    }
  }
  else{
    stop(paste("Test type", test_type, "is not supported!"))
  }
  return(result)
}
