
#' Run 1st level tests
#'
# ARG: let's not assume that the data frame and fc will come as a single object
# for input purposes. Also want to keep it more flexible for things like
# regression and such because most of this will be directly usable for other
# tests up until the for loop
XXX_RUN_1ST_LEVEL_TESTS = function(data, fc){
  
  library(parallel)
  library(dplyr)
  
  # Boolean array for upper triangular matrix
  upper = upper.tri(diag(dim(fc)[2]), diag = FALSE)
  # Number of unique entries in upper triangular matrix
  K = sum(upper)
  # Flatten fc data into a 2D array (K x n)
  flat_fc = apply(fc, 1, function(mat) mat[upper])
  # Convert to wide data frame
  flat_fc = as.data.frame(t(flat_fc))
  
  # Get list of unique node pairs
  node_pairs = which(upper, arr.ind = TRUE)
  colnames(flat_fc)[1:nrow(node_pairs)] =  paste0("Node", node_pairs[,1], "_Node", node_pairs[,2])
  
  # Merge group and fc data into one
  data = cbind(data, flat_fc)
  # Clear memory intensive data that's no longer needed
  rm(flat_fc)
  
  # Chunk the data by column (features of the fc matrix)
  feature_cols = names(data %>% select(starts_with("Node")))
  pvals = numeric(K)
  stats = numeric(K)
  
  for(i in seq_along(feature_cols)) {
    
    # ARG: this creates extra variables that we don't need, prefer not to do
    # that whenever possible
    # col = feature_cols[i]
    # y = df[[col]]
    # x = df$group
    
    # T-test formula
    f = formula(paste(feature_cols[i], "~ group"))
    # Run t-test
    tt = t.test(f, data)
    
    # Extract statistics and values
    stats[i] = unname(tt$statistic)
    pvals[i] = tt$p.value
    
  }
  
  return (list(stats = stats, pvals = pvals))
  
}

data = readRDS("C:/Users/arvin/Documents/fMRI_network_higher_criticism/testing/testdata_ttest.RDS")
q = XXX_RUN_1ST_LEVEL_TESTS(data$data, data$fc)
q
