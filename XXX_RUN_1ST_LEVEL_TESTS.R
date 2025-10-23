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
XXX_RUN_1ST_LEVEL_TESTS = function(data, fc){
  
  library(parallel)
  library(dplyr)
  
  # Number of subjects
  n = nrow(data)
  
  # Number of nodes in FC matrices
  k = dim(fc)[2]
  
  # Error checking
  if(dim(fc)[1] != n) { stop(sprintf("%i subjects in data, but %i FC matrices provided!", n, dim(fc)[1])) }
  if(dim(fc)[3] != k) { stop("FC matrices provided are not square!") }
  
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
  chunk = chunk_data(data, feature_cols, 100)
  
  # Access the groups once to avoid reloading the data
  groups = data$group
  
  pvals = numeric(K)
  stats = numeric(K)
  
  
  results = mclapply(chunk, function(chunk){
    
    # Runs apply on the columns and finds the stats
    chunk_stats = apply(chunk,2, function(feature_vals){
      
      # Run t-test
      tt = t.test(feature_vals~groups)
      c(stat = unname(tt$statistic), pval = tt$p.value)
    })
    return(chunk_stats)
  })
  
  results_mat = do.call(cbind, results)
  
  # Extract vectors
  stats = as.numeric(results_mat["stat", ])
  pvals = as.numeric(results_mat["pval", ])
  
  names(stats) =  names(pvals) =  feature_cols
  
  return (list(stats = stats, pvals = pvals))
  
}
#' Chunk_data: Used to break down the large data set into chunks to run various tests in
#' parallel 
#'
#' @param data The overall large data set that needs to be broken down
#' @param feature_cols The names of the columns for chucking
#' @param chunk_size The number of columns/features per chunk
#'
#' @returns Returns the chunked data for the specified size
#' @export
#'
#' @examples
chunk_data = function(data, feature_cols, chunk_size){
  
  # Subset the data for only the necessary columns
  data_subset = data[ ,feature_cols, drop = FALSE]
  n_features = length(feature_cols)
  
  # Label the chunks by index
  chunk_index = split(seq_len(n_features), 
                      ceiling(seq_along(feature_cols)/chunk_size))
  
  # Create the chunks by index
  chunks = lapply(chunk_index, function(idx) {
    data_subset[, idx, drop = FALSE]
  })
  
  return(chunks)
}

data = readRDS("C:/Users/arvin/Documents/fMRI_network_higher_criticism/testing/testdata_ttest.RDS")
q = XXX_RUN_1ST_LEVEL_TESTS(data$data, data$fc)
q
