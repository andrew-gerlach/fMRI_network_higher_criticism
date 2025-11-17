#' Routine to retrieve FC matrices from file names and pull into array
#'
#' @param data data frame containing subject level variables (n rows)
#' @param fc_col_name column name in data with subject level FC files (string)

XXX_RETRIEVE_FC_MATRICES = function(data, fc_col_name) {
  
  n = nrow(data)
  
  fc_mat = # figure out how to read in the first FC matrix
  
  # extract size and basic checks
  if(length(dim(fc_mat)) != 2) { stop("The FC matrix is not 2D!") }
  k = dim(fc_mat)[1]
  if(k != dim(fc_mat)[2]) { stop("The FC matrix is not square!") }
  
  # initialize FC storage array
  fc = array(NA, c(n, k, k))
  fc[1, , ] = fc_mat
  
  for(subj in 2 : n) {
    
    # check if file exists
    if(!file.exists(data[fc_col_name, subj])) {
      warning(sprintf("FC matrix not found: %s", data[fc_col_name, subj]))
      next
    }
    
    fc_mat = # figure out how to read in the FC matrix
    fc[subj, , ] = fc_mat
    
  }
  
  
  
}