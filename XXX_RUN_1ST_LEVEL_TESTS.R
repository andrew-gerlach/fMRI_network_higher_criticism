
#' Run 1st level tests
#'
t_test <- function(data){
  library(parallel)
  library(dplyr)
  
  #Get the group and fc data
  group = data[[1]]
  fc = data[[2]]
  
  #Flatten fc data into a vector
  upper = upper.tri(diag(dim(fc)[2]), diag = FALSE)
  
  flat_fc = apply(fc, 1, function(mat) mat[upper])
  flat_fc = as.data.frame(t(flat_fc))
  
  node_pairs = which(upper, arr.ind = TRUE)
  colnames(flat_fc)[1:nrow(node_pairs)] =  paste0("Node", node_pairs[,1], "_Node", node_pairs[,2])
  
  #Merge group and fc data into one
  df = cbind(group, flat_fc)
  
  #Chunk the data by column(features of the fc matrix)
  feature_cols = names(df %>% select(starts_with("Node")))
  pvals = numeric(length(feature_cols))
  tstats = numeric(length(feature_cols))
  
  for(i in seq_along(feature_cols))
  {
    col = feature_cols[i]
    y = df[[col]]
    x = df$group
    
    tt = t.test(y~x)
    
    pvals[i] = tt$p.value
    tstats[i] = unname(tt$statistic)
  }
  
  
   return (list(c(pvals), c(tstats)))
}

data = readRDS("C:/Users/arvin/Documents/fMRI_network_higher_criticism/testing/testdata_ttest.RDS")
q <- t_test(data)
q
