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
XXX_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, net1, net2, net_def){
  
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
  
  
  feature_cols = names(data %>% select(starts_with("Node")))
  
  # Access the groups once to avoid reloading the data
  groups = data$group
  
  pvals = numeric(K)
  stats = numeric(K)
  
  net_indices =  split(seq_along(net_def), net_def)
  
  net1_i = net_indices[[net1]]
  net2_i = net_indices[[net2]]
  
  results = lapply(feature_cols, function(colname) {
    feature_vals = data[[colname]]
    
    test = switch(test_type,
                  # 1: t-test
                  `1` = try(t.test(feature_vals ~ groups, data = data), silent = TRUE),
                  
                  # 2: linear model
                  `2` = try(summary(lm(feature_vals ~ groups, data = data)), silent = TRUE),
                  
                  # 3: ANOVA
                  `3` = try(summary(aov(feature_vals ~ groups, data = data)), silent = TRUE),
                  
                  stop("Invalid test_type")
    )
    
    # Extract statistic and p-value safely depending on object type
    if (inherits(test, "try-error")) {
      return(c(stat = NA_real_, pval = NA_real_))
    }
    
    if (test_type == 1) {
      # t-test
      stat = unname(test$statistic)
      pval = test$p.value
      
    } else if (test_type == 2) {
      # lm() summary — use coefficient for 'groups' term (2nd row)
      coef_table = test$coefficients
      if (nrow(coef_table) >= 2) {
        stat = coef_table[2, "t value"]
        pval = coef_table[2, "Pr(>|t|)"]
      } else {
        stat = NA_real_
        pval = NA_real_
      }
      
    } else if (test_type == 3) {
      # ANOVA summary — extract F and p-value
      aov_table = test[[1]]
      if (nrow(aov_table) >= 1) {
        stat = aov_table[1, "F value"]
        pval = aov_table[1, "Pr(>F)"]
      } else {
        stat = NA_real_
        pval = NA_real_
      }
    }
    
    return(c(stat = stat, pval = pval))
  })
  
  
  results_mat = do.call(cbind, results)
  
  # Extract vectors
  
  stats = as.numeric(results_mat["stat", ])
  pvals = as.numeric(results_mat["pval", ])
  
  names(stats) = names(pvals) = feature_cols
  
  node_stats = data.frame(stats = stats, pvals = pvals)
  
  split_names = do.call(rbind, strsplit(rownames(node_stats), "_"))
  node_stats$first_node = as.integer(gsub("Node", "", split_names[, 1]))
  node_stats$second_node = as.integer(gsub("Node", "", split_names[, 2]))
  
  if (net1 == net2) {
    # Within-network connections: both nodes must be in the same network
    net_stats = node_stats[
      node_stats$first_node_idx %in% net1_i & 
        node_stats$second_node_idx %in% net1_i, 
    ]
  } else {
    # Between-network connections: one node in each network
    net_stats = node_stats[
      (node_stats$first_node_idx %in% net1_i & node_stats$second_node_idx %in% net2_i) |
        (node_stats$first_node_idx %in% net2_i & node_stats$second_node_idx %in% net1_i),
    ]
  }
  
  net_stats$first_node_idx = NULL
  net_stats$second_node_idx = NULL
  
  return(net_stats)
  
}

data = readRDS("C:/Users/arvin/Documents/fMRI_network_higher_criticism/testing/testdata_ttest.RDS")
q = XXX_RUN_1ST_LEVEL_TESTS(data$data, data$fc, 1, "A", "B", net_def)
XXX_HIGHER_CRITICISM(q$pvals, k1 = 0.5, emp = F)
