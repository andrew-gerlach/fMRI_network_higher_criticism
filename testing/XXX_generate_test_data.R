# Function for generating test data
# In:  n - number of subject
#      k - number of nodes in parcellation
#      m - number of networks (not used currently)
#      mu - effect strength
#      tau - effect sparsity
#      seed - seed for replicability
# Out: data - data frame of variables
#      fc - connectivity matrices

XXX_generate_test_data = function(n, k, m, mu, tau, seed) {
  
  # set seed for replicability
  set.seed(seed)
  
  # data frame with subject number and 2 groups
  data = data.frame(subj = factor(1 : n),
                    group = rep(0 : 1, each = n / 2))
  
  # number of unique entries
  K = k * (k - 1) / 2
  
  # initialize connectivity matrices, stacked by subject
  fc = array(NA, c(n, k, k))
  
  # generate symmetric matrices
  for(i in 1 : n) {
    
    # initialize subject matrix
    mat = matrix(0, k, k)
    
    # random values for upper triangle   
    mat[upper.tri(mat)] = rnorm(K)
    
    # mirror to lower triangle
    mat = mat + t(mat)
    
    # Set diagonal to one
    diag(mat) = 1
    
    # Store in stacked array
    fc[i, , ] = mat

  }
  
  # inject sparse, weak signal
  for(j in 1 : round(tau * K)) {
    
    # random indices
    idx = sample(1 : k, 2, replace = F)
    # add signal to group 1 but not group 0
    fc[, idx[1], idx[2]] = fc[, idx[1], idx[2]] + data$group * mu
    fc[, idx[2], idx[1]] = fc[, idx[1], idx[2]]
    
  }
  
  # set group to factor after using as integer
  data$group = factor(data$group)
  
  return(list(data=data, fc=fc))

}