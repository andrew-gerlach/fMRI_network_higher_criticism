# Function for generating test data
# In:  n - number of subjects
#      k - number of nodes in parcellation
#      net_def - network definition for nodes
#      mu - effect strength (1 per network pair)
#      tau - effect sparsity (1 per network pair)
#      seed - seed for replicability
# Out: data - data frame of variables
#      fc - connectivity matrices

fCOuNT_GEN_TEST_DATA = function(n, k, net_def, mu, tau, seed) {

  # set seed for replicability
  set.seed(seed)
  
  # number of unique entries
  K = k * (k - 1) / 2
  
  # initialize connectivity matrices, stacked by subject
  fc = array(NA, c(n, k, k))
  
  # initialize data frame with:
  data = data.frame(subj = factor(1 : n),                               # subject ID
                    x = c(rnorm(2 * n / 3, 0, 1), rnorm(n / 3, 1, 1)),  # continuous ind. var.
                    group2 = factor(rep(0 : 1, each = n / 2)),          # 2-level ind. var.
                    group3 = factor(rep(0 : 2, each = n / 3)),          # 3-level ind. var.
                    age = rnorm(n, 45, 8),                              # continuous control
                    sex = factor(rep(c("F", "M"), n / 2)),              # 2-level control
                    site = factor(rep(c("A", "B", "C"), n / 3)))        # 3-level control
                    
  # generate random symmetric matrices
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

  # extract networks
  networks = unique(net_def)
  # number of networks
  m = length(networks)
  # number of network pairs
  M = m * (m + 1) / 2

  net_pair = 0

  for(i in 1 : m) {

    for(j in i : m) {

      net_pair = net_pair + 1

      if(mu[net_pair] == 0) {
        # skip if no signal in this network pair
        next
      } else {
        # otherwise determine the number of node pairs in network pair
        if(i == j) {
          K_net = sum(net_def == networks[i]) * (sum(net_def == networks[j]) - 1) / 2
        } else {
          K_net = sum(net_def == networks[i]) * sum(net_def == networks[j])
        }
      }

      # inject sparse/weak signal
      for(l in 1 : round(tau[net_pair] * K_net)) {

        # random indices
        idx1 = sample((1 : k)[which(net_def == networks[i])], 1, replace = F)
        idx2 = sample((1 : k)[which(net_def == networks[j])], 1, replace = F)
        
        # add signal to higher x (group2: 2 > 1, group3: 3 > 2, 1)
        fc[, idx1, idx2] = fc[, idx1, idx2] + data$x * mu[net_pair]

      }

    }

  }

  return(list(data=data, fc=fc, net_def=net_def))

}
