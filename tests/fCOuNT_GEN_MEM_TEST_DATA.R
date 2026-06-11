# Function for generating mixed effects model test data
# In:  n - number of subjects
#      nt - number of time points per subject
#      net_def - network definition for nodes
#      mu - fixed effect strength (1 per network pair)
#      tau - fixed effect sparsity (1 per network pair)
#      seed - seed for replicability
# Out: data - data frame of variables
#      fc - connectivity matrices

fCOuNT_GEN_MEM_TEST_DATA = function(n, nt, net_def, mu, tau, seed) {
  
  # set seed for replicability
  set.seed(seed)
 
  # number of nodes and unique entries
  k = net_def %>% length()
  K = k * (k - 1) / 2
  
  # check for compatibility in network definition/effects
  m = net_def %>% unique() %>% length()
  M = m * (m + 1) / 2
  if(length(mu) != M) {
    stop("Number of fixed effect strengths provided (%i) does not match number of total network pairs (%i)", length(mu), M)
  }
  if(length(tau) != M) {
    stop("Number of fixed effect sparsities provided (%i) does not match number of total network pairs (%i)", length(tau), M)
  }

  # initialize connectivity matrices, stacked by subject
  fc = array(NA, c(n * nt, k, k))
  
  # adjust n for divisibility by 2
  if(n %% 2 != 0) { 
    n = n - n %% 2
    warning("Adjusting n for divisibility into 2 groups")
  }
  
  # initialize data frame with:
  data = data.frame(subj = factor(rep(1 : n, each = nt)),             # subject ID
                    t = rep(1 : nt, n),                               # time variable
                    x = rep(c(rnorm(n / 2, 0, 1), rnorm(n / 2, 1, 1)), each = nt),   # continuous ind. var.
                    group = rep(0 : 1, each = nt * n / 2))            # 2-level ind. var.
                    
  # generate random symmetric matrices
  for(s in 1 : n) {

    # initialize subject matrix
    mat_fixed = matrix(0, k, k)
    # random values for upper triangle
    mat_fixed[upper.tri(mat_fixed)] = rnorm(K)
    # mirror to lower triangle
    mat_fixed = mat_fixed + t(mat_fixed)
    # Set diagonal to one
    diag(mat_fixed) = 1
    
    # Store in stacked array
    for(t in 1 : nt) {
      
      # initialize time matrix
      mat_random = matrix(0, k, k)
      # random values for upper triangle
      mat_random[upper.tri(mat_random)] = rnorm(K)
      # mirror to lower triangle
      mat_random = mat_random + t(mat_random)
      # Set diagonal to one
      diag(mat_random) = 1
      
      fc[((s - 1) * nt + t), , ] = mat_fixed + mat_random
      
    }

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
      
      # skip if no signal in this network pair
      if(mu[net_pair] == 0) { next }

      # determine the number of node pairs in network pair
      if(i == j) {
        K_net = sum(net_def == networks[i]) * (sum(net_def == networks[j]) - 1) / 2
      } else {
        K_net = sum(net_def == networks[i]) * sum(net_def == networks[j])
      }
      
      # random indices
      # TODO: add logic to preclude selecting diagonal or repeated indices
      idx1 = sample((1 : k)[which(net_def == networks[i])],
                    round(tau[net_pair] * K_net),
                    replace = T)
      idx2 = sample((1 : k)[which(net_def == networks[j])],
                    round(tau[net_pair] * K_net),
                    replace = T)
      
      # inject sparse/weak signal
      for(l in 1 : length(idx1)) {

        # add signal to group (higher x) and higher t 
        fc[, idx1[l], idx2[l]] = fc[, idx1[l], idx2[l]] + data$group * data$t * mu[net_pair]

      }
      
    }

  }
  
  data$group = factor(data$group)

  return(list(data=data, fc=fc, net_def=net_def))

}
