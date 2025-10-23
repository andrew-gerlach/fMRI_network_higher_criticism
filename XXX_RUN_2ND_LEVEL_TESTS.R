#' XXX_RUN_2ND_LEVEL_TESTS
#'
#' @param first_level_results data frame containing first level test results with columns for
#' node1, node2, direction of test, test statistic, and p value
#' @param net_def vector defining which node
#'
#' @returns second_level_results date frame containing second level test results with columns
#' for network1, network2, direction of test, HC statistic, and p value
#' @export
#'
#' @examples

XXX_RUN_2ND_LEVEL_TESTS = function(first_level_results, net_def) {

  # pull network info from net_def
  networks = unique(net_def)
  m = length(networks)
  # number of unique network pairs
  M = m * (m + 1) / 2

  # error checking
  if(length(unique(c(first_level_results$node1, first_level_results$node2))) != length(networks)) {
    stop("The network definition contains a different number of nodes than the first level tests!")
  }

  # add network definition to 1st level test results
  first_level_results$network1 = net_def[first_level_results$node1]
  first_level_results$network2 = net_def[first_level_results$node2]

  # initialize table
  second_level_results = data.frame(network1=character(M),
                                    network2=character(M),
                                    direction=character(M),
                                    n_tests=numeric(M),
                                    HC=numeric(M),
                                    p=numeric(M))

  # table row index
  i = 0
  # Loop through network pairs
  for(m1 in 1 : m) {

    for(m2 in m1 : m) {

      # Fill in table network definitions
      second_level_results$network1 = networks[m1]
      second_level_results$network2 = networks[m2]
      # Extract relevant p values
      p = first_level_results %>%
        filter((network1 == networks[m1] & network2 == networks[m2]) | (network1 == networks[m2] & network2 == networks[m1])) %>%
        pull(p)
      # Calculate number of tests
      second_level_results$n_tests = length(p)
      # Calculate HC statistic
      second_level_results$p = XXX_HIGHER_CRITICISM(p, k1=0.5, emp=F)

    }

  }

  return(second_level_results)

}


