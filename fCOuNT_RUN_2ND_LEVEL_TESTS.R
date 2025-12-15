#' XXX_RUN_2ND_LEVEL_TESTS
#'
#' @param first_level_results data frame containing first level test results with columns for
#' node1, node2, direction of test, test statistic, and p value (n rows)
#' @param net_def vector defining which network each node belongs to (length k)
#' @param hc_opts options for higher criticism
#'
#' @returns second_level_results date frame containing second level test results with columns
#' for network1, network2, direction of test, HC statistic, and p value
#' @export
#'
#' @examples

XXX_RUN_2ND_LEVEL_TESTS = function(first_level_results, net_def, hc_opts) {

  # set default higher criticism options
  # NOTE: deprecate this eventually (handle in main routine)
  if(missing(hc_opts)) {
    hc_opts = list(k1=0.5, emp=F, nsim=1E5, plot=T)
  }

  # pull network info from net_def
  networks = unique(net_def[!is.na(net_def)])
  m = length(networks)
  # number of unique network pairs
  M = m * (m + 1) / 2

  # error checking
  if(length(unique(c(first_level_results$node1, first_level_results$node2))) != length(net_def)) {
    stop("The network definition contains a different number of nodes than the first level tests!")
  }

  # add network definition to 1st level test results
  first_level_results$network1 = net_def[first_level_results$node1]
  first_level_results$network2 = net_def[first_level_results$node2]

  # initialize table
  second_level_results = data.frame(network1=character(2 * M),
                                    network2=character(2 * M),
                                    direction=rep(c("low", "high"), M),
                                    n_tests=numeric(2 * M),
                                    HC=numeric(2 * M),
                                    p=numeric(2 * M))

  # table row index
  i = -1
  # initialize plot storage
  hc_plots = list()

  # Loop through network pairs
  for(m1 in 1 : m) {

    for(m2 in m1 : m) {

      i = i + 2
      hc_plots[[(i + 1) / 2]] = list()
      # Fill in table network definitions
      second_level_results$network1[i : (i+1)] = networks[m1]
      second_level_results$network2[i : (i+1)] = networks[m2]
      # Extract relevant p values
      p_low = first_level_results %>%
        filter((network1 == networks[m1] & network2 == networks[m2]) | (network1 == networks[m2] & network2 == networks[m1])) %>%
        pull(p_low)
      p_high = first_level_results %>%
        filter((network1 == networks[m1] & network2 == networks[m2]) | (network1 == networks[m2] & network2 == networks[m1])) %>%
        pull(p_high)
      # Calculate number of tests
      second_level_results$n_tests[i : (i+1)] = length(p_low)
      # Calculate HC statistic for low direction
      tmp = XXX_HIGHER_CRITICISM(p=p_low,
                                 k1=hc_opts$k1,
                                 emp=hc_opts$emp,
                                 plot=hc_opts$plot)
      second_level_results$HC[i] = tmp$hc
      hc_plots[[(i + 1) / 2]][["low"]] = tmp$plot
      # Calculate HC statistic for high direction
      tmp = XXX_HIGHER_CRITICISM(p=p_high,
                                 k1=hc_opts$k1,
                                 emp=hc_opts$emp,
                                 plot=hc_opts$plot)
      second_level_results$HC[i + 1] = tmp$hc
      # Calculate p values for HC
      tmp = XXX_CALC_HC_P_VALUE(
        second_level_results$HC[i : (i + 1)],
        length(p_low),
        n_sim=hc_opts$nsim,
        k1=hc_opts$k1,
        emp=hc_opts$emp)
      second_level_results$p[i : (i + 1)] = tmp$p
      hc_plots[[(i + 1) / 2]][["high"]] = tmp$plot +
        geom_line(data=data.frame(x=c(0, 1),
                                  y=rep(tmp$hc_crit, 2)),
                  mapping=aes(x, y),
                  color="red",
                  size=2)

    }

  }

  return(list(second_level_results=second_level_results,
              hc_plot=hc_plots))

}


