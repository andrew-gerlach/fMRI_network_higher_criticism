#' fCOuNT_RUN_2ND_LEVEL_TESTS
#'
#' @param first_level_results data frame containing first level test results with columns for
#' node1, node2, direction of test, test statistic, and p value (n rows)
#' @param net_def vector defining which network each node belongs to (length k)
#' @param hc_opts options for higher criticism
#' @param parallel_opts list of parallel computing control options 
#'
#' @returns second_level_results date frame containing second level test results with columns
#' for network1, network2, direction of test, HC statistic, and p value
#' @export
#'
#' @examples

fCOuNT_RUN_2ND_LEVEL_TESTS = function(first_level_results, net_def, hc_opts, parallel_opts) {

  # set default plot theme for QC plot
  dpt = theme(text=element_text(size=18),
              strip.background=element_blank(),
              panel.background=element_blank(),
              axis.line=element_line(),
              axis.ticks=element_blank())
  
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
  qc_plots = list()

  # Loop through network pairs
  for(m1 in 1 : m) {

    for(m2 in m1 : m) {

      i = i + 2
      qc_plots[[(i + 1) / 2]] = list()
      # Fill in table network definitions
      second_level_results$network1[i : (i+1)] = networks[m1]
      second_level_results$network2[i : (i+1)] = networks[m2]

      # Extract relevant portion of data
      tmp_data = first_level_results %>%
        filter((network1 == networks[m1] & network2 == networks[m2]) |
               (network1 == networks[m2] & network2 == networks[m1]))

      # Calculate number of tests
      n_tests = sum(!is.na(tmp_data$p_low))
      second_level_results$n_tests[i : (i+1)] = n_tests

      # Calculate HC statistic for low direction
      hc_low = fCOuNT_HIGHER_CRITICISM(p=tmp_data$p_low,
                                       k1=hc_opts$k1,
                                       emp=hc_opts$emp)
      second_level_results$HC[i] = max(hc_low, na.rm=T)
      # Calculate HC statistic for high direction
      hc_high = fCOuNT_HIGHER_CRITICISM(p=tmp_data$p_high,
                                        k1=hc_opts$k1,
                                        emp=hc_opts$emp)
      second_level_results$HC[i + 1] = max(hc_high, na.rm=T)

      # Calculate p values for HC
      tmp = fCOuNT_CALC_HC_P_VALUE(second_level_results$HC[i : (i + 1)],
                                   n_tests,
                                   n_sim=hc_opts$nsim,
                                   k1=hc_opts$k1,
                                   emp=hc_opts$emp,
                                   parallel_opts=parallel_opts)
      second_level_results$p[i : (i + 1)] = tmp$p
      
      ### Quality control plots

      # Low direction p value histogram
      qc_plots[[(i + 1) / 2]][[1]] = tmp_data %>% 
        ggplot(aes(p_low, after_stat(density))) +
        geom_histogram(color="black", bins=round(n_tests / 10)) +
        geom_hline(yintercept=1, color="red", size=2) +
        xlab("First level p values (low direction)") +
        ylab("Density") +
        dpt
        
      # Low direction HC plot
      qc_plots[[(i + 1) / 2]][[2]] = tmp_data %>%
        arrange(p_low) %>%
        head(length(hc_low)) %>%
        cbind(data.frame(index = 1 : length(hc_low), HC = hc_low)) %>%
        ggplot(aes(index, HC)) +
        geom_point() +
        geom_line() +
        geom_hline(yintercept=tmp$hc_crit, color="red", size=2) +
        xlab("Test index (sorted by low p-values)") +
        ylab("Higher Criticism statistic") +
        dpt
        
      # High direction p value histogram
      qc_plots[[(i + 1) / 2]][[3]] = tmp_data %>% 
        ggplot(aes(p_high, after_stat(density))) +
        geom_histogram(color="black", bins=round(n_tests / 10)) +
        geom_hline(yintercept=1, color="red", size=2) +
        xlab("First level p values (high direction)") +
        ylab("Density") +
        dpt
      
      # Low direction HC plot
      qc_plots[[(i + 1) / 2]][[4]] = tmp_data %>%
        arrange(p_high) %>%
        head(length(hc_high)) %>%
        cbind(data.frame(index = 1 : length(hc_high), HC = hc_high)) %>%
        ggplot(aes(index, HC)) +
        geom_point() +
        geom_line() +
        geom_hline(yintercept=tmp$hc_crit, color="red", size=2) +
        xlab("Test index (sorted by high p-values)") +
        ylab("Higher Criticism statistic") +
        dpt

    }

  }

  return(list(second_level_results=second_level_results,
              qc_plots=qc_plots))

}


