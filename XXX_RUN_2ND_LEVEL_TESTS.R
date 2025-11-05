<<<<<<< HEAD
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
  if(length(unique(c(first_level_results$node1, first_level_results$node2))) != length(net_def)) {
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
    
      i = i + 1
      # Fill in table network definitions
      second_level_results$network1[i] = networks[m1]
      second_level_results$network2[i] = networks[m2]
      # Extract relevant p values
      p = first_level_results %>%
        filter((network1 == networks[m1] & network2 == networks[m2]) | (network1 == networks[m2] & network2 == networks[m1])) %>%
        pull(p)
      # Calculate number of tests
      second_level_results$n_tests[i] = length(p)
      # Calculate HC statistic
      second_level_results$p[i] = XXX_HIGHER_CRITICISM(p, k1=0.5, emp=F)
    
    }
  
  }
  
  return(second_level_results)
  
=======
################################################################################
# calc_HC calculates the higher criticism statistic on a set of p values
# In:  p - p-values to be highly criticized
#      alpha - absolute cutoff for p values considered for HC
#      k1 - fraction or number of p values to keep
#      emp - flag for using variance of empirical distribution
#      plot - flag to plot summary items
# Out: hc - maximum HC statistic
################################################################################

XXX_RUN_2ND_LEVEL_TESTS = function(p, alpha, k1, emp, plot, debug) {

    if(missing(debug)) { debug = FALSE }
    # Sort p values in ascending order
    n = length(p)
    p.sorted = sort(p)
    # Determine cutoff
    if(missing(k1)) {
        warning("k1 missing from calc_HC")
        k1 = length(p)
        if(missing(alpha)) {
            alpha = 1
        } else {
            k1 = max(which(p.sorted < alpha))
        }
    } else {
        if(missing(alpha)) { alpha = 1 }
        if(k1 <= 1) { k1 = floor(k1 * length(p)) }
    }
    p.sorted = p.sorted[1:k1]

    if(debug) { cat(sprintf("alpha %0.2f k1 %i emp %s\n", alpha, k1, emp)) }
    # Assume variance of theoretical null
    if(missing(emp)) { emp = FALSE }
    if(missing(plot)) { plot = FALSE }

    if(min(p.sorted) < alpha) {

        # Typical case
        i.vals = 1:k1

        # Calculate higher criticism
        # page 966 of Donoho2004 (unnumbered equation for HC^*_n)
        hc1 = rep(NA, k1)
        for (i in i.vals) {
            hc1[i] = sqrt(n) * (i / n - p.sorted[i]) /
                sqrt(p.sorted[i] * (1 - p.sorted[i])) }
        # eq (1) of Donoho2018
        hc2 = rep(NA, k1)
        for (i in i.vals) {
            hc2[i] = sqrt(n) * (i / n - p.sorted[i]) /
                sqrt(i / n * (1 - i / n)) }
        # select theoretical or empirical results
        if(emp) { hc = hc1 } else { hc = hc2 }

        # Remove points below Bonferroni correction
        hc[p.sorted < (0.05 / n)] = 0
        # Summarize results
        p.max = p.sorted[which.max(hc)]

        # Plot HC
        if(plot) {
            dev.new()
            plot(p.sorted, hc,
                pch='.', col='blue', cex=2,
                xlim=c(0,alpha),
                xlab='Fraction of Tests (ordered by p-value)',
                ylab='HC Statistic')
            lines(c(0,alpha), c(2,2), col='red', lwd=3) }

    } else {

        # No p values below cutoff
        hc = 0
        p.max = 0

    }

    return(max(hc))

>>>>>>> branch1/Arvind
}


