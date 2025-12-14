################################################################################
# calc_HC calculates the higher criticism statistic on a set of p values
# In:  p - p-values to be highly criticized
#      alpha - absolute cutoff for p values considered for HC
#      k1 - fraction or number of p values to keep
#      emp - flag for using variance of empirical distribution
#      plot - flag to plot summary items
# Out: hc - maximum HC statistic
################################################################################

XXX_HIGHER_CRITICISM = function(p, alpha, k1, emp, plot, debug) {

  if(missing(debug)) { debug = FALSE }

  # Assume variance of theoretical null
  if(missing(emp)) { emp = FALSE }
  if(missing(plot)) { plot = FALSE }

  # Sort p values in ascending order
  n = length(p)
  p_sorted = sort(p)

  # Determine cutoff
  if(missing(k1)) {
    warning("k1 missing from calc_HC")
    k1 = length(p)
    if(missing(alpha)) {
      alpha = 1
    } else {
      k1 = max(which(p_sorted < alpha))
    }
  } else {
    if(missing(alpha)) { alpha = 1 }
    if(k1 <= 1) { k1 = floor(k1 * length(p)) }
  }
  p_sorted = p_sorted[1:k1]

  if(debug) { cat(sprintf("alpha %0.2f k1 %i emp %s\n", alpha, k1, emp)) }

  if(min(p_sorted) < alpha) {

    # Typical case
    i_vals = 1:k1

    # Calculate higher criticism
    # page 966 of Donoho2004 (unnumbered equation for HC^*_n)
    hc1 = rep(NA, k1)
    for (i in i_vals) {
      hc1[i] = sqrt(n) * (i / n - p_sorted[i]) /
        sqrt(p_sorted[i] * (1 - p_sorted[i])) }
    # eq (1) of Donoho2018
    hc2 = rep(NA, k1)
    for (i in i_vals) {
      hc2[i] = sqrt(n) * (i / n - p_sorted[i]) /
        sqrt(i / n * (1 - i / n)) }
    # select theoretical or empirical results
    if(emp) { hc = hc1 } else { hc = hc2 }

    # Remove points below Bonferroni correction
    hc[p_sorted < (0.05 / n)] = 0

    # Plot HC
    # TODO: update this!!!
    if(plot) {
      plot = plot(p_sorted, hc,
                  pch='.', col='blue', cex=2,
                  xlim=c(0,alpha),
                  xlab='Fraction of Tests (ordered by p-value)',
                  ylab='HC Statistic')
      lines(c(0,alpha), c(2,2), col='red', lwd=3) }

  } else {

    # No p values below cutoff
    hc = 0
    plot = NULL

  }

  return(list(hc=max(hc), plot=plot))

}

