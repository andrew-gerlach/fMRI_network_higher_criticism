################################################################################
# XXX_HIGHER_CRITICISM calculates the higher criticism statistic on a set of p values
# In:  p - p-values to be highly criticized
#      k1 - fraction or number of p values to keep
#      emp - flag for using variance of empirical distribution
#      plot - flag to plot summary items
# Out: hc - maximum HC statistic
################################################################################

# TODO: add option for not excluding low p values

XXX_HIGHER_CRITICISM = function(p, k1, emp, plot) {

  # Assume variance of theoretical null
  if(missing(emp)) { emp = FALSE }
  if(missing(plot)) { plot = FALSE }

  # Sort p values in ascending order
  n = length(p)
  p_sorted = sort(p)

  # convert k1 to number of tests if given as fraction
  if(k1 <= 1) { k1 = floor(k1 * length(p)) }
  p_sorted = p_sorted[1:k1]

  # Typical case
  i_vals = 1 : k1

  # Calculate higher criticism
  # page 966 of Donoho2004 (unnumbered equation for HC^*_n)
  hc = rep(NA, k1)
  if(emp) {
    for (i in i_vals) {
      hc[i] = sqrt(n) * (i / n - p_sorted[i]) /
        sqrt(p_sorted[i] * (1 - p_sorted[i]))
    }
  } else {
    for (i in i_vals) {
      hc[i] = sqrt(n) * (i / n - p_sorted[i]) /
        sqrt(i / n * (1 - i / n))
    }
  }

  # Remove points below Bonferroni correction
  hc[p_sorted < (0.05 / n)] = 0

  # Plot HC
  if(plot) {
    hc_plot = data.frame(index=(i_vals / n), hc=hc) %>%
      ggplot(aes(index, hc)) +
      geom_point(color="blue") +
      xlab("Fraction of Tests (ordered by p-value)") +
      ylab("HC Statistic") +
      theme(strip.background=element_blank(),
            panel.background=element_blank(),
            axis.line=element_line(),
            axis.ticks=element_blank())
  }

  if(plot) {
    return(list(hc=max(hc), plot=hc_plot))
  } else {
    return(max(hc))
  }

}

