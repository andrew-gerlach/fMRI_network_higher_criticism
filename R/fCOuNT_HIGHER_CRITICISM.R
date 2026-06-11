#' calculates the higher criticism statistic on a set of p values
#'
#' @param p p-values from mass univariate tests (numeric vector)
#' @param k1 fraction or number of p values to keep (numeric)
#' @param emp flag for using variance of empirical distribution (boolean)
#'
#' @return hc higher criticism value for observed p values (numeric)
#'
#' @export

# TODO: add option for not excluding low p values

fCOuNT_HIGHER_CRITICISM = function(p, k1, emp) {

  # Removes NAs and sort p values in ascending order
  p = p[!is.na(p)]
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

  return(hc)

}

