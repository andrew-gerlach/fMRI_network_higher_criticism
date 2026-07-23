#' Primary routine for two-level testing
#'
#' This is the primary routine to drive network inference on functional
#' connectomes using higher criticism
#'
#' @param data data table containing subject level variables (data.frame, n rows)
#' @param test_type description of statistical test type (string)
#' @param custom_fun custom first level test definition (function)
#' @param form formula for statistical test (formula)
#' @param var_idx index of variable of interest in statistical test (integer)
#' @param net_def network definition for nodes (vector)
#' @param fc functional connectivity matrices (3D array, n x k x k)
#' @param results_plot flag to create circle plot of results (boolean)
#' @param plot_opts list of results plot control options
#' @param parallel_opts list of parallel computing control options
#' @param mcc option of multiple comparisons correction (string: fdr, bonferroni, none)
#' @param hc_opts list of higher criticism calculation options
#'
#' @return first_level_results table of first level test results for verification
#' @return second_level_results table of second level test results for primary inference
#' @return qc_plots list of quality control plots for verifying distribution of p-values and visualizing HC calculation (4 per network pair)
#' @return results_plots chord diagram summarizing second level test results
#'
#' @export

fCOuNT_MAIN = function(data, test_type, custom_fun, form, var_idx, net_def, fc, results_plot, plot_opts, parallel_opts, mcc, hc_opts) {

  # Step 1 run first level tests
  first_level_results = fCOuNT_RUN_1ST_LEVEL_TESTS(data, fc, test_type, form, var_idx, custom_fun)

  # Step 2 calculate network level HC statistics

  tmp = fCOuNT_RUN_2ND_LEVEL_TESTS(first_level_results, net_def, hc_opts, parallel_opts)
  second_level_results = tmp$second_level_results
  qc_plots = tmp$qc_plots

  # Step 3 summarize results graphically
  if(results_plot) {
    results_plots = fCOuNT_PLOT_RESULTS(second_level_results, plot_opts)
  } else {
    results_plots= NULL
  }

  return(list(first_level_results=first_level_results,
              second_level_results=second_level_results,
              qc_plots=qc_plots,
              results_plots=results_plots))

}
