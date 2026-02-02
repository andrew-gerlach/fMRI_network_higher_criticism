#' Primary routine
#'
#' This is the primary routine to drive network inference on functional
#' connectomes using higher criticism
#'
#' @param data data table containing subject level variables (data.frame, n rows)
#' @param test_type description of statistical test type (string)
#' @param form formula for statistical test (formula)
#' @param net_def network definition for nodes (vector)
#' @param fc functional connectivity matrices (3D array, n x k x k)
#' @param qc_plot flag to return HC plots (optional, boolean)
#' @param results_plot flag to create circle plot of results (boolean)
#' @param plot_opts list of results plot control options (optional)
#' @param mcc option of multiple comparisons correction (string: fdr, bonferroni, none)

fCOuNT_MAIN = function(data, test_type, form, var_idx, net_def, fc, qc_plot, results_plot, plot_opts, mcc, hc_opts) {

  # Step 1 run first level tests (call to another routine)
  first_level_results = fCOuNT_RUN_1ST_LEVEL_TESTS(data, fc, test_type, form, var_idx)
  
  # Step 2 calculate network level HC statistics
  
  tmp = fCOuNT_RUN_2ND_LEVEL_TESTS(first_level_results, net_def, hc_opts)
  second_level_results = tmp$second_level_results
  qc_plots = tmp$qc_plots
  
  # Step 3 summarize results graphically
  if(results_plot) {
    results_plots = fCOuNT_PLOT_RESULTS(second_level_results, net_def, mcc)
  } else {
    results_plots= NULL
  }
  
  return(list(second_level_results=second_level_results,
              qc_plots=qc_plots,
              results_plots=results_plots))

}
