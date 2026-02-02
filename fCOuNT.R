#' Primary routine
#'
#' This is the primary routine to drive network inference on functional
#' connectomes using higher criticism
#'
#' @param data data table containing subject level variables (string, data.frame, n rows)
#' @param test_type description of statistical test type (string)
#' @param form formula for statistical test (string)
#' @param var variable of interest (string)
#' @param controls control variables (strings)
#' @param net_def network definition for nodes (string, data.frame)
#' @param net_def_col name of column containing network definition (optional, string)
#' @param fc functional connectivity matrices (optional, 3D array, n x k x k)
#' @param fc_col_name column name in data with subject level FC files (optional, string)
#' @param fc_obj_name name of FC matrix object in storage structure (optional, string)
#' @param k1 HC control parameter for fractional cutoff (optional, numeric in (0, 1))
#' @param emp HC control parameter for using empirical variance (optional, boolean)
#' @param qc_plot flag to return HC plots (optional, boolean)
#' @param nsim HC control parameter for number of simulations in p value calculation (optional, numeric)
#' @param results_plot flag to create circle plot of results (optional, boolean)
#' @param mcc option of multiple comparisons correction (optional, string: fdr, bonferroni, none)
#' @param font_size font size for network labels on plot (optional, numeric)
#' @param label_height height of label track on plot (optional, numeric)
#' @param seed random seed for reproducibility (optional, numeric)

fCOuNT = function(data, test_type, form, var, controls, net_def, net_def_col, fc, fc_col_name, fc_obj_name, k1, emp, nsim, qc_plot, results_plot, mcc, font_size, label_height, seed) {

  # packages: tidyverse, stringr, rlang, tools, readxl, R.matlab
  require(tidyverse)
  require(stringr)
  require(rlang)
  require(tools)
  require(readxl)
  require(R.matlab)
  # option for parallel 
  # option for plot

  # Set seed if applied
  if(!missing(seed)) { set.seed(seed) }

  # Read in data file if needed
  data = fCOuNT_READ_DATA(data)
  
  # Check that test type is supported
  test_type = tolower(test_type)
  if(!(test_type %in% c("t.one", "t.two", "anova", "regression", "ancova"))) {
    stop(paste("Test type", test_type, "is not currently supported"))
  }
  
  # Formula handling
  if(missing(form)) { form = NULL }
  if(missing(var)) { var = NULL }
  if(missing(controls)) { controls = NULL }
  tmp = fCOuNT_GEN_FORMULA(data, test_type, form, var, controls)
  form = tmp$form
  var_idx = tmp$var_idx
 
  ### Step 1a load fc data into array if needed
  if(missing(fc_obj_name)) {fc_obj_name = NULL }
  if(missing(fc)) {
    if(missing(fc_col_name)) {
      stop("Must provide FC array or column name with paths to FC matrix files")
    } else {
      fc = fCOuNT_RETRIEVE_FC_MATRICES(data, fc_col_name, fc_obj_name)
    }
  }
  
  # Higher Criticism options
  if(missing(k1)) { k1 = NULL }
  if(missing(emp)) { emp = NULL }
  if(missing(nsim)) { nsim = NULL }
  if(missing(qc_plot)) { qc_plot = NULL }
  hc_opts = fCOuNT_GEN_HC_OPTIONS(k1, emp, nsim, qc_plot)
  
  # Multiple comparisons correction options
  if(missing(mcc)) {
    mcc = "fdr"
  } else {
    mcc = tolower(mcc)
    if(!(mcc %in% c("fdr", "bonferroni", "none"))) {
      stop("Invalid multiple comparisons correction option")
    }
  }
  
  # Plot options
  if(missing(results_plot)) { results_plot = T }
  if(results_plot) {
    if(missing(font_size)) { font_size = NULL }
    if(missing(label_height)) { label_height = NULL }
    plot_opts = fCOuNT_GEN_PLOT_OPTIONS(mcc, font_size, label_height)
  }
 
  # Load network definitions
  net_def = fCOuNT_RETRIEVE_NET_DEF(net_def, net_def_col)
  
  # Call main driver routine
  tmp = fCOuNT_MAIN(data, test_type, form, var_idx, net_def, fc, qc_plot, results_plot, plot_opts, mcc, hc_opts)
  
  return(list(second_level_results=tmp$second_level_results,
              qc_plots=tmp$qc_plots,
              results_plots=tmp$results_plots))
  
}
