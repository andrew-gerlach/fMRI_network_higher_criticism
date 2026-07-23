#' Interface routine to process and prepare inputs
#'
#' This routine processes inputs and prepares for the primary calculation in
#' fCOuNT_MAIN
#'
#' @param data data table containing subject level variables (string, data.frame, n rows)
#' @param test_type description of statistical test type (string)
#' @param custom_fun custom first level test definition (function)
#' @param form formula for statistical test (string)
#' @param var variable of interest (string)
#' @param net_def network definition for nodes (string, data.frame)
#' @param net_def_col name of column containing network definition (optional, string)
#' @param fc functional connectivity matrices (optional, 3D array, n x k x k)
#' @param fc_col_name column name in data with subject level FC files (optional, string)
#' @param fc_obj_name name of FC matrix object in storage structure (optional, string)
#' @param mcc option of multiple comparisons correction (optional, string: fdr, bonferroni, none)
#' @param parallel flag to use parallel calculations (optional, boolean, default true)
#' @param nodes number of nodes to use for parallel (optional, integer, default max available)
#' @param mcc option of multiple comparisons correction (optional, string: fdr, bonferroni, none)
#' @param k1 HC control parameter for fractional cutoff (optional, numeric in (0, 1))
#' @param emp HC control parameter for using empirical variance (optional, boolean)
#' @param nsim HC control parameter for number of simulations in p value calculation (optional, numeric)
#' @param results_plot flag to create circle plot of results (optional, boolean)
#' @param font_size font size for network labels on plot (optional, numeric)
#' @param label_height height of label track on plot (optional, numeric)
#' @param seed random seed for reproducibility (optional, numeric)
#'
#' @return first_level_results table of first level test results for verification
#' @return second_level_results table of second level test results for primary inference
#' @return qc_plots list of quality control plots for verifying distribution of p-values and visualizing HC calculation (4 per network pair)
#' @return results_plots chord diagram summarizing second level test results
#'
#' @export

fCOuNT = function(data,
                  test_type,
                  form = NULL,
                  var = NULL,
                  net_def,
                  net_def_col = NULL,
                  fc,
                  fc_col_name,
                  fc_obj_name = NULL,
                  mcc = "fdr",
                  parallel = T,
                  nodes = NULL,
                  k1 = NULL,
                  emp = NULL,
                  nsim = NULL,
                  results_plot,
                  font_size = NULL,
                  label_height = NULL,
                  seed) {

  # Load required packages
  fCOuNT_LOAD_PACKAGES()

  # Set seed if applied
  # TODO: seems like the results are still stochastic, related to parallelization maybe??
  if(!missing(seed)) { set.seed(seed) }

  # Set parallel options
  parallel_opts = fCOuNT_GEN_PARALLEL_OPTIONS(parallel, nodes)

  # Read in data file if needed
  data = fCOuNT_READ_DATA(data)

  # Check that test type is supported
  test_type = tolower(test_type)
  if(!(test_type %in% c("t.one", "t.two", "anova", "lr", "custom"))) {
    stop(paste("Test type", test_type, "is not currently supported"))
  }
  if(test_type == "custom") {
    if(missing(custom_fun)) {
      stop("Must supply custom_fun for 1st level test definitions for test_type custom")
    } else {
      fCOuNT_TEST_CUSTOM_FUN(custom_fun, data, form, var_idx)
    }
  } else {
    custom_fun = NULL
  }

  # Formula handling
  tmp = fCOuNT_GEN_FORMULA(data, test_type, form, var)
  form = tmp$form
  var_idx = tmp$var_idx
  # TODO: add ability to define reference level for groups for clearer directionality

  ### Step 1a load fc data into array if needed
  if(missing(fc)) {
    if(missing(fc_col_name)) {
      stop("Must provide FC array or column name with paths to FC matrix files")
    } else {
      fc = fCOuNT_RETRIEVE_FC_MATRICES(data, fc_col_name, fc_obj_name)
    }
  }

  # Higher Criticism options
  hc_opts = fCOuNT_GEN_HC_OPTIONS(k1, emp, nsim)

  # Multiple comparisons correction options
  mcc = tolower(mcc)
  if(!(mcc %in% c("fdr", "bonferroni", "none"))) {
    stop("Invalid multiple comparisons correction option")
  }

  # Plot options
  if(missing(results_plot)) { results_plot = T }
  if(results_plot) {
    plot_opts = fCOuNT_GEN_PLOT_OPTIONS(mcc, font_size, label_height)
  }

  # Load network definitions
  net_def = fCOuNT_RETRIEVE_NET_DEF(net_def, net_def_col)

  # Call main driver routine
  tmp = fCOuNT_MAIN(data, test_type, custom_fun, form, var_idx, net_def, fc, results_plot, plot_opts, parallel_opts, mcc, hc_opts)

  return(list(first_level_results=tmp$first_level_results,
              second_level_results=tmp$second_level_results,
              qc_plots=tmp$qc_plots,
              results_plots=tmp$results_plots))

}
