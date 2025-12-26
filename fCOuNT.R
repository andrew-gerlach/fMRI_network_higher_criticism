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

fCOuNT_MAIN = function(data, test_type, form, var, controls, net_def, net_def_col, fc, fc_col_name, fc_obj_name, k1, emp, nsim, qc_plot, results_plot, mcc, seed) {

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
  if(!(test_type %in% c("t.one", "t.two", "anova", "regression"))) {
    stop(paste("Test type", test_type, "is not currently supported"))
  }
  
  # Formula handling
  if(missing(form)) { form = NULL }
  if(missing(var)) { var = NULL }
  if(missing(controls)) { controls = NULL }
  form = fCOuNT_GEN_FORMULA(data, test_type, form, var, controls)
 
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
  if(missing(hc_opts)) { hc_opts = fCOuNT_GEN_HC_OPTIONS() }
  
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
    if(missing(plot_opts)) { plot_opts = fCOuNT_GEN_PLOT_OPTIONS() }
  }
 
  ### Step 1b load network definitions if needed
  if(!is.vector(net_def) | length(net_def) == 1) {
    
    # read in file name is provided
    if(is.character(net_def)){
      if(file.exists(net_def)) {
        file_type = tolower(file_ext(net_def))
        if(file_type == "csv") {
          net_def = read.csv(net_def)
        } else if(file_type == ".xlsx" | file_type == ".xls") {
          net_def = read_excel(net_def)
        } else {
          stop("Network definition file of unrecognized type, please provide .csv or Excel filetype")
        }
      } else {
        stop("Network definition file does not seem to exist!")
      }
    }
    
    # pull network definition column if needed
    if(is.data.frame(net_def)) {
      if(missing(net_def_col)) {
        stop("Network definition column name needed to read in network definition from table")
      } else {
        net_def = net_def %>% pull(net_def_col)
      }
    } else {
      stop("Unrecognized format of network definition! Please supply a data frame/tibble or readable filename (full path)")
    }
    
  }
  
  return(list(second_level_results=second_level_results,
              hc_plots=hc_plots))
  
}
