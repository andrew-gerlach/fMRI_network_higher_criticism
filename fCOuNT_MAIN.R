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
#' @param nsim HC control parameter for number of simulations in p value calculation (optional, numeric)
#' @param results_plot flag to create circle plot of results (optional, boolean)
#' @param qc_plot flag to return HC plots (optional, boolean)
#' @param mcc option of multiple comparisons correction (string: fdr, bonferroni, none)
#' @param seed random seed for reproducibility

fCOuNT_MAIN = function(data, test_type, form, var, controls, net_def, net_def_col, fc, fc_col_name, fc_obj_name, k1, emp, nsim, qc_plot, results_plot, mcc, seed) {

# packages: tidyverse, stringr, tools, readxl, R.matlab
require(tidyverse)
require(stringr)
require(tools)
require(readxl)
require(R.matlab)
# option for parallel 
# option for plot

# Set seed if applied
if(!missing(seed)) { set.seed(seed) }

### Step 0 process input

# TODO: need more logic here to construct formula if var and controls supplied but not form
if(missing(var)) {
  # if variable of interest is not supplied, assume first in formula
  var = 1
  if(!is.null(form)) { warning("No variable of interest supplied, peforming inference on first variable in formula!") }
} else {
  if(!is.numeric(var)) {
    if(is.null(form) | test_type != "t.one") { stop("Must supply formula for variables to construct formula!") }
    tmp = str_replace_all(form, " ", "")
    tmp = str_split(tmp, "~", simplify=T)
    y = tmp[1]
    x = str_split(tmp[2], "\\+", simplify=T)
    if(y == "fc") {
      var = which(x == var)
    } else {
      var = which(x == "fc")
    }
  }
}
if(!is_formula(form) & !is.null(form)) { form = as.formula(form) }

# Higher Criticism options
if(missing(k1)) { k1 = 0.5 }
if(missing(emp)) { emp = T }
if(missing(nsim)) { nsim = 1E5 }
if(missing(qc_plot)) { qc_plot = T }
hc_opts = list(k1 = k1,
               emp = emp,
               nsim = nsim,
               plot = qc_plot)

# Multiple comparisons correction options
if(missing(mcc)) {
  mcc = "fdr"
} else {
  mcc = tolower(mcc)
  if(!(mcc %in% c("fdr", "bonferroni", "none"))) {
    stop("Invalid multiple comparisons correction option")
  }
}

if(missing(results_plot)) { results_plot = T }
if(results_plot)

# Read in data file if needed
if(!is.data.frame(data)) {
  if(is.character(data)) {
    if(file.exists(data)) {
      file_type = tolower(file_ext(data))
      if(file_type == "csv") {
        data = read.csv(data)
      } else if(file_type == ".xlsx" | file_type == ".xls") {
        data = read_excel(data)
      } else {
        stop("Data file of unrecognized type, please provide .csv or Excel filetype")
      }
    } else {
      stop("Data file does not seem to exist!")
    }
  } else {
    stop("Unrecognized format of data! Please supply a data frame/tibble or readable filename (full path)")
  }
}

### Step 1a load fc data into array if needed
if(missing(fc_obj_name)) {fc_obj_name = NULL }
if(missing(fc)) {
  if(missing(fc_col_name)) {
    stop("Must provide FC array or column name with paths to FC matrix files")
  } else {
    fc = fCOuNT_RETRIEVE_FC_MATRICES(data, fc_col_name, fc_obj_name)
  }
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

### Step 2 run first level tests (call to another routine)
# mclapply(RUN_1ST_LEVEL_TESTS, <inputs for RUN_1ST_LEVEL_TESTS>, <options for mclapply>)
# output table can be created here instead of in RUN_1ST_LEVEL_TESTS if that's easier
# ignore direction for now in output table
# need p for sure, would be great to get the test statistic as well (t in this case)
# ignore networks for now
first_level_results = XXX_RUN_1ST_LEVEL_TESTS(data, fc, test_type, form, var)

### Step 3 calculate network level HC statistics
tmp = fCOuNT_RUN_2ND_LEVEL_TESTS(first_level_results, net_def, hc_opts)
second_level_results = tmp$second_level_results
hc_plots = tmp$hc_plots

### Step 4 summarize results graphically
results_plot = fCOuNT_PLOT_RESULTS(second_level_results, net_def, mcc)

return(list(second_level_results=second_level_results,
            hc_plots=hc_plots))

}
