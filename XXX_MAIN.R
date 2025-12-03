#' Primary routine
#'
#' This is the primary routine to drive network inference on functional
#' connectomes using higher criticism
#'
#' @param data data frame containing subject level variables (n rows)
#' @param fc functional connectivity matrices (3D array, n x k x k)
#' @param fc_col_name column name in data with subject level FC files (string)
#' @param fc_obj_name name of FC matrix object in storage structure (string)
#' @param alpha HC control parameter (optional, numeric in (0, 1))
#' @param k1 HC control parameter (optional, numeric in (0, 1))
#' @param emp HC control parameter (optional, boolean)
#' @param plot flag to return HC plots (optional, boolean)

# packages: tidyverse, tools, readxl, R.matlab

### Step 0 process input

# Higher Criticism options
if(missing(alpha)) { alpha = NULL }
if(missing(k1)) { if(is.null(alpha)) { k1 = 0.5 } else { k1 = NULL } }
if(missing(emp)) { emp = T }
if(missing(plot)) { plot = T }
hc_opts = list(alpha = alpha,
               k1 = k1,
               emp = emp,
               plot = plot)

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

### Step 1a convert test information to a formula

### Step 1b load fc data into array if needed
if(is.missing(fc_obj_name)) {fc_obj_name = NULL }
if(is.null(fc)) { fc = XXX_RETRIEVE_FC_MATRICES(data, fc_col_name, fc_obj_name) }

### Step 2 run first level tests (call to another routine)
# mclapply(RUN_1ST_LEVEL_TESTS, <inputs for RUN_1ST_LEVEL_TESTS>, <options for mclapply>)
# output table can be created here instead of in RUN_1ST_LEVEL_TESTS if that's easier
# ignore direction for now in output table
# need p for sure, would be great to get the test statistic as well (t in this case)
# ignore networks for now
first_level_results = XXX_RUN_1ST_LEVEL_TESTS(data, fc)


### Step 3 calculate network level HC statistics
tmp = XXX_RUN_2ND_LEVEL_TESTS(first_level_results, net_def, hc_options)
second_level_results = tmp$second_level_results
hc_plots = tmp$hc_plots
