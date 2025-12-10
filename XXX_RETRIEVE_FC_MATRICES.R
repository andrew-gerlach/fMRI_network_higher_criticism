#' Routine to retrieve FC matrices from file names and pull into array
#'
#' @param data data frame containing subject level variables (n rows)
#' @param fc_col_name column name in data with subject level FC files (string)
#' @param fc_obj_name name of FC matrix object in storage structure (string)

XXX_RETRIEVE_FC_MATRICES = function(data, fc_col_name, fc_obj_name) {
  
  # number of subjects
  n = nrow(data)
  
  # read in first matrix for data gathering
  fc_fn = data[, fc_col_name] %>% first(na_rm=T)
  if(file.exists(fc_fn)) {
    fc_fn_type = tolower(file_ext(fc_fn))
    if(fc_fn_type == "csv") {
      fc_mat = read.csv(fc_fn)
    } else if(fc_fn_type == "rds") {
      fc_mat = readRDS(fc_fn)
    } else if(fc_fn_type == "rdata" | fc_fn_type == "rda") {
      fc_mat = load(fc_fun)[[fc_obj_name]]
    } else if(fc_fn_type == "mat") {
      # for some reason underscores are converted to periods for Matlab objects
      fc_obj_name = str_replace_all(fc_obj_name, "_", ".")
      fc_mat = readMat(fc_fn)[[fc_obj_name]]
    } else {
      # TODO: Add support for Python
      stop("The FC file type is not supported, please provide a .csv, .rds, .rdata, .rda, or .mat file")
    }
  }
  
  # extract size and basic checks
  if(length(dim(fc_mat)) != 2) { stop("The FC matrix is not 2D!") }
  k = dim(fc_mat)[1]
  if(k != dim(fc_mat)[2]) { stop("The FC matrix is not square!") }
  
  # initialize FC storage array
  fc = array(NA, c(n, k, k))
  fc[1, , ] = fc_mat
  rm(fc_mat)
  
  for(subj in 2 : n) {
    
    fc_fn = data[subj, fc_col_name]
    
    # check if file exists
    if(!file.exists(fc_fn)) {
      warning(sprintf("FC matrix not found: %s", fc_fn))
      next
    }
    
    # read in FC file
    if(fc_fn_type == "csv") {
      fc_mat = read.csv(fc_fn)
    } else if(fc_fn_type == "rds") {
      fc_mat = readRDS(fc_fn)
    } else if(fc_fn_type == "rdata" | fc_fn_type == "rda") {
      if(missing(fc_obj_name) | is.null(fc_obj_name)) {
        stop("The FC matrix object (variable) name is required for .rdata/.rda files!")
      }
      fc_mat = load(fc_fun)[[fc_obj_name]]
    } else if(fc_fn_type == "mat") {
      if(missing(fc_obj_name) | is.null(fc_obj_name)) {
        stop("The FC matrix object (variable) name is required for .mat files!")
      }
      fc_mat = readMat(fc_fn)[[fc_obj_name]]
    } else {
      # TODO: Add support for Python
      stop("The FC file type is not supported, please provide a .csv, .rds, .rdata, .rda, or .mat file")
    }
    fc[subj, , ] = fc_mat
    rm(fc_mat)
    
  }
  
  return(fc)
  
}