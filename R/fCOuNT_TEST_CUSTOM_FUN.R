#' Function to test compatibility of custom_fun for 1st level tests
#'
#' This routine ensures the custom 1st level test function has the correct input
#' and output
#'
#' @param custom_fun function for performing 1st level tests
#' @param data subject information/subject number (data.frame, n rows)
#' @param form formula for fist level test (string)
#' @param var_idx variable of interest (string)
#'
#' @export

fCOuNT_TEST_CUSTOM_FUN = function(custom_fun, data, form, var_idx) {

  if(!is.function(custom_fun)) {
    stop("The argument supplied for custom_fun does not appear to be a function")
  }

  # test for proper input
  if(!all(c("fc_vec", "data", "form", "var_idx") %in% names(formals(custom_fun)))) {
    stop("The custom test function does not contain the required inputs (fc_vec, data, form, var_idx)")
  }
  if(length(names(formals(custom_fun))) > 4) {
    warning("Extra inputs detected in custom_fun cannot be used")
  }

  # test output on random vector
  tmp = custom_fun(rnorm(nrow(data)), data, form, var_idx)
  if(!all(c("test_statistic", "p_low", "p_high") %in% names(tmp))) {
    stop("Custom test function does not return the required information (test_statistic, p_low, and p_high)")
  }

}
