#' Function for loading required packages
#'
#' @export

fCOuNT_LOAD_PACKAGES = function() {

  # packages: tidyverse, stringr, rlang, tools, readxl, R.matlab
  packages = c("tidyverse", "stringr", "rlang", "tools", "readxl", "R.matlab", "xfun", "parallel", "circlize", "lmerTest")
  for(p in packages) {
    if(!require(p, character.only=T)) {
      install.packages(p)
    }
    library(p, character.only=T)
  }

}
