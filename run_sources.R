# packages: tidyverse, stringr, rlang, tools, readxl, R.matlab
packages = c("tidyverse", "stringr", "rlang", "tools", "readxl", "R.matlab", "xfun", "parallel", "circlize", "lmerTest")
for(p in packages) {
  if(!require(p, character.only=T)) {
    install.packages(p)
  }
  library(p, character.only=T)
}

source('~/Fellowship/projects/hc/fCOuNT/fCOuNT.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_MAIN.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_READ_DATA.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_GEN_FORMULA.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_GEN_PLOT_OPTIONS.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_GEN_HC_OPTIONS.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_GEN_PARALLEL_OPTIONS.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RETRIEVE_FC_MATRICES.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RETRIEVE_NET_DEF.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RUN_1ST_LEVEL_TESTS.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RUN_1ST_LEVEL_TESTS_mac.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RUN_1ST_LEVEL_TESTS_windows.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_RUN_2ND_LEVEL_TESTS.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_HIGHER_CRITICISM.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_CALC_HC_P_VALUE.R')
source('~/Fellowship/projects/hc/fCOuNT/fCOuNT_PLOT_RESULTS.R')
source("~/Fellowship/projects/hc/fCOuNT/testing/fCOuNT_GEN_TEST_DATA.R")
source("~/Fellowship/projects/hc/fCOuNT/testing/fCOuNT_GEN_MEM_TEST_DATA.R")
