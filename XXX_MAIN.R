#' Primary routine
#'
#' This is the primary routine to drive network inference on functional
#' connectomes using higher criticism

# Step 1 convert test information to a formula



# Step 2 run first level tests (call to another routine)
# mclapply(RUN_1ST_LEVEL_TESTS, <inputs for RUN_1ST_LEVEL_TESTS>, <options for mclapply>)
# output table can be created here instead of in RUN_1ST_LEVEL_TESTS if that's easier
# ignore direction for now in output table
# need p for sure, would be great to get the test statistic as well (t in this case)
# ignore networks for now



# Step 3 calculate network level HC statistics (call to another routine)
# RUN_2ND_LEVEL_TESTS()
# p is main input, don't set alpha, set k1=0.5, emp=F
