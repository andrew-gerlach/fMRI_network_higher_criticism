#' Generate list of higher criticism options
#'
#' @param parallel flag to use parallel calculations (boolean)
#' @param nodes number of nodes to use for parallel calculations (integer)
#' @param systype operating system (string)
#'
#' @return parallel_opts list of parallel options (parallel flag, number of nodes)
#'
#' @export

fCOuNT_GEN_PARALLEL_OPTIONS = function(parallel, nodes) {

  # initialize
  parallel_opts = list(parallel=parallel,
                       nodes=NULL,
                       systype=NULL)

  if(parallel) {

    # Ideally, this wouldn't be needed, but the parallel package is mac-specific
    # and future.apply seems to break things
    parallel_opts$systype = .Platform$OS.type
    if(parallel_opts$systype != "unix") {
      parallel_opts$parallel = F
      warning("Parallel computing only available on Mac for now, running in serial")
    }

    if(is.null(nodes)) {
      parallel_opts$nodes = detectCores()
    } else {
      parallel_opts$nodes = nodes
    }

  }

  return(parallel_opts)

}
