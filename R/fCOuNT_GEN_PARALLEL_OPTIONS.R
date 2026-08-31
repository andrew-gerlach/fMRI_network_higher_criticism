#' Generate list of higher criticism options
#'
#' @param parallel flag to use parallel calculations (boolean)
#' @param nodes number of nodes to use for parallel calculations (integer)
#' @param max_nodes maximum number of nodes available (integer)
#'
#' @return parallel_opts list of parallel options (parallel flag, number of nodes)
#'
#' @export

fCOuNT_GEN_PARALLEL_OPTIONS = function(parallel, nodes) {

  # initialize
  parallel_opts = list(parallel=parallel,
                       nodes=NULL,
                       sys=NULL)

  if(parallel) {
    if(is.null(nodes)) {
      parallel_opts$nodes = detectCores()
    } else {
      parallel_opts$nodes = nodes
    }
  }

  return(parallel_opts)

}
