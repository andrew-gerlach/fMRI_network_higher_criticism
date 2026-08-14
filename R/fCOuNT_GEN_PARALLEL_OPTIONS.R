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
                       max_nodes=NULL)

  if(parallel) {
    plan(multisession)
    parallel_opts$max_nodes = nbrOfWorkers()
    if(is.null(nodes)) {
      # only use 75% of available nodes to (hopefully) avoid issues
      parallel_opts$nodes = max(c(floor(0.75 * parallel_opts$max_nodes), 1))
    } else {
      if(nodes > parallel_opts$max_nodes) {
        parallel_opts$nodes = max(min(c(floor(0.75 * parallel_opts$max_nodes), 1)), 1)
        warning(paste("The number of nodes specified appears to exceed the number available, reducing to", parallel_opts$nodes))
      }
      parallel_opts$nodes = nodes
    }
    plan(sequential)
  }

  return(parallel_opts)

}
