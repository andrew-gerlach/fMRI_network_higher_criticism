
fCOuNT_GEN_PARALLEL_OPTIONS = function(parallel, nodes) {

  # initialize
  parallel_opts = list(parallel=parallel,
                       nodes=NA,
                       systype=NA)

  # Determine system type for pointing to appropriate parallel algorithms
  # (R is annoying and uses different methods )
  parallel_opts$systype = .Platform$OS.type
  if(!(parallel_opts$systype %in% c("windows", "unix"))) {
    warning("Unrecognized operating system, running in serial")
    parallel_opts$parallel = F
  }

  if(parallel & !is.null(nodes)) {
    parallel_opts$nodes = nodes
  }

  return(parallel_opts)

}
