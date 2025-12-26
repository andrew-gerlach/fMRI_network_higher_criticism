
fCOuNT_GEN_HC_OPTIONS = function(k1, emp, nsim, first_level_plot) {

  # initialize
  hc_opts = list(k1=NA,
                 emp=NA,
                 nsim=NA,
                 first_level_plot=NA)
  
  # set defaults for missing
  if(missing(k1)) { plot_options$k1 = 0.5 } else { plot_options$k1 = k1 }
  if(missing(emp)) { plot_options$emp = T } else { plot_options$emp = emp }
  if(missing(nsim)) { plot_options$nsim = 1E5 } else { plot_options$nsim = nsim }
  if(missing(first_level_plot)) { plot_options$first_level_plot = T } else { plot_options$first_level_plot = first_level_plot }

  return(plot_options)

}
