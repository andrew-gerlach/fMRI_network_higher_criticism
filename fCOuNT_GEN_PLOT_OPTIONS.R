
fCOuNT_GEN_PLOT_OPTIONS = function(mcc, font_size, label_height) {

  plot_options = list(mcc=NA,
                      font_size=NA,
                      label_height=NA)
  # set defaults for missing
  if(missing(mcc)) { plot_options$mcc = "fdr" } else { plot_options$mcc = mcc }
  if(missing(font_size)) { plot_options$mcc = 1 } else { plot_options$mcc = mcc }
  if(missing(label_height)) { plot_options$label_height = 10 } else { plot_options$label_height = label_height }

  return(plot_options)

}
