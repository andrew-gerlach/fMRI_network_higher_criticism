
fCOuNT_GEN_PLOT_OPTIONS = function(mcc, font_size, label_height) {

  # initialize
  plot_opts = list(mcc=NA,
                      font_size=NA,
                      label_height=NA)

  # set defaults for missing
  if(missing(mcc) | is.null(mcc)) {
    plot_opts$mcc = "fdr"
  } else {
    plot_opts$mcc = mcc
  }
  if(missing(font_size) | is.null(font_size)) {
    plot_opts$font_size = 1
  } else {
    plot_opts$font_size = font_size
  }
  if(missing(label_height) | is.null(label_height)) {
    plot_opts$label_height = 10
  } else {
    plot_opts$label_height = label_height
  }

  return(plot_opts)

}
