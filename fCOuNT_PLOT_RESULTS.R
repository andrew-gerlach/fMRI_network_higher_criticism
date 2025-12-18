#' fCOuNT_PLOT_RESULTS
#'
#' @param second_level_results data frame containing first level test results with columns for
#' network1, network2, direction of test, HC value, and p value (M rows)
#' @param net_def vector defining which network each node belongs to (length k)
#' @param mcc flag for multiple comparisons option
#' @param font_size font size for network labels on plot
#' @param label_height height of label track on plot
#'
#' @returns 
#' @export results_plot circle plot of network
#'
#' @examples

fCOuNT_PLOT_RESULTS = function(second_level_results, net_def, mcc, font_size, label_height) {

  # set graphic defaults
  if(missing(font_size)) { font_size = 1 }
  if(missing(label_height)) { label_height = 10 }
  
  # pull network info from net_def
  networks = unique(net_def[!is.na(net_def)])
  m = length(networks)
  # number of unique network pairs
  M = m * (m + 1) / 2
  
  # LLD vs Healthy comparisons circle plot
  plot_mat = matrix(NA, m, m)
  exit = FALSE
  j = 1
  while(!exit) {
    
    # set significance threshold
    if(mcc == "none") {
      mcc_factor = 1 / 2
    } else {
      mcc_factor = j / (2 * M)
    }
    # track number of significant tests for FDR criterion
    j_old = j
    
    for(net1 in 1:m) {
      
      for(net2 in 1:m) {
        
        # TODO: add checks for significant results in both directions
        
        # Label low results with blue
        p = second_level_results %>%
          filter((network1 == networks[net1] & network2 == networks[net2]) |
                 (network1 == networks[net2] & network2 == networks[net1]),
                 direction == "low") %>%
          pull(p)
        
        if(is.na(plot_mat[net1, net2]) & p < (0.05 * mcc_factor)) {
          plot_mat[net1, net2] = "blue"
          plot_mat[net2, net1] = "blue"
          j = j + 1
        }
        
        p = second_level_results %>%
          filter((network1 == networks[net1] & network2 == networks[net2]) |
                 (network1 == networks[net2] & network2 == networks[net1]),
                 direction == "high") %>%
          pull(p)
        
        if(is.na(plot_mat[net1, net2]) & p < (0.05 * mcc_factor)) {
          plot_mat[net1, net2] = "red"
          plot_mat[net2, net1] = "red"
          j = j + 1
        }
        
      }
      
    }
    
    # FWE correction with Bonferroni
    if(mcc == "bonferroni" | mcc == "none") { exit = TRUE }
    
    # FDR correction with Benjamini-Hochberg
    if(j == j_old) { exit = TRUE }
    
  }
  
  rownames(plot_mat) = networks
  colnames(plot_mat) = networks
  
  plot_data = data.frame(from = rep(rownames(plot_mat), times = ncol(plot_mat)),
                         to = rep(colnames(plot_mat), each = nrow(plot_mat)),
                         color = as.vector(plot_mat),
                         value = rep(1, m ^ 2),
                         stringsAsFactors = FALSE)
  for(i in seq(m, 1, -1)) { plot_data = plot_data[-((m * i + 1) : ((m + 1) * i)), ] }
  chordDiagram(plot_data,
               col=plot_data$color,
               grid.col=rep("grey", m),
               annotationTrack=c("grid"),
               annotationTrackHeight=mm_h(label_height))
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim),
                mean(ylim),
                si,
                sector.index=si,
                track.index=1,
                facing="bending.inside",
                niceFacing=T,
                col="black",
                cex=font_size)
    
  }
  
}


