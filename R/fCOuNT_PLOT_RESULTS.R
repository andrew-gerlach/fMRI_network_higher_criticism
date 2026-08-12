#' fCOuNT_PLOT_RESULTS
#'
#' @param second_level_results data frame containing first level test results with columns for
#' network1, network2, direction of test, HC value, and p value (M rows)
#' @param plot_opts list containing mcc option, font size for circle graph, and height for label row
#'
#' @return results_plot chord diagram of second level results
#'
#' @export

fCOuNT_PLOT_RESULTS = function(second_level_results, plot_opts) {

  require(circlize)

  # pull network info from net_def
  networks = unique(c(second_level_results$network1, second_level_results$network2))
  networks = networks[!is.na(networks)]
  m = length(networks)
  # number of unique network pairs
  M = m * (m + 1) / 2

  # LLD vs Healthy comparisons circle plot
  plot_mat = matrix(NA, m, m)
  exit = FALSE
  j = 1
  while(!exit) {

    # set significance threshold
    if(plot_opts$mcc == "none") {
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

        # Label high results with red
        p = second_level_results %>%
          filter((network1 == networks[net1] & network2 == networks[net2]) |
                 (network1 == networks[net2] & network2 == networks[net1]),
                 direction == "high") %>%
          pull(p)

        # Skip if there are no results for the high direction (e.g., anova)
        if(is.na(p)) { next }

        if(is.na(plot_mat[net1, net2]) & p < (0.05 * mcc_factor)) {
          plot_mat[net1, net2] = "red"
          plot_mat[net2, net1] = "red"
          j = j + 1
        }

        # Label mixed results with purple
        if(plot_mat[net1, net2] == "blue" & p < (0.05 * mcc_factor)) {
          plot_mat[net1, net2] = "purple"
          plot_mat[net2, net1] = "purple"
          j = j + 1
        }

      }

    }

    # FWE correction with Bonferroni
    if(plot_opts$mcc == "bonferroni" | plot_opts$mcc == "none") { exit = TRUE }

    # FDR correction with Benjamini-Hochberg
    if(j == j_old) { exit = TRUE }

  }

  # If direction is not present, plot with purple
  if(all(is.na(second_level_results$p_high[second_level_results$direction == "high"]))) {
    plot_mat[plot_mat == "blue"] = "purple"
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
               annotationTrackHeight=mm_h(plot_opts$label_height))
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
                cex=plot_opts$font_size)

  }

}


