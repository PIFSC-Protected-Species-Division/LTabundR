#' Plot Rg(0) curves
#'
#' This function produces a multi-pane plot of changes in
#' relative trackline detection probabilities, *Rg(0)*,
#' across Beaufort sea states for a set of species.
#'
#' @param Rg0 A `data.frame` of Rg(0) estimates for a set of species groups,
#' as produced by the `LTabundR` function `g0_table()`. See its documentation for details.
#'
#' @param panes The number of plot panes to produce. Species will be assigned to each
#' pane according to their minimum Rg(0) estimate, such that species with similar
#' detectability will be grouped together.
#'
#' @param legend_key Height of each legend key, in cm, to allow for fine-tuning the formatting of the plot.
#'
#' @param legend_font Legend font size, to allow for fine-tuning the formatting of the plot.
#'
#' @return A `ggplot` object with faceted panes arranged vertically.
#' @export
#' @import ggplot2
#' @import dplyr
#'
g0_plot <- function(Rg0,
                    panes = 3,
                    legend_key = 0.4,
                    legend_font = 7){

if(FALSE){ # debugging only === not run ========================================
  data(g0_results)
  Rg0 <- g0_results
  #data(barlow_2015)
  #Rg0 <- barlow_2015
  panes = 3
  legend_key = 0.4
  legend_font = 7

  # Try it
  g0_plot(Rg0, panes = 3)

} # end not run ================================================================

Rg0 %>% names

# Handle if the input is straight from g0_model instead of g0_table
if(!is.data.frame(Rg0)){
  Rg0 <- Rg0$Rg0
  Rg0$title <- 'Model result'
}

# Rank species groups by their lowest Rg0 value
gtab <-
  Rg0 %>%
  dplyr::group_by(title) %>%
  dplyr::summarize(gmin = min(Rg0)) %>%
  dplyr::arrange(desc(gmin))
gtab

# Set up group assignments
# (divvy up species into plot panes according to their detectability)
nrow(gtab)
panes
(breaks <- round(seq(1,nrow(gtab),length=panes+1)))
(starts <- breaks[1:(length(breaks)-1)])
(ends <- breaks[2:(length(breaks))] - 1)

groups <- rep(panes, times = nrow(gtab)) ; groups
for(i in 1:length(starts)){
  groups[starts[i]:ends[i]] <- i
}
groups

gtab$Group <- groups
gtab

Rg0new <- dplyr::left_join(Rg0, gtab, by = 'title')

# Build multi-pane plot dynamically # ==========================================

ggList <- lapply(split(Rg0new, Rg0new$Group),
                 function(i) {
                   ggplot(i,
                          aes(x=bft, y=Rg0, color=title)) +
                     geom_line() + geom_point(size=.5) +
                     scale_y_continuous(breaks = seq(0,1,by=0.1), limits=c(0,1)) +
                     theme_light() +
                     scale_x_continuous(breaks=0:6) +
                     xlab('Beaufort Sea State') + ylab('Relative g(0)') +
                     labs(color = 'Species group') +
                     theme(legend.title = element_text(size=(legend_font + 1)), #change legend title font size
                           legend.key.height = unit(legend_key, 'cm'), #change legend key height
                           legend.text = element_text(size=legend_font))
                 }
)

# ==============================================================================
# Print plot

gg <- cowplot::plot_grid(plotlist = ggList, ncol = 1,
                   align = 'v')

#print(gg)
return(gg)

}
