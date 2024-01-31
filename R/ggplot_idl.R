#' Modify a `ggplot` object to handle the International Date Line (IDL)
#'
#' @param p_base A `ggplot` object that you want to visualize in a way that handles the International Date Line.
#' @param lon_range Desired longitude range, as a two-element vector.
#' The first element should be a positive number between 0 and 180 (indicating decimal degrees East),
#' the second should be a negative number between -180 and 0 (indicating decimal degrees West).
#' @param lat_range Desired latitude range, as a two-element vector of decimal degrees (positive = N, negative = S).
#' @param idl_padding The space between the Eastern hemisphere plot and the Western hemisphere plot.
#' @param axes If `FALSE`, axis labels and ticks will be removed.
#' @param bypass An option to return an unmodified `ggplot`, to ease automated toggling when using in various functions within `LTabundR`.

#' @return A `ggplot2` object that is actually two plots side by side, with a small space in between indicating the IDL.
#' @import dplyr
#' @import ggplot2
#' @import ggbreak
#' @export
#'
ggplot_idl <- function(p_base,
                       lon_range = c(100, -100),
                       lat_range = NULL,
                       idl_padding = 0.02,
                       axes = TRUE,
                       bypass = FALSE){

  if(bypass){
    pz <- p_base
  }else{

    lon_range
    (lon_diff <- c((180 - lon_range[1]),
                   abs(-180 - lon_range[2])))
    (lon_sum <- sum(lon_diff))
    (lon_ratio <- lon_diff / lon_sum)

    pe <-
      p_base +
      coord_sf(xlim = c(lon_range[1], 180),
               ylim = lat_range,
               expand=FALSE) +
      theme(plot.margin = unit(c(0.01, 0.02, 0.01, 0.01), "inches"))

    if(lon_ratio[1] < .33){
      nlabs <- ifelse(lon_ratio[1] < .15, 1, 2)
      pe <- pe + scale_x_continuous(breaks = round(seq(lon_range[1], 180, length=nlabs), 2))
    }

    pw <-
      p_base +
      coord_sf(xlim = c(-180, lon_range[2]),
               ylim = lat_range,
               expand = FALSE) +
      theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.02), "inches")) +
      ylab(NULL) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

    if(lon_ratio[2] < .33){
      nlabs <- ifelse(lon_ratio[2] < .15, 1, 2)
      pw <- pw + scale_x_continuous(breaks = round(seq(-180, lon_range[2], length=nlabs), 2))
    }

    if(axes == FALSE){
      pe <- pe + theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank())

      pw <- pw + theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank())
    }

    pz <- pe + pw
    pz

  }
  return(pz)
}
