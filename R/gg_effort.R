#' Add `cruz` effort to a base `ggplot2` map.
#'
#' @param p Base `ggplot2` to which you are adding this `geom`.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param color_by_bft Color code by Beaufort sea state?
#' @param color Color string.
#' @param lwd Effort thickness.
#' @param alpha Effort transparency.
#'
#' @return An updated `ggplot2` object.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
#'
gg_effort <- function(p,
                      cruz,
                      cohort=1,
                      color_by_bft = FALSE,
                      color = 'black',
                      lwd=.1,
                      alpha =.5){

  if(FALSE){
    p <- ggplot()
    cohort = 1
    color_by_bft = FALSE
    color = 'black'
    lwd = .5
    alpha = .5
  }

  dasi <- cruz$cohorts[[cohort]]$das

  if(color_by_bft){
    p <- p +
      geom_point(data= dasi,
                 mapping=aes(x=Lon,
                             y=Lat,
                             color=Bft),
                 size = lwd,
                 alpha = alpha)
  }else{
    p <- p +
      geom_point(data= dasi,
                 mapping=aes(x=Lon,
                             y=Lat),
                 color = color,
                 size = lwd,
                 alpha = alpha)
  }

  return(p)
}
