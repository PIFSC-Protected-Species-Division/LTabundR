#' Add `cruz` geostrata to a base `ggplot2` map.
#'
#' @param p Base `ggplot2` to which you are adding this `geom`.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param strata desc
#' @param color Color string.
#' @param lwd Boundary line width.
#' @param lty Boundary line type.
#' @param crs Optional CRS.
#'
#' @return An updated `ggplot2` object.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
gg_geostratum <- function(p,
                          cruz,
                          strata,
                          color = 'orchid4',
                          lwd=.5,
                          lty = 1,
                          crs = 4326){

  if(FALSE){
    p <- ggplot()
    strata = c('MHI', 'WHICEAS')
    color = 'orchid4'
    lwd = .5
    lty=1
    crs <- 4326
    legend = TRUE
  }

  cruz_strata <- cruz$settings$strata
  cruz_strata %>% names

  if(length(strata) > 1){
    if(length(color) == 1){color <- rep(color, times=length(strata))}
    if(length(lwd) == 1){lwd <- rep(lwd, times=length(strata))}
    if(length(lty) == 1){lty <- rep(lty, times=length(strata))}
  }

  i=1
  for(i in 1:length(strata)){
    (strati <- strata[i])
    (cruz_strati <- cruz_strata[strati][[1]])
    poli <- process_polygon(cruz_strati)$sf
    poli
    sf::st_crs(poli) <- crs
    p <-
      p + geom_sf(data= poli,
                  fill=NA,
                  color = color[i],
                  lwd = lwd[i],
                  lty = lty[i])
  }

  return(p)
}
