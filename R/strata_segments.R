#' Display/map stratum assignments for segments
#'
#' A simple diagnostic function, usually not used by most analysts.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to display, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param plot_title Optional title for plot.

#' @return A `ggplot2` object.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
#'
strata_segments <- function(cruz,
                            cohort=1,
                            plot_title = 'Stratum assignments'){

  if(FALSE){
    data("cnp_150km_1986_2020")
    cohort <- 1

    cruz <- cnp_150km_1986_2020
    strata_segments(cruz)
  }

  suppressWarnings({suppressMessages({

    segs <- cruz$cohorts[[cohort]]$segments
    strats <- cruz$settings$cohorts[[1]]$strata
    stratdat <- cruz$settings$strata
    i <- which(names(stratdat) %in% strats)
    strata <- stratdat[i]

    xmin <- xmax <- ymin <- ymax <- NA
    for(i in 1:length(strata)){
      strati <- strata[[i]]
      (xmin <- min(c(xmin, min(strati$Lon)), na.rm=TRUE))
      (xmax <- max(c(xmax, max(strati$Lon)), na.rm=TRUE))
      (ymin <- min(c(ymin, min(strati$Lat)), na.rm=TRUE))
      (ymax <- max(c(ymax, max(strati$Lat)), na.rm=TRUE))
    }
    xmin
    xmax
    ymin
    ymax

    (world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))
    p <- ggplot(data = world) +
      geom_sf() +
      geom_point(data=segs,
                 mapping=aes(x=mlon,
                             y=mlat,
                             color=stratum)) +
      xlim(xmin, xmax) + ylim(ymin, ymax)

    i=1
    for(i in 1:length(strata)){
      strati <- strata[[i]]
      sfi <- process_polygon(strati)$sf
      st_crs(sfi) = 4326
      p <- p + geom_sf(data=sfi, fill='blue', alpha=.1)
    }
    #p

    p <- p + labs(title = plot_title,
                  subtitle = 'Dots are center points of segments')

  }) })

  return(p)
}
