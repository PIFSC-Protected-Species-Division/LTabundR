#' Add `cruz` sightings to a base `ggplot2` map.
#'
#' @param p Base `ggplot2` to which you are adding this `geom`.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param spp Character vector of species codes.
#' @param spp_translate Dataframe of scecies code information for translating between codes and common names. See `data(species_codes)` for an example.
#' @param color_by_spp Color code by species code?
#' @param color Color string.
#' @param pch Sighting point type.
#' @param cex Sighting point size.
#' @param alpha Sighting transparency.
#'
#' @return An updated `ggplot2` object.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
#'
gg_sightings <- function(p,
                         cruz,
                         cohort=1,
                         spp = NULL,
                         spp_translate = NULL,
                         color_by_spp = FALSE,
                         color = 'black',
                         pch = 16,
                         cex=.5,
                         alpha =.5){

  if(FALSE){
    p <- ggplot()
    cohort = 1
    spp_translate <- species_codes
    color_by_spp = FALSE
    spp <- '076'
    color = 'black'
    lwd = .5
    alpha = .5
  }

  siti <- cruz$cohorts[[cohort]]$sightings
  if(!is.null(spp)){
    siti <- siti %>% filter(species %in% spp)
  }

  if(!is.null(spp_translate)){
    #spp_translate = species_codes
    newspp <- c()
    i=1
    for(i in 1:nrow(siti)){
      transli <- species_translator(siti$species[i], codes=spp_translate)
      if(nrow(transli)>0){
        newspp[i] <- transli$common[1]
      }else{
        newspp[i] <- siti$species[i]
      }
    }
    newspp
    siti$species <- newspp
  }

  if(color_by_spp){
    p <- p +
      geom_point(data= siti,
                 mapping=aes(x=Lon,
                             y=Lat,
                             color=species),
                 size = cex,
                 shape=pch,
                 alpha = alpha)
  }else{
    p <- p +
      geom_point(data= siti,
                 mapping=aes(x=Lon,
                             y=Lat),
                 color = color,
                 size = cex,
                 shape=pch,
                 alpha = alpha)
  }

  return(p)
}
