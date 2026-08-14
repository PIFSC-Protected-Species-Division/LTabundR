#' Find affiliated species from mixed-species groups
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#'
#' @param cohort The cohort whose sightings you would like to recalibrate,
#' provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @param spp Character vector of species codes. Groups containing these species
#' will be found, and all other species present in their mixed-species groups will
#' be returned as "affiliates".
#'
#' @return A character vector of species codes for all species affiliated with `spp` in
#' mixed-species groups, provided in increasing order of frequency.
#'
#' @import dplyr
#' @export
#'
mixed_group_affiliates <- function(cruz,
                                   cohort = 1,
                                   spp){

  if(FALSE){ # for development & debugging =====================================
    library(dplyr)
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cohort=1
    spp <- '013'

    mixed_group_affiliates(cruz, spp='013')
  } #===========================================================================

  # Filter down to the cohort-analysis specified
  cohorti <- cruz$cohorts[[cohort]]
  names(cohorti) # review
  sits <- cohorti$sightings
  sits %>% head

  # Get sighting numbers for detections involving this species
  (sitnos <- sits %>% filter(species %in% spp) %>% pull(SightNoDaily) %>% unique)

  # Get those sightings
  (those_sits <- sits %>% filter(SightNoDaily %in% sitnos))

  # Get associated species
  (affiliates <- those_sits %>% pull(species) %>% table %>% sort)
  (affiliates <- names(affiliates)[which(! names(affiliates) %in% spp)])

  #print(affiliates)

  return(affiliates)
}
