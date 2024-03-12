#' Summarize sightings (for subgroup species) based on subgroups
#'
#' This is an internal function, not typically called by the user.
#' It is called during the `process_subgroups()` routine within `process_surveys()`,
#' and may also be called if the user manually edits subgroup phase assignments using the `subgroup_phases()` function.
#' It takes the result of the function `subgroup_subgroups()`.
#'
#' @param events
#'
#' @return A `data.frame` with one row for each phase of each sighting.
#'
#' @export
#' @import dplyr
#'
subgroup_sightings <- function(subgroups){

  if(FALSE){ # objects for debugging ===========================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    (events <- cruz$cohorts$pseudorca$subgroups$events) %>% head
    subgroups <- subgroup_subgroups(events)
  } #===========================================================================

  results <- subgroups

  # get column indces of stratum columns
  (stratum_cols <- names(results)[grep('stratum',names(results))])

  # Combine into single phase-sighting
  # add all subgroups together in each phase
  sitsum <-
    results %>%
    dplyr::group_by(Cruise,Date,SightNo,Phase) %>%
    dplyr::summarize(across(DateTime:Lon,mean),
                     dplyr::across(Bft:ObsInd,function(x){x[1]}),
                     Obs_Sight = Obs_Sight[1],
                     Species = Species[1],
                     Angle = mean(Angle, na.rm=TRUE),
                     RadDist = mean(RadDist, na.rm=TRUE),
                     PerpDist = mean(PerpDist, na.rm=TRUE),
                     GSBest = sum(GSBest, na.rm=TRUE),
                     GSBest_geom = sum(GSBest_geom, na.rm=TRUE),
                     GSBest_allvalid = all(GSBest_valid == TRUE),
                     GSBest_geom_allvalid = all(GSBest_geom_valid == TRUE),
                     ObsStd = ObsStd[1],
                     seg_id = seg_id[1],
                     EffType = EffType[1],
                     OnEffort = OnEffort[1],
                     population = population[1],
                     pop_prob = pop_prob[1],
                     dplyr::across(all_of(stratum_cols),function(x){x[1]})) %>%
    as.data.frame()

  # review
  sitsum %>% head

  return(sitsum)
}
