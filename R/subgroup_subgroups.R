#' Summarize subgroups based on subgroup events
#'
#' This is an internal function, not typically called by the user.
#' It is called during the `process_subgroups()` routine within `process_surveys()`,
#' and may also be called if the user manually edits subgroup phase assignments using the `subgroup_phases()` function.
#' It takes the result of the function `subgroup_events()`.
#'
#' @param events The output of `subgroup_events()`.
#'
#' @return A `data.frame` with one row for each subgroup in each phase of a sighting.
#'
#' @export
#' @import dplyr
#'
subgroup_subgroups <- function(events){

  if(FALSE){ # objects for debugging ===========================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    (events <- cruz$cohorts$pseudorca$subgroups$events) %>% head
  } #===========================================================================

  # Determine phase & calculate geometric means
  # Stage results
  results <- data.frame()

  # Loop through each unique date-sightings
  usits <- events$sitid %>% unique
  usits
  i=9
  for(i in 1:length(usits)){
    usiti <- usits[i]
    subi <- events[events$sitid == usiti,] ; subi

    # Loop through each subgroup in this sighting
    usg <- subi$SubGrp %>% unique ; usg
    j=2 # for debugging
    for(j in 1:length(usg)){
      usgj <- usg[j]
      subij <- subi[subi$SubGrp == usgj,] ; subij
      subij
      #message('sighting i = ', i, ' subgroup j = ', j,' nrow = ',nrow(subij))
      sub_result <-
        subij %>%
        dplyr::rename(GSBest_raw = GSBest,
                      GSH_raw = GSH,
                      GSL_raw = GSL) %>%
        dplyr::group_by(phase) %>%
        dplyr::summarize(Cruise = Cruise[1],
                         Date = Date[1],
                         dplyr::across(DateTime:Species,function(x){x[1]}),
                         SubGrp = SubGrp[1],
                         Angle = mean(Angle, na.rm=TRUE),
                         RadDist = mean(RadDist, na.rm=TRUE),
                         PerpDist = mean(PerpDist),
                         GSBest = round(mean(GSBest_raw, na.rm=TRUE),2),
                         GSH = round(mean(GSH_raw, na.rm=TRUE),2),
                         GSL = round(mean(GSL_raw, na.rm=TRUE),2),
                         GSBest_geom = round(exp(mean(log(GSBest_raw), na.rm=TRUE)),2),
                         GSH_geom = round(exp(mean(log(GSH_raw), na.rm=TRUE)),2),
                         GSL_geom = round(exp(mean(log(GSL_raw), na.rm=TRUE)),2),
                         GSBest_valid = ifelse(!is.na(GSBest), TRUE, FALSE),
                         GSBest_geom_valid = ifelse(!is.na(GSBest_geom), TRUE, FALSE),
                         ObsStd = ObsStd[1],
                         seg_id = seg_id[1],
                         sgid = sgid[1],
                         sitid = sitid[1],
                         population = population[1],
                         pop_prob = pop_prob[1],
                         dplyr::across(grep('stratum',names(subij)),function(x){x[1]})) %>%
        as.data.frame

      sub_result # review

      # Make sure group size estimates are valid
      if(!is.finite(sub_result$GSBest)){sub_result$GSBest <- sub_result$GSL}
      if(!is.finite(sub_result$GSBest_geom)){sub_result$GSBest_geom <- sub_result$GSL_geom}

      # Add to growing results object
      results <- rbind(results,
                       sub_result)
    }
  }

  # Review for debugging
  nrow(results)
  results %>% head
  results$sgid
  results$sgid %>% table %>% table
  results$Phase = results$phase
  #results$phase <- results$sgid <- results$sitid <- NULL
  results$Phase %>% table
  results$GSBest %>% table(useNA='always')
  results$GSBest_geom %>% table(useNA='always')
  results$Phase %>% table

  return(results)
}
