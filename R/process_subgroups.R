#' Analyze subgroup detections for false killer whales
#'
#' Subgroups are found based on `DAS` event code "`G`" and associated events.
#' Subgroup sizes are estimated for each phase (based upon the false killer whale protocol),
#' and both arithmetic and geometric means are provided.
#' \cr \cr
#' This is an internal function typically not called by a user directly.
#' It is the final processing subroutine called within `process_surveys()`, after the subroutine `process_sightings()`.
#'
#' @param cruz A `cruz` object passed from `process_surveys()`.
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A finalized `cruz` object.
#' If subgroups are found, a `subgroups` slot is added to the analysis list for a cohort.
#' This `subgroups` slot holds a list with three dataframes:
#' \enumerate{
#' \item `events`, in which each row is a school size estimate for a single subgroup during a single phase -- 1 or 2 -- within a single sighting;
#' \item `subgroups`, in which each row is a single phase for a single subgroup, with all school size estimates averaged together (both arithmetically and geometrically);
#' \item `sightings`, in which each row is a school size estimate for a single phase for a single sighting, with all subgroup school sizes summed together.
#' }
#' Phase is determined simply according to the `OnEffort` column. If it is `TRUE`, `Phase` is 1; if not, `Phase` is 2.
#'
#' @export
#' @import dplyr
#'
process_subgroups <- function(cruz,
                              verbose=TRUE){

  if(FALSE){ # debugging materials -- not run! =================================
    data(example_settings)
    settings <- example_settings
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    cruz <- process_strata(das, settings)
    cruz <- das_format(cruz)
    cruz <- segmentize(cruz)
    cruz <- process_sightings(cruz)
    verbose=TRUE
    cohorts_i = 1
  } #===========================================================================

  # save a safe copy of the input data
  cruzi <- cruz

  # Harvest subgroup phases data

  # Loop through each cohort
  for(cohorts_i in 1:length(cruzi$cohorts)){
    cohorti <- cruzi$cohorts[[cohorts_i]] # get cohort data
    ani <- cohorti # rename for convenience
    ani %>% names

    # Get settings
    cohort_settings <- cruzi$settings$cohorts[[cohorts_i]] # get cohort settings
    (species_filter <- cohort_settings$species) # get species for this cohort

    if(verbose){message('Cohort "',names(cruzi$cohorts)[cohorts_i],
                        '": processing subgroups ...')}

    # Find subgroups using a LTabundR function
    dass <- ani$das
    subs <- get_subgroups(dass, species_filter)
    subs %>% head
    subs %>% tail

    if(nrow(subs)>0){

      subs %>% head

      # Assign Off-Effort rows as Phase 2
      subs$phase <- 2
      subs$phase[subs$OnEffort == TRUE] <- 1
      # subs$phase <- 1
      # subs$phase[subs$EffType != 'S'] <- 2
      subs$phase %>% table
      subs$sgid %>% table %>% table

      # Determine phase & calculate geometric means
      # Stage results
      results <- data.frame()

      # Loop through each unique date-sightings
      usits <- subs$sitid %>% unique
      usits
      i=9
      for(i in 1:length(usits)){
        usiti <- usits[i]
        subi <- subs[subs$sitid == usiti,] ; subi

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
                             dplyr::across(Angle:seg_id,mean),
                             PerpDist = mean(PerpDist),
                             GSBest = round(mean(GSBest_raw, na.rm=TRUE),2),
                             GSH = round(mean(GSH_raw, na.rm=TRUE),2),
                             GSL = round(mean(GSL_raw, na.rm=TRUE),2),
                             GSBest_geom = round(exp(mean(log(GSBest_raw), na.rm=TRUE)),2),
                             GSH_geom = round(exp(mean(log(GSH_raw), na.rm=TRUE)),2),
                             GSL_geom = round(exp(mean(log(GSL_raw), na.rm=TRUE)),2),
                             GSBest_valid = ifelse(!is.na(GSBest), TRUE, FALSE),
                             GSBest_geom_valid = ifelse(!is.na(GSBest_geom), TRUE, FALSE),
                             seg_id = seg_id[1],
                             use = use[1],
                             sgid = sgid[1],
                             sitid = sitid[1],
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
      results$sgid %>% table %>% table
      results$Phase = results$phase
      results$phase <- results$sgid <- results$sitid <- NULL
      results$Phase %>% table
      results$GSBest %>% table(useNA='always')
      results$GSBest_geom %>% table(useNA='always')
      results$Phase %>% table

      # get column indces of stratum columns
      (stratum_cols <- names(results)[grep('stratum',names(results))])

      # Combine into single phase-sighting
      # add all subgroups together in each phase
      sitsum <-
        results %>%
        dplyr::group_by(Cruise,Date,SightNo,Phase) %>%
        dplyr::summarize(across(DateTime:Lon,mean),
                         Bft = Bft[1],
                         Species = Species[1],
                         across(Angle:PerpDist,mean),
                         GSBest = sum(GSBest, na.rm=TRUE),
                         GSBest_geom = sum(GSBest_geom, na.rm=TRUE),
                         GSBest_allvalid = all(GSBest_valid == TRUE),
                         GSBest_geom_allvalid = all(GSBest_geom_valid == TRUE),
                         seg_id = seg_id[1],
                         EffType = EffType[1],
                         OnEffort = OnEffort[1],
                         use = use[1],
                         dplyr::across(all_of(stratum_cols),function(x){x[1]})) %>%
        as.data.frame()

      # review
      sitsum %>% head

      # prepare results list
      subgroups <- list(sightings = sitsum, # one row per phase-sighting
                        subgroups = results, # one row per phase-subgroup
                        events = subs) # one row per G event in DAS data

      # review
      subgroups$sightings %>% head
      subgroups$subgroups %>% head
      subgroups$subgroups %>% dplyr::select(GSBest, GSBest_geom, GSBest_valid, GSBest_geom_valid)
      subgroups$events %>% head

      # Add subgroups table to new slot in cohort's list
      cruzi$cohorts[[cohorts_i]]$subgroups <- subgroups

    } # if there are G events in this das file
  } # loop through each cohort

  return(cruzi)
}
