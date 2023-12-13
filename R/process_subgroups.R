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
#' @param phase_edits An optional input: a `data.frame` indicating manual adjustments to the protocol phase that data are assigned to.
#' This `data.frame` is produced by the `LTabundR` function `subgroup_phases()`; see its documentation for formatting.
#' If this input is supplied, the `process_subgroups()` function will first automatically assign data to phases,
#' then redact those assignments using this input. The recommended workflow here is:
#' (1) process your survey data without supplying `phase_edits`;
#' (2) review the automatic phase assignments using the `subgroup_phases()` function;
#' (3) if revisions are needed, stage those edits using the same function.
#' (4) re-run `process_subgroups()`, this time supplying the `phase_edits` input.
#'
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
                              phase_edits = NULL,
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
    cohorts_i = 4

    phase_edits <- subgroup_phases(cruz, cohort='pseudorca')
    phase_edits

  } #===========================================================================

  # save a safe copy of the input data
  cruzi <- cruz

  # Harvest subgroup phases data

  # Loop through each cohort
  for(cohorts_i in 1:length(cruzi$cohorts)){
    cohorti <- cruzi$cohorts[[cohorts_i]] # get cohort data
    (cohort_name <- names(cruzi$cohorts)[[cohorts_i]])
    ani <- cohorti # rename for convenience
    ani %>% names

    # Get settings
    cohort_settings <- cruzi$settings$cohorts[[cohorts_i]] # get cohort settings
    (species_filter <- cohort_settings$species) # get species for this cohort

    if(verbose){message('Cohort "',names(cruzi$cohorts)[cohorts_i],
                        '": processing subgroups ...')}

    # Find subgroup events using a LTabundR function
    dass <- ani$das
    events <- subgroup_events(dass, species_filter)

    if(nrow(events)>0){
      # the events dataframe:
      events %>% head
      # each row is a single school size estimate for a single subgroup
      # within a single phase of a single sighting
      # (effectively, the 'raw' data for subgroups within the `das` data).

      # Attempt automated phase assignment
      events$phase <- 2
      events$phase[events$OnEffort == TRUE] <- 1
      events$phase %>% table
      events$sgid %>% table %>% table

      # Modify using the phase_edits input, if needed ==========================
      old_events <- events # store old version of events
      if(!is.null(phase_edits)){

        # See if any revisions have been stored for this cohort
        (pec <- phase_edits %>% filter(cohort == cohort_name))
        if(nrow(pec)>0){
          pec2join <- pec %>% select(Cruise, Species, Line, SubGrp, Event, new_phase)
          suppressMessages({
            revised_events <- left_join(events, pec2join)
          })
          revised_events <-
            revised_events %>%
            rowwise() %>%
            mutate(phase = ifelse(is.na(new_phase), phase, new_phase)) %>%
            ungroup() %>%
            select(-new_phase) %>%
            as.data.frame

          revised_events %>% head
          events <- revised_events
        }
      }

      # Review changes
      data.frame(old_phase = old_events$phase, new_phase = events$phase)

      #=========================================================================

      # Summarize events into subgroups
      # one row for each subgroup in each phase of a sighting, with averaged school size estimates
      subgroups <- subgroup_subgroups(events)
      subgroups %>% head

      # Summarize subgroups into sightings
      # one row for each phase of each sighting, with summed group size estimates (by summing subgroups)
      suppressMessages({
        sitsum <- subgroup_sightings(results)
      })
      sitsum %>% head

      # prepare results list
      subgroups_list <- list(sightings = sitsum, # one row per phase-sighting
                             subgroups = subgroups, # one row per phase-subgroup
                             events = events) # one row per G event in DAS data

      # review
      subgroups_list$sightings %>% head
      subgroups_list$subgroups %>% head
      subgroups_list$subgroups %>% dplyr::select(GSBest, GSBest_geom, GSBest_valid, GSBest_geom_valid)
      subgroups_list$events %>% head

      # Add subgroups table to new slot in cohort's list
      cruzi$cohorts[[cohorts_i]]$subgroups <- subgroups_list

    } # if there are G events in this das file
  } # loop through each cohort

  return(cruzi)
}
