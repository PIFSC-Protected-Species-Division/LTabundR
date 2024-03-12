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
#' @param edits An optional input: a `list` indicating manual adjustments to the subgroup `events` data.
#' This `list` can contain elements produced by the `LTabundR` functions `subgroup_populations()` or `subgroup_edits()`,
#' or simply `list`(s) that the user prepares themselves.
#'
#' Each `list` element needs to be either a `data.frame` or a `list`
#' with the following names: `edit`, `cohort`, `crusie`, and `sgid`. The remaining names depend on the type of `edit`
#' (see the documentation for the `subgroup_edits()` function for details). Edits will be applied in the order in which they are supplied.
#'
#' If this input is supplied, the `process_subgroups()` function will first process the data using default operations,
#' then redact the `events` data according to this input. The recommended workflow here is:
#' (1) process your survey data without supplying `edits`;
#' (2) review the `events` data using the `subgroup_explorer()` function;
#' (3) if revisions are needed, stage those edits using `subgroup_populations()` function, `subgroup_edits()` function, or manually coded lists.
#' (4) collate those staged edits into a list.
#' (5) re-run `process_subgroups()`, this time supplying the `edits` input.
#'
#' See the vignette for details.
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
                              edits = NULL,
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
    edits = NULL
    verbose=TRUE
    cohorts_i = 1

    #data("cnp_150km_1986_2020")
    #cruz <- cnp_150km_1986_2020
    #cruz_structure(cruz)
    #cruz_explorer(cruz, cohort='pseudorca')

    #phase_edits <- subgroup_phases(cruz, cohort='pseudorca')
    #phase_edits

    # Prepare some fake edits for testing
    #data("cnp_150km_1986_2020")
    #cruz <- cnp_150km_1986_2020
    cruz <- process_subgroups(cruz)
    cohort = 'default'
    #cohort = 'pseudorca'
    default_pop = 'pelagic'
    mhi <- cruz$settings$strata$MHI
    nwhi <- cruz$settings$strata$NWHI
    eez <- cruz$settings$strata$HI_EEZ
    populations <- list(MHI = mhi, NWHI = nwhi)
    (new_pops <- subgroup_populations(populations, cruz, cohort, default_pop))
    edits <-
      list(new_pops,
           subgroup_edits(cohort=cohort, sgid = cruz$cohorts$default$subgroups$events$sgid[1],
                          phase = 2),
           subgroup_edits(cohort=cohort, sgid = cruz$cohorts$default$subgroups$events$sgid[2],
                          population = 'hahaha', pop_prob = '1'),
           subgroup_edits(cohort=cohort, sgid = cruz$cohorts$default$subgroups$events$sgid[3],
                          phase=2,
                          population = 'hahaha', pop_prob = 'hohoho'),
           subgroup_edits(cohort=cohort, sgid = cruz$cohorts$default$subgroups$events$sgid[4],
                          exclude = TRUE))
    edits

    subgroup_explorer(cruz, cohort=1)

  } #===========================================================================

  # save a safe copy of the input data
  cruzi <- cruz

  # Harvest subgroup phases data

  # Loop through each cohort
  cohorts_i <- 1
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

      # Assign phases ==========================================================
      # Any with OnEffort == TRUE is Phase 1.
      # Any with OnEffort == FALSE AND NOT with an "S" in subgroup name is Phase 1.
      # Anything else is Phase 2

      # Find which subgroups have an "S" (this means they are probably Phase 2)
      events$SubGrp %>% unique %>% sort
      events <-
        events %>%
        rowwise() %>%
        mutate(has_S = ifelse(nchar(SubGrp)>1 & grepl('S', SubGrp),
                              TRUE,
                              FALSE)) %>%
        mutate(phase = ifelse(OnEffort == TRUE,
                              1,
                              ifelse(has_S == FALSE,
                                     1,
                                     2))) %>%
        ungroup() %>%
        select(-has_S)

      # Review
      events$phase %>% table
      events$sgid %>% table %>% table

      # Add placeholder for population assignments =============================
      events$population <- NA
      events$pop_prob <- NA

      # Apply subgroup_edits, if any ===========================================
      if(!is.null(edits)){

        new_events <- events
        edits %>% length
        lapply(edits, unlist, use.names=FALSE)

        # Flatten edits
        i=2
        new_edits <- c()
        for(i in 1:length(edits)){
          (editi <- edits[[i]])
          if(is.data.frame(editi)){
            editi <- split(editi, seq(nrow(editi)))
          }else{
            new_editi <- c()
            for(ii in 1:length(editi)){
              (editii <- editi[[ii]])
              editiii <- split(editii, seq(nrow(editii)))
              new_editi <- c(new_editi, editiii)
            }
            new_editi
            editi <- new_editi
          }
          editi
          new_edits <- c(new_edits, editi)
        }
        new_edits

        # Now loop through edits
        i=2
        for(i in 1:length(new_edits)){
          (editi <- new_edits[[i]])

          (ei <- editi$edit)

          # Check to see if cohort applies
          (edit_cohort <- editi$cohort)
          if(is.numeric(edit_cohort)){edit_cohort <- names(cruz$cohorts)[edit_cohort]}
          edit_cohort
          if(edit_cohort == cohort_name){
            (stop_msg <- paste0('FAIL! This edit did not find a match in the data :: edit ',ii,' within edit slot ',i,
                                ' :: sgid ', editi$sgid))
            if(ei == 'population'){
              editi
              (matchi <- which(new_events$sgid %in% editi$sgid))
              if(length(matchi)>0){
                new_events$population[matchi] <- editi$population
                new_events$pop_prob[matchi] <- editi$pop_prob
              }else{ stop(stop_msg) }
            }
            if(ei == 'phase'){
              editi
              (matchi <- which(new_events$sgid == editi$sgid))
              if(length(matchi)>0){
                new_events$phase[matchi] <- editi$phase
              }else{ stop(stop_msg) }
            }
            if(ei == 'ObsStd'){
              editi
              (matchi <- which(new_events$sgid == editi$sgid))
              if(length(matchi)>0){
                new_events$ObsStd[matchi] <- editi$ObsStd
              }else{ stop(stop_msg) }
            }
            if(ei == 'exclude'){
              editi
              (matchi <- which(new_events$sgid == editi$sgid))
              if(length(matchi)>0){
                new_events <- new_events[-matchi, ]
              }else{ stop(stop_msg) }
            }
          } # end of make sure cohort applies
        }

        # Review post
        events %>% nrow
        new_events %>% nrow

        events$phase %>% head
        new_events$phase %>% head

        events$population %>% head
        new_events$population %>% head

        events$pop_prob %>% head
        new_events$pop_prob %>% head

        # update events
        events <- new_events
      }

      # OLD WAY (SKIPPING NOW) #################################################

      if(FALSE){
        # phase_edits An optional input: a `data.frame` indicating manual adjustments to the protocol phase that data are assigned to.
        # This `data.frame` is produced by the `LTabundR` function `subgroup_phases()`; see its documentation for formatting.
        # If this input is supplied, the `process_subgroups()` function will first automatically assign data to phases,
        # then redact those assignments using this input. The recommended workflow here is:
        # (1) process your survey data without supplying `phase_edits`;
        # (2) review the automatic phase assignments using the `subgroup_phases()` function;
        # (3) if revisions are needed, stage those edits using the same function.
        # (4) re-run `process_subgroups()`, this time supplying the `phase_edits` input.

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
      }
      # END OF OLD WAY BEING SKIPPED ###########################################

      #=========================================================================

      # Summarize events into subgroups
      # one row for each subgroup in each phase of a sighting, with averaged school size estimates
      subgroups <- subgroup_subgroups(events)
      subgroups %>% head

      # Summarize subgroups into sightings
      # one row for each phase of each sighting, with summed group size estimates (by summing subgroups)
      suppressMessages({
        sitsum <- subgroup_sightings(subgroups)
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
