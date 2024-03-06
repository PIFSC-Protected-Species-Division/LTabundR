#' Assign subgroups to populations based on polygons
#'
#' @param populations A named list in which each slot is a `data.frame` of coordinates for a geostratum polygon.
#' Each `data.frame` must have `Lon` and `Lat` as the first two columns,
#' providing coordinates in decimal degrees in which West and South coordinates are negative.
#' It is acceptable if vertices in the eastern hemisphere are described using negative longitudes below -180, e.g., -185.
#' Other columns are allowed, but the first two need to be `Lon` and `Lat`.
#' The name of the slot holding the `data.frame` will be used as a reference name for the stratum.
#' Note that if coordinates in your data or in your collection of strata span
#' the International Date Line (IDL) such that some longitudes are positive
#' and some are negative, during data processing all longitudes will be coerced
#' to negative degrees West.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to use, provided as the number or name of the slot in `cruz$cohorts` to be referenced.
#' @param default_pop desc
#' @return A `data.frame` of population assignments for each subgroup event, formatted to be accepted as edits
#' as an input in `process_subgroups()`.
#' @export
#' @import dplyr
#' @import tidyr
#'
subgroup_populations <- function(populations,
                                 cruz,
                                 cohort='pseudorca',
                                 default_pop = 'pelagic'){
  # will preliminarily assign each subgroup to a population
  # based on a set of polygons you provide.
  # You can adjust these assignments, and their relative probabilities,
  # in subgroup_edits

  if(FALSE){ #==================================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cohort = 'pseudorca'
    default_pop = 'pelagic'

    mhi <- cruz$settings$strata$MHI
    nwhi <- cruz$settings$strata$NWHI
    eez <- cruz$settings$strata$HI_EEZ
    populations <- list(MHI = mhi,
                        NWHI = nwhi)

    # Try it
    subgroup_populations(populations, cruz, cohort, default_pop)

  }  #==========================================================================

  # Filter to correct cohort and analysis
  cohorti <- cruz$cohorts[[cohort]] ; names(cohorti)
  sg <- cohorti$subgroups ; names(sg)
  events <- sg$events
  events %>% head
  events$sgid
  events$temp_line_id <- 1:nrow(events)

  strata <- populations
  i=1
  for(i in 1:length(strata)){
    strati <- populations[i] # get dataframe of coordinates for this stratum
    sname <- names(strati) # get name of stratum
    sname
    strati
    if(verbose){message('--- Population polygon ',i,' of ',length(strata),' :: ',sname,' ...')}

    suppressWarnings({suppressMessages({
      (polygon_dataframe <- strati[[1]])
      poli_list <- process_polygon(strati[[1]]) # convert from df to polygon (also returns area in km2)
      poli <- poli_list$sf
      dasloc <- events %>% dplyr::filter(!is.na(Lon) & !is.na(Lat)) # remove invalid locations
      strata_das <- coordinates_in_strata(lon = dasloc$Lon, lat = dasloc$Lat, poli=poli)
    })})
    # returns a decision for every coordinate: 1 = in stratum, 0 = not
    table(strata_das, useNA='ifany') #checkout result (debugging)
    dasloc$new_var <- strata_das # Save these decisions in a new column
    dasloc <- dasloc %>%
      dplyr::select(temp_line_id, new_var) # create a simple df for joining
    events <- dplyr::left_join(events, dasloc, by='temp_line_id') # join new column to das
    table(events$new_var, useNA='ifany') # review (debugging)
    # rename the new column using the stratum name
    names(events)[which(names(events)=='new_var')] <- paste0('pop_',sname)
    head(events)
  }
  events
  names(events)

  # Assign probabilities
  (pops <- names(populations))
  (pop_cols <- which(names(evi) %in% paste0('pop_',pops)))
  #(prob_cols <- which(names(evi) %in% paste0('prob_',pops)))
  events$population <- default_pop
  events$pop_prob <- 1
  i=1
  i=nrow(events)
  for(i in 1:nrow(events)){
    (evi <- events[i,]) %>% as.data.frame
    (popi <- evi[1,pop_cols] %>% as.logical)
    if(length(which(popi))>0){
      (n_in <- length(which(popi)))
      probs <- rep(0, times=length(pops))
      (probs[popi] <- round(1/n_in, digits=3))
      (probs <- probs[probs > 0])
      #events[i, prob_cols] <- probs
      events$population[i] <- paste(pops[popi], collapse=';')
      events$pop_prob[i] <- paste(probs, collapse=';')
      events[i,]
    }
  }

  events %>% as.data.frame %>% head
  events %>% tail
  events$population %>% table(useNA='ifany')
  events$pop_prob %>% table(useNA='ifany')

  edits <-
    events %>%
    mutate(cruise = Cruise) %>%
    mutate(edit = 'population') %>%
    mutate(cohort = cohort) %>%
    select(edit, cohort,
           #cruise,
           sgid, population, pop_prob)

  edits %>% head

  #split(edits, seq(nrow(edits)))

  return(edits)

}
