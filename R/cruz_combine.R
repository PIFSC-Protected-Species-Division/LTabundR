#' Combine several `cruz` objects
#'
#' This function combines processed `cruz` objects
#' (the outputs of `LTabundR::process_surveys()`),
#' under the assumption that the survey settings in both objects are the exact same.
#' If the same cohort name occurs in multiple `cruz` objects, the contents of the instances of the cohort
#' are checked for redundancy (using Cruise number - date combinations),
#' and only non-redundant content is combined.
#' If different cohorts occur in the supplied `cruz` objects,
#' the cohorts are added without modification.
#'
#' @param cruzes A `list` of `cruz` objects, e.g., `list(cruz1, cruz2, cruz3)`.
#' To understand `cruz` objects, see the output of the function `process_surveys()`.
#'
#' @return A single `cruz` object.
#' @export
#'
cruz_combine <- function(cruzes){

  if(FALSE){ #========================================== debugging -- not run!
    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020

    # MHI-only data
    cruz_mhi <- filter_cruz(cruz,
                            regions = 'MHI',
                            verbose = FALSE)
    # HI_EEZ-only data
    cruz_hi_eez <- filter_cruz(cruz,
                               regions = 'HI_EEZ',
                               not_regions = 'MHI',
                               verbose = FALSE)
    cruzes <- list(cruz_mhi, cruz_hi_eez)
    length(cruzes)
  } # ================================================== end debugging

  #=============================================================================
  message('Adding unique identifiers to each cruz object line ...')
  all_cohorts <- c() # meanwhile, get all cohort names
  # Loop through each cruz object
  i=1
  for(i in 1:length(cruzes)){
    message('--- cruz object ',i,' ... ')
    cruzi <- cruzes[[i]]
    cruzi %>% names
    # Add cohorts in this cruz object to growing list of cohort names
    all_cohorts <- c(all_cohorts, names(cruzi$cohorts)) %>% unique()

    # Loop through each cohort within this cruz object
    ci=1
    for(ci in 1:length(cruzi$cohorts)){
      message('--- --- cohort ',ci,' ...')
      cohorti <- cruzi$cohorts[[ci]]
      cohorti %>% names
      # Create unique identifiers for each row of data in each list within the cohort
      cohorti$das$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                    cohorti$das$Cruise,
                                    lubridate::date(cohorti$das$DateTime),
                                    sep='-')
      cohorti$segments$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                         cohorti$segments$Cruise,
                                         lubridate::date(cohorti$segments$DateTime1),
                                         sep='-')
      cohorti$sightings$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                          cohorti$sightings$Cruise,
                                          lubridate::date(cohorti$sightings$DateTime),
                                          sep='-')

      if('subgroups' %in% names(cohorti)){
        cohorti$subgroups %>% names
        cohorti$subgroups$sightings
        if(!is.null(cohorti$subgroups$events)){
          cohorti$subgroups$events$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                                     cohorti$subgroups$events$Cruise,
                                                     lubridate::date(cohorti$subgroups$events$DateTime),
                                                     sep='-')
        }
        if(!is.null(cohorti$subgroups$sightings)){
          cohorti$subgroups$sightings$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                                        cohorti$subgroups$sightings$Cruise,
                                                        lubridate::date(cohorti$subgroups$sightings$DateTime),
                                                        sep='-')
        }
        if(!is.null(cohorti$subgroups$subgroups)){
          cohorti$subgroups$subgroups$datum_id <- paste(names(cruzi$cohorts)[[ci]],
                                                        cohorti$subgroups$subgroups$Cruise,
                                                        lubridate::date(cohorti$subgroups$subgroups$DateTime),
                                                        sep='-')
        }
      }
      # Replace cohort with this modified version of it
      cruzi$cohorts[[ci]] <- cohorti
    }
    # Replace cruz object with this modified version of it
    cruzes[[i]] <- cruzi
  }

  # check out unique cohorts across datasets: (debugging)
  all_cohorts

  #=============================================================================
  # Setting up final cruz object

  # Initiate final cruz object by grabbing the first cruz object in the provided list
  cruzi <- cruzes[[1]]
  cruz <- cruzi

  # Inventory the datum lines in the first cruz object
  das_have <- seg_have <- sit_have <- c()
  sgev_have <- sgsg_have <- sgsit_have <- c()
  # Loop through each cohort
  for(ci in 1:length(cruzi$cohorts)){
    (dasi <- cruzi$cohorts[[ci]]$das$datum_id %>% unique)
    das_have <- c(das_have, dasi)

    (segi <- cruzi$cohorts[[ci]]$segments$datum_id  %>% unique)
    seg_have <- c(seg_have, segi)

    (siti <- cruzi$cohorts[[ci]]$sightings$datum_id %>% unique)
    sit_have <- c(sit_have, siti)

    if('subgroups' %in% names(cruzi$cohorts[[ci]])){
      (sgev <- cruzi$cohorts[[ci]]$subgroups$events$datum_id %>% unique)
      sgev_have <- c(sgev_have, sgev)

      (sgev <- cruzi$cohorts[[ci]]$subgroups$sightings$datum_id %>% unique)
      sgsit_have <- c(sgsit_have, sgev)

      (sgev <- cruzi$cohorts[[ci]]$subgroups$subgroups$datum_id %>% unique)
      sgsg_have <- c(sgsg_have, sgev)
    }
  }

  #=============================================================================

  message('\nCombining multiple cruz objects together ...')
  # Now loop through second and subsequent cruz objects,
  # combining them with the first cruz object according to whether their cohorts
  # already occur in the first.
  i=2
  for(i in 2:length(cruzes)){
    message('--- processing cruz object ',i,' . . . ')
    cruzi <- cruzes[[i]]
    cruzi %>% names

    # Loop through each cohort in this cruz object
    ci=1
    for(ci in 1:length(all_cohorts)){
      (cohorti_name <- all_cohorts[ci])
      (master_index <- which(names(cruz$cohorts)==cohorti_name))
      (cohorti_index <- which(names(cruzi$cohorts)==cohorti_name))
      if(length(cohorti_index)==0){
        # this cohort is not in the new cruz object. No concatenation/redundancy check needed
      }else{

        message('--- --- combining cohort "',cohorti_name,'" ...')
        cohorti <- cruzi$cohorts[[cohorti_index]]

        if(length(master_index)==0){
          # this cohort is not in the master cruz object. Just add it.
          cruz$cohorts[[length(cruz$cohorts)+1]]$new <- cohorti
          names(cruz$cohorts)[which(names(cruz$cohorts)=='new')] <- cohorti_name

        }else{
          # this cohort is already in the master cruz object. Add carefully, checking for redundancy.

          # segments
          newdat <- cohorti$segments
          message('--- --- --- number of segments to add from new cruz object: ')
          message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
          newdat <- newdat %>% dplyr::filter(! datum_id %in% seg_have)
          message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
          if(nrow(newdat)>0){
            # Update master cruz object
            cruz$cohorts[[master_index]]$segments <-
              rbind(cruz$cohorts[[master_index]]$segments, newdat)
            # Update vector of data you already have
            seg_have <- c(seg_have, newdat$datum_id)
          }

          # sightings
          newdat <- cohorti$sightings
          message('--- --- --- number of sightings to add from new cruz object: ')
          message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
          newdat <- newdat %>% dplyr::filter(! datum_id %in% sit_have)
          message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
          if(nrow(newdat)>0){
            # Update master cruz object
            cruz$cohorts[[master_index]]$sightings <-
              rbind(cruz$cohorts[[master_index]]$sightings, newdat)
            # Update vector of data you already have
            sit_have <- c(sit_have, newdat$datum_id)
          }

          # das
          newdat <- cohorti$das
          message('--- --- --- number of DAS rows to add from new cruz object: ')
          message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
          newdat <- newdat %>% dplyr::filter(! datum_id %in% das_have)
          message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
          if(nrow(newdat)>0){
            # Update master cruz object
            cruz$cohorts[[master_index]]$das <-
              rbind(cruz$cohorts[[master_index]]$das, newdat)
            # Update vector of data you already have
            das_have <- c(das_have, newdat$datum_id)
          }

          if('subgroups' %in% names(cohorti)){
            if(all(!is.null(cohorti$subgroups),
                   !is.null(cohorti$sightings),
                   !is.null(cohorti$events))){

              if(! 'subgroups' %in% names(cruz$cohorts[[master_index]])){
                # the master cohort does not have subgroups yet. Just add it.
                cruz$cohorts[[master_index]]$subgroups <- cohorti$subgroups
              }else{
                # the master cohort already has subgroups in this cohort.
                # add carefully, checking for redundancy.
                subs <- cohorti$subgroups

                # subgroup subgroups
                (newdat <- subs$subgroups)
                if(!is.null(newdat)){
                  message('--- --- --- number of subgroups$subgroups rows to add from new cruz object: ')
                  message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
                  newdat <- newdat %>% dplyr::filter(! datum_id %in% sgsg_have)
                  message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
                  if(nrow(newdat)>0){
                    # Update master cruz object
                    cruz$cohorts[[master_index]]$subgroups$subgroups <-
                      rbind(cruz$cohorts[[master_index]]$subgroups$subgroups, newdat)
                    # Update vector of data you already have
                    sgsg_have <- c(sgsg_have, newdat$datum_id)
                  }
                }

                # subgroups sightings
                newdat <- subs$sightings
                if(!is.null(newdat)){
                  message('--- --- --- number of subgroups$sightings rows to add from new cruz object: ')
                  message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
                  newdat <- newdat %>% dplyr::filter(! datum_id %in% sgsit_have)
                  message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
                  if(nrow(newdat)>0){
                    # Update master cruz object
                    cruz$cohorts[[master_index]]$subgroups$sightings <-
                      rbind(cruz$cohorts[[master_index]]$subgroups$sightings, newdat)
                    # Update vector of data you already have
                    sgsit_have <- c(sgsit_have, newdat$datum_id)
                  }
                }

                # subgroups events
                newdat <- subs$events
                if(!is.null(newdat)){
                  message('--- --- --- number of subgroups$events rows to add from new cruz object: ')
                  message('--- --- --- --- (before checking for redundancy: ',nrow(newdat),')')
                  newdat <- newdat %>% dplyr::filter(! datum_id %in% sgev_have)
                  message('--- --- --- --- (after checking for redundancy: ',nrow(newdat),')')
                  if(nrow(newdat)>0){
                    # Update master cruz object
                    cruz$cohorts[[master_index]]$subgroups$events <-
                      rbind(cruz$cohorts[[master_index]]$subgroups$events, newdat)
                    # Update vector of data you already have
                    sgev_have <- c(sgev_have, newdat$datum_id)
                  }
                }

              } # end when master cruz already has subgroups
            } # end when there is actually subgroups data
          } # end when this cohort object has subgroups
        } # end when this cohort is already in the master cruz
      } # end when this cohort is in fact in the new cruz object object
    } # end loop through cohorts
  } # end loop through cruz objects

  # Review new cruz structure - debugging only
  if(FALSE){
    cruz_structure(cruz)
  }

  return(cruz)
}
