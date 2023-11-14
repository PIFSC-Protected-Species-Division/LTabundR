#' Interpolate DAS data
#'
#' This function is not typically called by the user directly; it is called during `process_surveys()`.
#' The user can instruct `LTabundR` to interpolate `DAS` data in `load_survey_settings()`.
#'
#' param das  A `data.frame` of a `DAS` survey data file, created by `load_das()`.
#' @param new_interval The interpolation interval, in seconds.
#' @param verbose Print updates to the Console?
#'
#' @details This function allows you to interpolate the DAS position data
#' at the onset of processing if your position updates are separated
#' by large time intervals, which would make spatial effort and stratum assignments less exact.
#' `LTabundR` will interpolate the data using simple-linear methods (i.e., no great-circle calculations), such that
#' position updates occur every `new_interval` seconds or less. If adjacent `DAS` rows are from different dates or cruises,
#' the interpolation routine will skip to the next pair of related rows. Interpolation will only occur for On-Effort rows.
#'
#' @return An interpolated `data.frame` of the `DAS` data. No formatting has been changed.
#' @export
#' @import dplyr
#'
das_interpolate <- function(das,
                            new_interval = 120,
                            verbose = FALSE){

  if(FALSE){ #==================================================================
    # debugging
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    verbose = TRUE
    new_interval = 120
    das2 <- das_interpolate(das, verbose=TRUE)
  } #===========================================================================
  # end debugging

  if(verbose){
    message('\nOriginal DAS dataset is ', nrow(das), ' rows')
  }

  if(verbose){message('\nChecking for rows needing interpolation ...')}

  # New approach based on dplyr ===============================================
  mr1 <-
    das %>%
    # get leading values
    mutate(line_numi = line_num,
           this_dt = lubridate::as_datetime(DateTime),
           this_date = lubridate::date(DateTime),
           next_cruise = lead(Cruise),
           next_dt = lubridate::as_datetime(lead(DateTime)),
           next_date = lubridate::date(lead(DateTime)),
           next_eff = lead(OnEffort),
           next_lon = lead(Lon),
           next_lat = lead(Lat)) %>%

    # check to see that all needed values are valid & that effort has not changed
    mutate(interp_valid = ifelse(!is.na(Cruise) &
                                   !is.na(next_cruise) &
                                   !is.na(this_dt) &
                                   !is.na(next_dt) &
                                   !is.na(OnEffort) &
                                   !is.na(next_eff) &
                                   !is.na(Lon) &
                                   !is.na(next_lon) &
                                   !is.na(Lat) &
                                   !is.na(next_lat) &
                                   Cruise == next_cruise &
                                   this_date == next_date &
                                   OnEffort == next_eff,
                                 TRUE,
                                 FALSE)) %>%

    # get time lag
    mutate(lag_dt = as.numeric(difftime(next_dt, this_dt, units='secs'))) %>%

    # decide whether to interpolate the line
    mutate(interp_needed = ifelse(interp_valid &
                                    lag_dt > new_interval,
                                  TRUE,
                                  FALSE))

  #=============================================================================

  # Filter down to rows that need interpolation
  mr_interp <-
    mr1 %>%
    filter(interp_needed == TRUE)

  if(verbose){message('--- Rows needing interpolation, based on specified interval = ', nrow(mr_interp))}

  if(nrow(mr_interp) > 0){
    if(verbose){message('\nInterpolating ...')}

    # Conduct interpolation
    mr_new <- data.frame()
    i=1
    for(i in 1:nrow(mr_interp)){
      (mri <- mr_interp[i,])

      # Calculate fractional steps to take
      (diffi <- mri$lag_dt)
      (interps <- ceiling(diffi / new_interval))
      (fracs <- (1:interps)/interps)

      # Setup new dataframe
      (mrii <- mri[rep(1, times=(interps)), ])

      # Change events
      mrii$Event[2:nrow(mrii)] <- 'C'
      mrii$Data1[2:nrow(mrii)] <- 'Interpolated_position'

      # Change line numbers
      (mrii$line_numi <- mrii$line_numi + head(c(0, fracs), interps))

      # Change times
      (time_steps <- head(c(0, round((diffi*fracs))), interps))
      (mrii$DateTime <- mrii$DateTime[1] + time_steps)

      # Change longitude
      (lon_diffi <- mri$next_lon - mri$Lon)
      (lon_steps <- head(c(0, lon_diffi*fracs), interps))
      (mrii$Lon <- mrii$Lon[1] + lon_steps)
      mri$next_lon

      # Change latitude
      mri$next_lat ; mri$Lat
      (lat_diffi <- mri$next_lat - mri$Lat)
      (lat_steps <- head(c(0, lat_diffi*fracs), interps))
      (mrii$Lat <- mrii$Lat[1] + lat_steps)
      mri$next_lat

      # Result
      if(verbose){message(' --- Line number ', mri$line_num[1], ' (',round(100*(i/nrow(mr_interp))),'%): added ',
                          (nrow(mrii)-1), ' interpolated row(s).')}

      mr_new <- rbind(mr_new, mrii)
    }

    if(verbose){message('\nAdding interpolated rows to survey data...')}
    # get all rows that were not interpolated
    mr2 <-
      mr1 %>%
      filter(interp_needed == FALSE)

    # add interpolated rows
    mr2 <- rbind(mr2, mr_new)

    # arrange to original sequential order and get rid of new columns
    mr2 <-
      mr2 %>%
      arrange(line_numi) %>%
      select(Event:line_num) %>%
      # re-do line_num
      mutate(line_num = 1:n())

    # QA/QC
    if(FALSE){
      qaqc <- mr2 %>% filter(Data1 == 'Interpolated_position') %>% pull(line_num)
      qaqc <- c(qaqc, qaqc - 1, qaqc + 1) %>% sort
      mr2[qaqc,] %>% select(Event:Cruise, EventNum:line_num) %>% View
    }

  }else{
    if(verbose){message('No interpolation performed.')}
    mr2 <- mr1
  } # end of if there are rows needing interpolation

  #=============================================================================

  if(verbose){
    message('Interpolation complete. New DAS dataset is ', nrow(mr2), ' rows')
  }

  return(mr2)
}


#===============================================================================
#===============================================================================
#
#   mr <- data.frame()
#   i = 66
#   for(i in 2:nrow(das)){
#     (mr1 <- das[(i-1), ])
#     (mr2 <- das[i, ])
#
#     ok <- FALSE
#     if(all(c(!is.na(mr1$Cruise), !is.na(mr2$Cruise),
#              !is.na(mr1$DateTime), !is.na(mr2$DateTime),
#              !is.na(mr1$OnEffort), !is.na(mr2$OnEffort),
#              !is.na(mr1$Lon), !is.na(mr2$Lon),
#              !is.na(mr1$Lat), !is.na(mr2$Lat)))){
#       #message('no NAs')
#       if(mr1$Cruise == mr2$Cruise){
#         #message('same cruise')
#         if(lubridate::date(mr1$DateTime) == lubridate::date(mr2$DateTime)){
#           #message('same date')
#           if(any(c(mr1$OnEffort, mr2$OnEffort))){
#             #meesage('on effort')
#             (diffi <- as.numeric(difftime(mr2$DateTime, mr1$DateTime, units = 'secs')))
#             if(diffi > new_interval){
#               #message('long interval')
#               ok <- TRUE
#             }
#           }
#         }
#       }
#     }
#     ok
#
#     if(ok){
#       # Interpolate!
#       if(verbose){message(' --- Row ', i, ' (',(round(100*(i/nrow(das)), 1)),'%):')}
#       if(verbose){message(' --- --- interpolating a time difference of ', diffi,' seconds starting at ', mr1$DateTime)}
#       diffi
#       #diffi <- 124
#
#       # Calculate fractional steps to take
#       (interps <- ceiling(diffi / new_interval))
#       (fracs <- (1:interps)/interps)
#
#       # Setup new dataframe
#       (mri <- mr1[rep(1, times=(interps)), ])
#
#       # Change events
#       mri$Event[2:nrow(mri)] <- 'C'
#       mri$Data1[2:nrow(mri)] <- 'Interpolated_position'
#
#       # Change line numbers
#       #mri$line_num <- mri$line_num + head(c(0, fracs), interps)
#
#       # Change times
#       (time_steps <- head(c(0, round((diffi*fracs))), interps))
#       (mri$DateTime <- mri$DateTime[1] + time_steps)
#       mr2$DateTime
#
#       # Change longitude
#       mr2$Lon ; mr1$Lon
#       (lon_diffi <- mr2$Lon - mr1$Lon)
#       (lon_steps <- head(c(0, lon_diffi*fracs), interps))
#       (mri$Lon <- mri$Lon[1] + lon_steps)
#       mr2$Lon
#
#       # Change latitude
#       mr2$Lat ; mr1$Lat
#       (lat_diffi <- mr2$Lat - mr1$Lat)
#       (lat_steps <- head(c(0, lat_diffi*fracs), interps))
#       (mri$Lat <- mri$Lat[1] + lat_steps)
#       mr2$Lat
#
#       # Result
#       if(verbose){message(' --- --- added ', (nrow(mri)-1), ' interpolated row(s).')}
#       #mr1 ; mri ; mr2
#       mr <- rbind(mr, mri)
#
#     }else{
#       # No interpolation
#       mr <- rbind(mr, mr1)
#     }
#   } # for loop
#
#   # Force new integer line numbers
#   mr$line_num <- 1:nrow(mr)
#
#   if(verbose){
#     message('Interpolation complete. New DAS dataset is ', nrow(mr), ' rows')
#   }
#
#   return(mr)
# }
