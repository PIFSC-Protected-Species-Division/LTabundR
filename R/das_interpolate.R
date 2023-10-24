#' Interpolate DAS data
#'
#' This function is not typically called by the user directly; it is called during `process_surveys()`.
#' The user can instruct `LTabundR` to interpolate `DAS` data in `load_survey_settings()`.
#'
#' @param das  A `data.frame` of a `DAS` survey data file, created by `load_das()`.
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
  } #===========================================================================
  # end debugging

  if(verbose){
    message('Original DAS dataset is ', nrow(das), ' rows')
  }

  if(verbose){message('Interpolating ...')}
  mr <- data.frame()
  i = 66
  for(i in 2:nrow(das)){
    (mr1 <- das[(i-1), ])
    (mr2 <- das[i, ])

    ok <- FALSE
    if(all(c(!is.na(mr1$Cruise), !is.na(mr2$Cruise),
             !is.na(mr1$DateTime), !is.na(mr2$DateTime),
             !is.na(mr1$OnEffort), !is.na(mr2$OnEffort),
             !is.na(mr1$Lon), !is.na(mr2$Lon),
             !is.na(mr1$Lat), !is.na(mr2$Lat)))){
      #message('no NAs')
      if(mr1$Cruise == mr2$Cruise){
        #message('same cruise')
        if(lubridate::date(mr1$DateTime) == lubridate::date(mr2$DateTime)){
          #message('same date')
          if(any(c(mr1$OnEffort, mr2$OnEffort))){
            #meesage('on effort')
            (diffi <- as.numeric(difftime(mr2$DateTime, mr1$DateTime, units = 'secs')))
            if(diffi > new_interval){
              #message('long interval')
              ok <- TRUE
            }
          }
        }
      }
    }
    ok

    if(ok){
      # Interpolate!
      if(verbose){message(' --- Row ', i, ' (',(round(100*(i/nrow(das)), 1)),'%):')}
      if(verbose){message(' --- --- interpolating a time difference of ', diffi,' seconds starting at ', mr1$DateTime)}
      diffi
      #diffi <- 124

      # Calculate fractional steps to take
      (interps <- ceiling(diffi / new_interval))
      (fracs <- (1:interps)/interps)

      # Setup new dataframe
      (mri <- mr1[rep(1, times=(interps)), ])

      # Change events
      mri$Event[2:nrow(mri)] <- 'C'
      mri$Data1[2:nrow(mri)] <- 'Interpolated_position'

      # Change line numbers
      #mri$line_num <- mri$line_num + head(c(0, fracs), interps)

      # Change times
      (time_steps <- head(c(0, round((diffi*fracs))), interps))
      (mri$DateTime <- mri$DateTime[1] + time_steps)
      mr2$DateTime

      # Change longitude
      mr2$Lon ; mr1$Lon
      (lon_diffi <- mr2$Lon - mr1$Lon)
      (lon_steps <- head(c(0, lon_diffi*fracs), interps))
      (mri$Lon <- mri$Lon[1] + lon_steps)
      mr2$Lon

      # Change latitude
      mr2$Lat ; mr1$Lat
      (lat_diffi <- mr2$Lat - mr1$Lat)
      (lat_steps <- head(c(0, lat_diffi*fracs), interps))
      (mri$Lat <- mri$Lat[1] + lat_steps)
      mr2$Lat

      # Result
      if(verbose){message(' --- --- added ', (nrow(mri)-1), ' interpolated row(s).')}
      #mr1 ; mri ; mr2
      mr <- rbind(mr, mri)

    }else{
      # No interpolation
      mr <- rbind(mr, mr1)
    }
  } # for loop

  # Force new integer line numbers
  mr$line_num <- 1:nrow(mr)

  if(verbose){
    message('Interpolation complete. New DAS dataset is ', nrow(mr), ' rows')
  }

  return(mr)
}
