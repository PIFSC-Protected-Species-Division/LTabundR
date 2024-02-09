#' Calculate distances between rows in a DAS file
#'
#' This function is the near-equivalent of the `DISTRAV` subroutine in `ABUND9`.
#' This is an internal function typically not called by a user directly.
#' It is called in `format_das()`.
#'
#' @param das  A dataframe of `DAS` survey data.
#' @param min_interval  Minimum seconds between rows before distance will be calculated.
#' Default is 30 seconds; if the interval is less than this value, the distance will be assumed to be 0 km.
#' @param max_interval The maximum time interval, in seconds, between rows before assuming that
#' there has been a break in survey data logging. This time interval will be replaced with the value specified in `replacement_interval`.
#' @param replacement_interval The time interval, in second, to use as a replacement
#' when the time interval between rows exceeds `max_interval`. The default is 900 seconds.
#' or the `max_interval` specified, whichever is smaller.
#' @param max_km_gap Another way of avoiding long gaps in data; this is the maximum distance gap, in km, allowed between rows of data.
#' This constraint is applied *after* the `interval` constraints above.
#' @param max_km_replace The distance, in km, to use when the distance between rows exceeds `max_km_gap`.
#' @param debug_mode  Boolean, with default `FALSE`, indicating whether details should be printed to the Console that facilitate debugging.
#'
#' @return A numeric vector, the same length as the number of rows in `das`, of distances (in km).
#' The final element of this vector will be 0.
#'
#' @export
#'
process_km <- function(das,
                       min_interval = 30,
                       max_interval = 900,
                       replacement_interval = min(c(900, max_interval)),
                       max_km_gap = 30,
                       max_km_replace = 0,
                       debug_mode = TRUE){

  if(FALSE){ # for debugging -- not run! =======================================
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- load_das(das_file)
    min_interval = 30
    max_interval = 900
    replacement_interval = 900
    max_km_gap = 30
    max_km_replace = 0
    debug_mode = TRUE
  } # end debugging! ===========================================================

  # rename for convenience
  dass <- das

  # Add the column year if it does not exist yet
  if(! 'year' %in% names(dass)){dass$year <- lubridate::year(dass$DateTime)}

  # make a small dataframe with each lat/long and its subsequent lat/long on the same row
  pos <- data.frame(lat1 = dass$Lon[1:(nrow(dass)-1)],
                    lon1 = dass$Lat[1:(nrow(dass)-1)],
                    lat2 = dass$Lon[2:(nrow(dass))],
                    lon2 = dass$Lat[2:(nrow(dass))],
                    time_delta = as.numeric(difftime(dass$DateTime[2:nrow(dass)],
                                                     dass$DateTime[1:(nrow(dass)-1)])),
                    knots = dass$SpdKt[1:(nrow(dass)-1)],
                    year = dass$year[1:(nrow(dass)-1)])
  head(pos)
  nrow(pos)
  #hist(pos$time_delta[pos$time_delta < 1000], breaks=seq(0,1000,by=10)) # debugging

  # QA/QC of reported speeds
  #tstart <- Sys.time()
  km <- apply(pos,1,function(x){
    d = 0
    na_check <- !is.na(x)
    if(is.na(x[5])){x[5] <- 0} # knots
    if(is.na(x[6])){x[6] <- 10} # knots
    if(is.na(x[6])){x[6] <- 10} # knots
    if(is.na(x[7])){x[7] <- 1990} # year

    # if there are no missing data
    if(all(na_check)){

      if(x[7] >= 1991){ # years 1991 or later
        if(x[5] > min_interval){ # only calculate distance if time gap between the adjacent rows is more than 30 seconds
          if(x[5] < max_interval){
            # only calculate distance if time gap is less than max_row_interval
            if(x[1] == x[3]){x[3] <- NA} # if latitudes are equivalent, coerce the function to return d = 0
            if(x[2] == x[4]){x[4] <- NA} # same for longitudes
            d <- swfscDAS::distance_greatcircle(x[1], x[2], x[3], x[4])
          }else{
            # if speed is valid, use that to calculate distance for the duration of the replacement interval
            if(is.finite(x[6])){
              d <- x[6] * 1.852 * (replacement_interval/3600)
            }
          }
        }
      }else{
        # Years prior to 1991
        if(x[5] > min_interval){ # only calculate distance if time gap between the adjacent rows is more than 30 seconds
          # if speed is valid, use that to calculate distance given time gap
          if(is.finite(x[6])){
            d <- x[6] * 1.852 * (x[5]/3600)
          }
        }
      }
    }
    return(d)
  })
  km <- c(km, 0)

  # Find which gaps are larger than allowed
  bads <- which(km > max_km_gap)
  # Fix those large gaps
  km_fixed <- km
  if(length(bads)>0){
    km_fixed[bads] <- max_km_replace
  }

  # Debugging information ======================================================

  if(debug_mode){
    par(mfrow=c(2,2))
    hist(pos$time_delta[pos$time_delta < 1000],
         breaks=seq(0,1000,by=1), xlim=c(-1,1000),
         main='Time gaps', xlab='Seconds between each row of data')

    hist(pos$knots, main='Speed (knots)', xlab='Knots')

    hist(km,
         breaks=seq(-1,1.1*max(km),by=(diff(range(km))/30)),
         main='KM gaps', xlab='KM between each row of data')

    hist(km[km < 30],
         breaks=seq(-1,30,by=1),
         main='KM gaps (zoom)', xlab='KM between each row of data')

    par(mfrow=c(1,1))

    print('Years in data:')
    print(table(pos$year,useNA='ifany'))

    message('Max km gap = ',round(max(km),2))
    message('Number of km gaps over max allowed = ',length(bads))

    dass$km <- km_fixed # c(km,0)
    dass <- dass %>% dplyr::filter(Lon <= -120)
    message('Total KM (raw): ', round(sum(km)))
    message('Total KM (after removing large gaps): ', round(sum(km_fixed)))
    message('KM with EffType = S, OnEffort = TRUE,  Bft <= 6 : ',
            dass %>% dplyr::filter(EffType == 'S',OnEffort == TRUE, Bft <= 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)
    message('KM with EffType = N, OnEffort = TRUE,  Bft <= 6 : ',
            dass %>% dplyr::filter(EffType == 'N',OnEffort == TRUE, Bft <= 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)

    message('KM with EffType = S, OnEffort = FALSE, Bft <= 6 : ',
            dass %>% dplyr::filter(EffType == 'S',OnEffort == FALSE, Bft <= 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)
    message('KM with EffType = N, OnEffort = FALSE, Bft <= 6 : ',
            dass %>% dplyr::filter(EffType == 'N',OnEffort == FALSE, Bft <= 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)

    message('KM with EffType = S, OnEffort = TRUE,  Bft  > 6 : ',
            dass %>% dplyr::filter(EffType == 'S',OnEffort == TRUE, Bft > 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)
    message('KM with EffType = N, OnEffort = TRUE,  Bft  > 6 : ',
            dass %>% dplyr::filter(EffType == 'N',OnEffort == TRUE, Bft > 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)

    message('KM with EffType = S, OnEffort = FALSE, Bft  > 6 : ',
            dass %>% dplyr::filter(EffType == 'S',OnEffort == FALSE, Bft > 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)
    message('KM with EffType = N, OnEffort = FALSE, Bft  > 6 : ',
            dass %>% dplyr::filter(EffType == 'N',OnEffort == FALSE, Bft > 6) %>%
              summarize(km = sum(km)) %>% as.numeric %>% round)
  }

  #=============================================================================

  return(km_fixed)
}
