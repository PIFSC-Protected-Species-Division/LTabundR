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
#' @param speed_filler When speed is not available in the data, this value (in kmh) will be used as a filler in order to estimate the
#' distance between consecutive rows of data based on timestamp differences (when lat/long coordinates are not available).
#' @param km_filler When valid speed and position information is not available (e.g., the given distance exceeds `max_km_gap`), this value (in km) will be used as an estimate of the
#' distance in between consecutive rows of data.
#' @param debug_mode  Boolean, with default `FALSE`, indicating whether details should be printed to the Console that facilitate debugging.
#'
#' @return A numeric vector, the same length as the number of rows in `das`, of distances (in km).
#' The final element of this vector will be 0.
#' @import dplyr
#' @export
#'
process_km <- function(das,
                       min_interval = 5,
                       max_interval = 1800,
                       replacement_interval = 1800,
                       max_km_gap = 10,
                       km_filler = 1,
                       speed_filler = 10*1.852,
                       debug_mode = TRUE){

  if(FALSE){ # for debugging -- not run! =======================================
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    min_interval = 10
    max_interval = 900
    replacement_interval = 900
    km_filler = 10
    speed_filler = 18
    max_km_replace = 2
    debug_mode = TRUE
  } # end debugging! ===========================================================

  # rename for convenience
  dass <- das

  # Add the column year if it does not exist yet
  if(! 'year' %in% names(dass)){dass$year <- lubridate::year(dass$DateTime)}

  # make a small dataframe with each lat/long and its subsequent lat/long on the same row
  pos <-
    dass %>%
    transmute(lat1 = Lat,
              lon1 = Lon,
              lat2 = lead(Lat),
              lon2 = lead(Lon),
              knots = SpdKt,
              year = year,
              dt1 = DateTime,
              dt2 = lead(DateTime)) %>%
    mutate(time_delta = as.numeric(dt2) - as.numeric(dt1),
           # Stage defaults
           method = 'pre',
           km_valid = FALSE) %>%
    # Checks
    rowwise() %>%
    mutate(latlon_ok = ifelse(all(!is.na(c(lat1, lon1, lat2, lon2))), TRUE, FALSE)) %>%
    mutate(spd_ok = ifelse(!is.na(knots), TRUE, FALSE)) %>%
    mutate(delta_ok = ifelse(!is.na(time_delta), TRUE, FALSE)) %>%
    # Initial adjustments
    mutate(knots = ifelse(spd_ok == TRUE, knots, speed_filler)) %>%
    mutate(delta_use = ifelse(delta_ok == TRUE, time_delta, 0)) %>%
    # More checks
    mutate(min_ok = ifelse(delta_use >= min_interval, TRUE, FALSE)) %>%
    mutate(max_ok = ifelse(delta_use <= max_interval, TRUE, FALSE)) %>%
    mutate(delta_use = ifelse(max_ok == TRUE, delta_use, replacement_interval)) %>%
    # More adjustments
    mutate(km_valid = ifelse(all(c(latlon_ok, spd_ok, delta_ok, max_ok)), TRUE, km_valid)) %>%
    mutate(method = ifelse(year > 1991 & latlon_ok, 'post', method)) %>%
    # Now calculate distance
    mutate(km = ifelse(min_ok,
                       ifelse(method == 'post',
                              swfscDAS::distance_greatcircle(lat1, lon1, lat2, lon2),
                              knots*(delta_use / 3600)*1.852),
                       0)) %>%
    # Check km max
    mutate(km_valid = ifelse(km > max_km_gap, FALSE, km_valid)) %>%
    mutate(km = ifelse(km > max_km_gap, km_filler, km)) %>%
    ungroup() %>%
    select(-year, -dt1, -dt2)

  pos %>% head(20)

  if(debug_mode){

    hist(pos$km, breaks=50, main='Full range')
    hist(pos$km[pos$km < 2], breaks=50, main='Zoom')

    pos$latlon_ok %>% table
    pos$delta_ok %>% table
    pos$max_ok %>% table
    pos$spd_ok %>% table
    pos$knots %>% table(useNA='ifany')
    pos$min_ok %>% table
    pos$km_valid %>% table

    dass$SpdKt %>% table(useNA='ifany')
  }
  # Old way
  # if(FALSE){
  #   pos <- data.frame(lat1 = dass$Lon[1:(nrow(dass)-1)],
  #                   lon1 = dass$Lat[1:(nrow(dass)-1)],
  #                   lat2 = dass$Lon[2:(nrow(dass))],
  #                   lon2 = dass$Lat[2:(nrow(dass))],
  #                   time_delta = as.numeric(difftime(dass$DateTime[2:nrow(dass)],
  #                                                    dass$DateTime[1:(nrow(dass)-1)])),
  #                   knots = dass$SpdKt[1:(nrow(dass)-1)],
  #                   year = dass$year[1:(nrow(dass)-1)])
  # head(pos)
  # nrow(pos)
  # #hist(pos$time_delta[pos$time_delta < 1000], breaks=seq(0,1000,by=10)) # debugging
  #
  # # QA/QC of reported speed
  # #tstart <- Sys.time()
  #
  # (x <- pos[2, ] %>% as.numeric)
  # km <- apply(pos,1,function(x){
  #   # Stage result default
  #   d = 0
  #   d_valid = FALSE
  #
  #   # Test for data quality
  #   if(!is.na(x[1]) & !is.na(x[3])){
  #     if(x[1] == x[3]){x[3] <- NA} # if latitudes are equivalent, coerce the function to return d = 0
  #   }
  #   if(!is.na(x[2]) & !is.na(x[4])){
  #     if(x[2] == x[4]){x[4] <- NA} # same for longitudes
  #   }
  #
  #   if(all(!is.na(x))){
  #     (min_check <- x[5] >= min_interval)
  #     if(min_check){
  #       (max_check <- x[5] <= max_interval)
  #       if(!max_check){
  #         x[5] <- replacement_interval
  #       }
  #       (year_check <- !is.na(x[7]) && x[7] > 1990)
  #
  #       # Calculate based on speed instead
  #       if(!year_check | !max_check){
  #         d <- x[6] * 1.852 * (x[5]/3600)
  #         if(max_check){ d_valid <- TRUE }
  #       }else{
  #         # Proper calculation of distance
  #         d <- swfscDAS::distance_greatcircle(x[1], x[2], x[3], x[4])
  #         (km_check <- d <= max_km_gap)
  #         if(!km_check){
  #           d <- x[6] * 1.852 * (x[5]/3600)
  #         }else{
  #           d_valid <- TRUE
  #         }
  #       }
  #
  #     } # end of no data missing
  #   }else{
  #     # Some data are misisng
  #     if(is.na(x[5])){x[5] <- replacement_interval} # interval
  #     if(is.na(x[6])){x[6] <- 10} # knots
  #     d <- x[6] * 1.852 * (x[5]/3600)
  #   }
  #
  #   # See if d is within max_km_gap
  #   (km_check <- d <= max_km_gap)
  #   if(!km_check){
  #     d <- max_km_replace
  #     d_valid <- FALSE
  #   }
  #   return(data.frame(d, d_valid))
  # })
  #
  # km <- rbind(km, data.frame(km=NA, d_valid=FALSE))
  # km %>% head
  #
  # # Find which gaps are larger than allowed
  # bads <- which(km > max_km_gap)
  # # Fix those large gaps
  # km_fixed <- km
  # if(length(bads)>0){
  #   km_fixed[bads] <- max_km_replace
  # }
  # }

  # Debugging information ======================================================

  if(debug_mode){
    # par(mfrow=c(2,2))
    # hist(pos$delta_use[pos$delta_use < 1000],
    # #hist(pos$time_delta[pos$time_delta < 1000],
    #      breaks=seq(0,1000,by=1), xlim=c(-1,1000),
    #      main='Time gaps', xlab='Seconds between each row of data')
    #
    # hist(pos$knots, main='Speed (knots)', xlab='Knots')
    #
    # hist(pos$km,
    #      breaks=seq(-1,1.1*max(pos$km),length = 50),
    #      main='KM gaps', xlab='KM between each row of data')
    #
    # hist(pos$km[pos$km < 2],
    #      breaks=seq(-1,2,length = 50),
    #      main='KM gaps (zoom)', xlab='KM between each row of data')
    #
    # par(mfrow=c(1,1))
    #
    # #print('Years in data:')
    # #print(table(pos$year,useNA='ifany'))
    #
    # message('Max km gap = ',round(max(pos$km),2))
    #
    # pos$km_valid %>% table

  #   dass$km <- km_fixed # c(km,0)
  #   dass <- dass %>% dplyr::filter(Lon <= -120)
  #   message('Total KM (raw): ', round(sum(km)))
  #   message('Total KM (after removing large gaps): ', round(sum(km_fixed)))
  #   message('KM with EffType = S, OnEffort = TRUE,  Bft <= 6 : ',
  #           dass %>% dplyr::filter(EffType == 'S',OnEffort == TRUE, Bft <= 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #   message('KM with EffType = N, OnEffort = TRUE,  Bft <= 6 : ',
  #           dass %>% dplyr::filter(EffType == 'N',OnEffort == TRUE, Bft <= 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #
  #   message('KM with EffType = S, OnEffort = FALSE, Bft <= 6 : ',
  #           dass %>% dplyr::filter(EffType == 'S',OnEffort == FALSE, Bft <= 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #   message('KM with EffType = N, OnEffort = FALSE, Bft <= 6 : ',
  #           dass %>% dplyr::filter(EffType == 'N',OnEffort == FALSE, Bft <= 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #
  #   message('KM with EffType = S, OnEffort = TRUE,  Bft  > 6 : ',
  #           dass %>% dplyr::filter(EffType == 'S',OnEffort == TRUE, Bft > 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #   message('KM with EffType = N, OnEffort = TRUE,  Bft  > 6 : ',
  #           dass %>% dplyr::filter(EffType == 'N',OnEffort == TRUE, Bft > 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #
  #   message('KM with EffType = S, OnEffort = FALSE, Bft  > 6 : ',
  #           dass %>% dplyr::filter(EffType == 'S',OnEffort == FALSE, Bft > 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  #   message('KM with EffType = N, OnEffort = FALSE, Bft  > 6 : ',
  #           dass %>% dplyr::filter(EffType == 'N',OnEffort == FALSE, Bft > 6) %>%
  #             summarize(km = sum(km)) %>% as.numeric %>% round)
  }

  #=============================================================================

  km_fixed <- pos %>% select(km, km_valid)
  return(km_fixed)
}
