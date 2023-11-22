#' Core utility function for segmentizing effort
#'
#' This is an internal function typically not called by a user directly.
#' It is called within `segmentize_wrapper()`, which itself is a subroutine
#' called within `segmentize()`.
#' This function segmentizes the data within a single "effort bloc", returning a list of segments.
#'
#' @param dasspliti  An "effort bloc" of survey data corresponding to a single effort scenario
#' (year, cruise number, stratum, effort type, effort mode, etc.).
#' @param segment_method See documentation for `segmentize()`.
#' @param segment_target_km See documentation for `segmentize()`.
#' @param segment_max_interval See documentation for `segmentize()`.
#' @param segment_remainder_handling See documentation for `segmentize()`.
#' @param debug_mode  Boolean indicating whether or not details should be printed to the Console that facilitate debugging.
#' @param verbose Boolean, with default `FALSE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A list in which each slot is a chunk of survey effort corresponding to a single segment.
#'
#' @export
#'
segmentize_core <- function(dasspliti,
                            segment_method,
                            segment_target_km,
                            segment_max_interval,
                            segment_remainder_handling,
                            debug_mode,
                            verbose){

  if(FALSE){ # For debugging only -- not run! ==================================
    # Run code in segmentize_wrapper first, up to the dasspliti stage
    verbose=TRUE
    debug_mode = TRUE
    segment_remainder_handling <- c('append', 'segment')
    segment_method <- 'day'
    segment_method <- 'equallength'
    segment_max_interval <- 24 * 3600 #48 # hours
    segment_target_km <- 30
  } #===========================================================================

  # This function takes a set of DAS data representing a unique effort scenario
  # it is called by segmentize_wrapper() in `utils_segmentize_wrapper.R`,
  # which itself is called by segmentize().

  # This function does the actual segmentizing: by day or by equallength

  # stage results
  segments <- list()

  # save a safe copy of the DAS data and make sure it is arranged by line number
  dassproc <- dasspliti %>%
    arrange(line_num)

  # Review
  #print(table(dassproc$stratum))
  #print(nrow(dassproc))
  #print(segment_method)

  # Segment by day =============================================================

  if(segment_method == 'day'){
    days <- unique(dassproc$yday) # get unique dates in data
    days
    for(day_i in 1:length(days)){ # loop through each date
      #day_i <- 1
      dayi <- days[day_i] # get date
      das_day <- dassproc %>% dplyr::filter(yday == dayi) # filter by that date
      segments[[length(segments) + 1]] <-das_day # add date's data as a slot in your segments list
    }
  }

  # Segment by equallength =====================================================

  if(segment_method == 'equallength'){
    # First find where logistical breaks need to occur due to time gaps
    # ie, determine splits necessary due to `segment_max_interval`
    dt <- dassproc$DateTime # get date-times
    times <- data.frame(t1 = dt[1:(length(dt)-1)], # make simple dataframe. Each row has a time and the next row's time.
                        t2 = dt[2:length(dt)])
    time_int <- apply(times,1,function(x){difftime(x[2],x[1],units='secs')}) # Calculate time gap in each row
    time_int <- c(time_int,0) # add 0 to the end of that vector, since we can't calculate gap for final row
    dtime_cum <- cumsum(time_int) # cumulative sum of time gaps
    range(time_int) # review
    segment_max_interval # remind myself of setting

    # Stage a vector for the time group for each row.
    split_group <- rep(1, times=nrow(dassproc)) # assume there will be no splits
    breaks_raw <- which(time_int >= segment_max_interval) ; breaks_raw # see if there are going to be any time splits
    if(length(breaks_raw)>0){ # if so....
      breaks <- dtime_cum[breaks_raw-1] ; breaks # get the cumulative time of the rows where the breaks happen

      # plot for review
      # plot(dassproc$km_cum,type='l') ; abline(h=dassproc$km_cum[breaks_raw], col='steelblue3')

      # assign time group ID according to the cumulative time
      split_group <- sapply(dtime_cum, function(x){length(which(breaks < x))}) + 1
    }
    #print(table(split_group)) # review/debug
    dassproc$seg_split <- factor(split_group)     # Add time split ID as column in DAS

    # Now loop through each of those time splits and break up the data within according to equal length
    splits <- unique(dassproc$seg_split) # time split IDs
    splits <- splits[!is.na(splits)] # make sure none is NA
    if(length(splits)>0){ # as long as there is one or more splits
      debug_tracker <- data.frame() # for debugging

      for(split_i in 1:length(splits)){ # loop through each split
        #split_i <- 3
        das_split <- dassproc %>% filter(seg_split == splits[split_i]) # get the DAS data for this split

        #plot(das_split$km_cum,type='l') # debug

        # Handle rows with large KM gaps between it and the next row
        gaps <- which(das_split$km_int > 20)
        #gaps # debug
        #hist(das_split$km_int) # debug
        if(length(gaps)>0){
          #message('KM gap at split ',split_i,': ',paste(round(das_split$km_int[gaps]),collapse=',')) # debug
          # if this data split is more than one row, just change the rows with large gaps to dist of 0
          #das_split$km_int[gaps] <- 0
        }

        # Determine KM of effort remaining if these data were to be split evenly
        tot_km <- das_split$km_int %>% sum
        cum_km <- das_split$km_int %>% cumsum
        n_segments <- floor(tot_km / segment_target_km)
        remainder <- tot_km - n_segments*segment_target_km

        # Based on length of remainder, determine how to handle it
        # segment_remainder_handling can be
        # length 1 (remainder length doesnt matter, same method will be used in any case)
        # or 2 (element 1 pertains to if remainder length is <= half of segment length;
        # element 2 pertains to if remainder length is more than half)
        # accepted values in that vector are:
           # disperse = adjust target_km to avoid remainders
           # append = randomly tack onto another segment
           # segment = treat remainder as its own segment; randomly place it

        # decide remainder handling based on size of remainder relative to target length
        if(remainder < (segment_target_km/2)){
          remainder_handling <- segment_remainder_handling[1]
        }else{
          remainder_handling <- segment_remainder_handling[length(segment_remainder_handling)]
        }

        # Based on handling method, determine location of segment breaks

        # Disperse
        if(remainder_handling == 'disperse'){
          km_breaks <- seq(0,tot_km,length = n_segments)
          if(length(km_breaks)>1){km_breaks <- km_breaks[1:(length(km_breaks)-1)]}
          if(length(km_breaks)>1){km_breaks <- km_breaks[2:(length(km_breaks))]}
        }

        # Stand alone segment
        if(remainder_handling == 'segment'){
          if(n_segments == 0){
            km_breaks <- 0
          }else{
            # Stage a vector of km breaks
            # each break is a multiple of target length
            # make this vector one longer than the number of full-length segments, to make a placeholder for the remainder
            km_breaks_raw <- rep(segment_target_km,times=n_segments + 1)
            km_breaks_raw

            # Replace one of these breaks with the remainder
            remainder_i <- sample(1:length(km_breaks_raw),1) # select random break
            km_breaks_raw[remainder_i] <- remainder # replace that break km with the remainder
            km_breaks_raw

            # Convert to cumulative sum
            km_breaks <- km_breaks_raw %>% cumsum
            km_breaks
            if(length(km_breaks)>1){km_breaks <- km_breaks[1:(length(km_breaks)-1)]}
          }
        }

        # Append -- similar approach to segment, just no additional break to placehold for remainder
        if(remainder_handling == 'append'){
          if(n_segments == 0){
            km_breaks <- 0
          }else{
            # Stage a vector of km breaks
            km_breaks_raw <- rep(segment_target_km,times=n_segments)
            km_breaks_raw

            # Replace one of these with the remainder
            remainder_i <- sample(1:length(km_breaks_raw),1)
            km_breaks_raw[remainder_i] <- km_breaks_raw[remainder_i] + remainder
            km_breaks_raw

            # Convert to cumulative sum
            km_breaks <- km_breaks_raw %>% cumsum
            km_breaks
            if(length(km_breaks)>1){km_breaks <- km_breaks[1:(length(km_breaks)-1)]}
          }
        }

        # if this chunk of data will only be one segment, change km_breaks to 0
        if(length(km_breaks)==0){km_breaks <- c(0)}

        # Implement segment breaks ===============

        # review
        cum_km %>% head
        km_breaks %>% head

        cum_km %>% tail
        km_breaks %>% tail

        # split up data according to cumulative km and km_breaks vector
        split_group <- sapply(cum_km, function(x){length(which(km_breaks <= x))})
        if(0 %in% split_group){split_group <- split_group + 1}
        table(split_group)
        das_split$seg_split <- factor(split_group)
        das_list <- split(das_split, das_split$seg_split)
        length(das_list)

        # Add each segment break as a new slot in the segments list
        list_i=1
        for(list_i in 1:length(das_list)){
          segments[[length(segments)+1]] <-das_list[[list_i]]

          # debugging code
          km_list_i <- das_list[[list_i]]$km_int %>% sum
          if(debug_mode){km_list_i %>% print}
          debug_i <- data.frame(split_i, list_i, km_list_i)
          debug_tracker <- rbind(debug_tracker, debug_i)

          if(verbose){message('--- --- segment created: ',round(km_list_i,1),' km ...')}
        } # end of adding segments to results list
      } # end of time split loop
    } # if time splits is more than 0
  } # segment by equallength

  # review
  length(segments)
  #which(unlist(lapply(segments,function(x){sum(x[,49])})) > 155)
  lapply(segments,nrow) %>% unlist
  #segments[[1]] %>% glimpse

  return(segments)
}
