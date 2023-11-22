#' Utility function to summarize segments
#'
#' This is an internal function typically not called by a user directly.
#' This function takes a list with segment data and produces a dataframe
#' summarizing each segment.
#'
#' @param segments  A list in which each slot has a chunk of `DAS` survey data corresponding to a single effort segment.
#' @param das  The `DAS` survey data.
#' @param debug_mode  Boolean, with default `FALSE`, indicating whether details should be printed to the Console that facilitate debugging.
#' @param to_plot  Boolean, with default `TRUE`, indicating whether or not histograms showing segment lengths should be produced.
#' @param verbose Boolean, with default `FALSE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A list with two slots: `segments` (summary dataframe, in which each row contains metadata for a single segment),
#' and `das` (modified `DAS` survey data, with new columns `segid` -- segment ID -- and `use` -- a logical indicating
#' whether the event will be included in analysis). This result is passed back to `segmentize()`.
#'
#' @export
#'
segmentize_summarize <- function(segments,
                                 das,
                                 debug_mode= FALSE,
                                 to_plot = TRUE,
                                 verbose=FALSE){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    # This function summarizes a list of segment data.
    # Each slot is DAS data corresponding to a single segment.
    # It is called by segmentize().

    # To debug, run up to line 266 in segmentize.R
  }

  # Rename for convenience
  dass <- das

  # Stage results
  results <- data.frame()
  results_segments <- list()

  if(verbose){pb <- txtProgressBar(1, length(segments), style=3)} # setup progress bar

  # Loop through each segment
  i=1
  for(i in 1:length(segments)){
    effi <- segments[[i]] # get data for this segment
    nrow(effi)
    #print(effi$stratum %>% table)

    if(debug_mode){
      message(i,' | nrow = ', nrow(effi),
              ' | start = ', effi$line_num[1],
              ' | end = ', effi$line_num[nrow(effi)],
              ' | km = ', round(sum(effi$km_int)))
    }

    # As long as this segment isnt empty ...
    if(nrow(effi)>1){

      # Add seg_id column to data
      effi$seg_id <- i

      # Add to results_segments
      results_segments[[i]] <- effi

      # Get rows corresponding to beginning, middle, and end of segment
      if(nrow(effi)==2){
        effb <- effm <- effi[1,]
        effe <- effi[2,]
      }else{
        effb <- effi[1,] # first row
        effm <- effi[round(nrow(effi)/2),] # mid row
        effe <- effi[nrow(effi),] # last row
      }

      # review
      effb %>% as.data.frame %>% head
      effb
      effm
      effe

      effi
      nrow(effi)

      #review
      #plot(effi$km_int)
      #plot(effi$km_int %>% cumsum)

      # Create results dataframe
      # drawing data from beginning / middle / end, or a weighted average of all rows
      resulti <- data.frame(Cruise = effb$Cruise,
                            ship = effb$ship,
                            stratum = effb$stratum,
                            seg_id = i,
                            yday = effb$yday,
                            dist = sum(effi$km_int,na.rm=TRUE),

                            lat1 = effb$Lat,
                            lon1 = effb$Lon,
                            DateTime1 = effb$DateTime,
                            timestamp1 = as.numeric(effb$DateTime),

                            lat2 = effe$Lat,
                            lon2 = effe$Lon,
                            DateTime2 = effe$DateTime,
                            timestamp2 = as.numeric(effe$DateTime),

                            mlat = effm$Lat,
                            mlon = effm$Lon,
                            mDateTime = effm$DateTime,
                            mtimestamp = as.numeric(effb$DateTime),

                            use = effb$use,
                            Mode = effb$Mode,
                            EffType = effb$EffType,
                            OnEffort = effb$OnEffort,
                            ESWsides = effb$ESWsides,

                            year = effb$year,
                            month = effb$month,
                            day = effb$day,

                            min_line = effb$line_num,
                            max_line = effe$line_num,
                            n_rows = nrow(effi),

                            avgBft = stats::weighted.mean(effi$Bft,effi$km_int, na.rm=TRUE), # weighted averages
                            avgSwellHght = stats::weighted.mean(effi$SwellHght,effi$km_int, na.rm=TRUE),
                            avgHorizSun = stats::weighted.mean(effi$HorizSun,effi$km_int, na.rm=TRUE),
                            avgVertSun = stats::weighted.mean(effi$VertSun,effi$km_int, na.rm=TRUE),
                            avgGlare = stats::weighted.mean(effi$Glare,effi$km_int, na.rm=TRUE),
                            avgVis = stats::weighted.mean(effi$Vis,effi$km_int, na.rm=TRUE),
                            avgCourse = stats::weighted.mean(effi$Course,effi$km_int, na.rm=TRUE),
                            avgSpdKt = stats::weighted.mean(effi$SpdKt,effi$km_int, na.rm=TRUE)
      )

      # add segment row to results table
      results <- rbind(results, resulti)
    } # end if if this is more than 1 row
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar
  } # end of segment loop

  ##############################################################################

  # Review
  head(results)
  nrow(results)
  results$stratum %>% table
  results_segments %>% length

  # Print lengths of segments
  if(FALSE){
    x <- results$dist[results$use == TRUE] %>% sort() %>% round()
    message('\n\nUsable segment lengths (n=',length(x),') = ',paste(x,collapse=', '))

    x <- results$dist[results$use == FALSE] %>% sort() %>% round()
    message('\n\nNot-usable segment lengths (n=',length(x),') = ',paste(x,collapse=', '))
  }

  # Plot for review
  if(to_plot){
    par(mfrow=c(2,1)) ; par(mar=c(4.2,4.2,2.5,.5))
    x <- results$dist[results$use == TRUE]
    if(segment_method=='day'){
      hist_title <- paste0('Usable | segment by date')
    }else{
      hist_title <- paste0('Usable | target length = ',segment_target_km,' km')
    }
    if(length(x)==0){plot(1,type='n',axes=FALSE,ann=FALSE)}else{
      hist(x,breaks=seq(-1, 1.1*max(results$dist),by=segment_target_km/30), main=hist_title, xlab='Segment length (km)')
    }

    x <- results$dist[results$use == FALSE]
    if(segment_method=='day'){
      hist_title <- paste0('Not usable | segment by date')
    }else{
      hist_title <- paste0('Not usable | target length = ',segment_target_km,' km')
    }
    if(length(x)==0){plot(1,type='n',axes=FALSE,ann=FALSE)}else{
      hist(x,breaks=seq(-1, 1.1*max(results$dist),by=segment_target_km/30), main=hist_title, xlab='Segment length (km)')
    }
    par(mfrow=c(1,1))
  }

  ##############################################################################
  # Update das

  # Update DAS with seg_id column
  # create a new version of das by binding segments list into a dataframe
  new_das <- dplyr::bind_rows(results_segments)
  nrow(new_das)
  nrow(dass)

  # simplify for joining
  new_das_to_join <- new_das %>% dplyr::select(line_num,seg_id,use)
  new_das_to_join %>% head

  # join to original data
  new_dass <- dplyr::left_join(dass,new_das_to_join,by='line_num')

  # review
  table(new_dass$seg_id, useNA='ifany') %>% sort
  table(new_dass$use, useNA='ifany') %>% sort

  # Return list of results
  return(list(segments = results,
              #effort = results_segments,
              das = new_dass
              ))
}
