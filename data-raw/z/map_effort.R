#' Add tracklines to a survey map
#'
#' @param m Your `tmap` map object (produced from `map_base()` or any of the other `LTabundR` mapping functions).
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param use_type Segment tracks to display, based on the `use` column within `cruz$cohorts$<cohort>$<analysis>$segments`, and provided
#' as a boolean vector (accepted values are `TRUE` and/or `FALSE`).
#' @param effort_color Color of tracklines.
#' Before using this value, the function will first look for a `col` column in
#' `cruz$cohorts$<cohort>$segments`.
#' If that is not found, the color provided here will be used.
#' @param effort_stroke Line width of tracklines.
#' Before using this value, the function will first look for a `lwd` column in
#' `cruz$cohorts$<cohort>$segments`.
#' If that is not found, the stroke value provided here will be used.
#' @param effort_linetype Line type (classic graphical parameter `lty` values).
#' Before using this value, the function will first look for a `lty` column in
#' `cruz$cohorts$<cohort>$segments`.
#' If that is not found, the value provided here will be used.
#' @param max_km_gap Maximum allowable gap between events, before breaking a continuous printed line.
#' @param verbose Boolean indicating whether or not to print status updates to the Console.
#'
#' @return A `tmap` map with survey tracklines shown (the map is built using the package `tmap`), which will print as a plot if not saved as an object.
#' If saved as an object, other features can be added to the map (see `map_strata()`, and `map_sightings()`,
#' or other `tmap` functions).
#' @export
#'
map_effort <- function(m,
                       cruz,
                       cohort=1,
                       use_type = c(TRUE),
                       effort_color='steelblue3',
                       effort_stroke=1.5,
                       effort_linetype=1,
                       max_km_gap = NULL,
                       verbose=FALSE){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    m <- map_base()
    data(example_cruz)
    cruz <- example_cruz
    cohort <- 1
    use_type <- c(TRUE)
    effort_color = 'steelblue3'
    effort_stroke = 1.5
    effort_linetype =1
    max_km_gap <- 5
    verbose=TRUE

    # test
    map_effort(m,cruz)
  }
  #=============================================================================

  # Get the segments from the cohort-analysis specified by inputs
  survey <- cruz$cohorts[[cohort]]
  names(survey)

  # For mapping, transform das into a list with a slot for each segment
  map_segments <-
    survey$das %>%
    dplyr::group_by(seg_id) %>%
    dplyr::group_split()

  # Save segment summary as its own object
  effsum <- survey$segments

  # review data
  length(map_segments)
  nrow(effsum)

  #for(i in 1:length(map_segments)){
  #  map_segments[[i]]$use %>% table %>% print
  #}

  # Harvest the max_km_gap setting from input or cruz$settings
  # This value specifies max gap allowed between data points for mapping
  if(is.null(max_km_gap)){
    max_km_gap <- 5 # cruz$settings$survey$segment_max_km_gap
  }

  # Add each map segment to the map
  if(verbose){message('Adding segments to map ...')}
  if(verbose){pb <- txtProgressBar(1, length(map_segments), style=3)} # setup progress bar
  rows_processed <- 0 # begin counter for rows processed

  # Loop through each segment
  i=250 # for debugging only
  for(i in 1:length(map_segments)){
    #print(i)
    effi <- map_segments[[i]] # subset to data for this segment
    effi
    rows_processed <- rows_processed + 1 # add to counter

    # If this list slot is not NULL...
    if(!is.null(effi)){

      names(effi)
      effi <- effi %>% filter(use %in% use_type) # filter to data matching use criteria in input

      # If there are still rows in the data...
      if(nrow(effi)>0){
        # Get the row from the segment summary that matches this segment id
        summ_match <- which(effsum$seg_id == unique(effi$seg_id))

        # Add formatting variables
        # color
        if(length(summ_match)>0 & 'col' %in% names(effsum)){
          effi$col <- effsum$col[summ_match]
        }else{
          effi$col <- effort_color
        }

        # line width / stroke
        if(length(summ_match)>0 & 'lwd' %in% names(effsum)){
          effi$lwd <- effsum$lwd[summ_match]
        }else{
          effi$lwd <- effort_stroke
        }

        # line type
        if(length(summ_match)>0 & 'lty' %in% names(effsum)){
          effi$col <- effsum$lty[summ_match]
        }else{
          effi$lty <- effort_linetype
        }

        # Look for breaks in effort
        # (you don't want the line to span between long breaks)
        # set up dataframe with current position and next position on same row
        pos <- data.frame(lat1 = effi$Lon[1:(nrow(effi)-1)],
                          lon1 = effi$Lat[1:(nrow(effi)-1)],
                          lat2 = effi$Lon[2:(nrow(effi))],
                          lon2 = effi$Lat[2:(nrow(effi))])
        nrow(pos)
        # Determine distane between the two coordinates in each row
        km <- apply(pos,1,function(x){swfscMisc::distance(x[1],
                                                          x[2],
                                                          x[3],
                                                          x[4],
                                                          units='km',
                                                          method = "lawofcosines")})
        km_int <- c(km,0) ; km_int
        km_cum <- cumsum(km_int) ; km_cum
        #message(i,' ',sum(km_int))

        # determine which rows represent a gap in effort exceeding the max_km_gap setting
        split_group <- rep(1, times=nrow(effi)) # assume there will be no splits
        breaks_raw <- which(km_int >= max_km_gap) ; breaks_raw
        if(length(breaks_raw)>0){
          breaks <- km_cum[breaks_raw-1] ; breaks
          #plot(km_cum,type='l') ; abline(h=km_cum[breaks_raw], col='steelblue3') # for debugging
          split_group <- sapply(km_cum, function(x){length(which(breaks < x))}) + 1
        }
        # store which segment split each row of data belongs to
        effi$map_split <- factor(split_group)

        # review
        #km_cum
        #split_group
        #data.frame(km_int,km_cum, split_group)

        # Remove rows that create the big gaps and cause artifical line connections
        effi <- effi[km_int < max_km_gap,]

        # Loop through each split & add each to the map separately
        splits <- unique(effi$map_split) # get unique split ids
        splits <- splits[!is.na(splits)] # make sure none of those are NA
        if(length(splits)>0){ # if there are still valid split ids:
          for(split_i in 1:length(splits)){ # loop through each split
            eff_split <- effi %>% filter(map_split == splits[split_i]) # filter data to this split

            # make a simple df for mapping
            eff_line <- data.frame(long = eff_split$Lon,
                                   lat = eff_split$Lat)
            eff_line
            # QAQC: make sure this is a line, not a point (more than one unique coordinate set)
            multi_pt_check <- c(length(unique(eff_line$long)),length(unique(eff_line$lat)))
            multi_pt_check <- any(multi_pt_check > 1)

            if(multi_pt_check){ # if it IS a line:
              # format points into a line
              # `points_to_line` is a LTabundR function.
              sp.lines <- points_to_line(data = eff_line,
                                         long = "long",
                                         lat = "lat")
              # add to map!
              m <- m +
                tmap::tm_shape(sp.lines) +
                tmap::tm_lines(col=eff_split$col[1],
                                                     lwd=eff_split$lwd[1],
                                                     lty=eff_split$lty[1])
              #m %>% print # for debugging
            } # end of add to map
          } # end of splits length
        } # end of if length splits > 0
      } # end of if nrow(effi)>0
    } # end if !is.null(effi)
    if(verbose){setTxtProgressBar(pb, rows_processed)} # update progress bar
  } # end of map_segments loop
  message('\n')

  # return the updated map object
  return(m)
}

