#' Segmentize survey effort
#'
#' This function chops `DAS` effort into discrete sections for the purposes of estimating the variance of the abundance estimate.
#' \cr \cr
#' This is an internal function typically not called by a user directly.
#' It is the fourth subroutine called within `process_surveys()`, after `format_das()` and before `process_sightings()`.
#' However, arguments are provided that allow you to call this function directly and override settings so that you can
#' explore how segmentizing works.
#'
#' @param cruz A nascent `cruz` object, which is a list produced by `format_das()`,
#' or a fully processed `cruz` object if you want to experiment with segmentizing alternatives.
#'
#' @param segment_method This and the remainder of inputs allow you to override
#' the settings contained within the `cruz` object. Leaving them as `NULL`
#' means the original settinsg will be used.
#' For `segment_method`, the two method options are `"day"` --
#' all effort within the same Cruise-Stratum-Year-Effort scenario (i.e., an effort bloc)
#' will be binned into segments by calendar date -- and `"equallength"` --
#' effort within each unique effort bloc
#' will be divided into segments of approximately equal length.
#'
#' @param segment_target_km If segmentizing by `"equallength"`,
#' this field allows you to specify what that target length is, in km.
#'
#' @param segment_max_interval If segmentizing by `"equallength"`,
#' this setting allows you to specify the time gaps in effort (in hours)
#' that are allowed to be contained within a single segment.
#' For example, if your goal is a few large segments of equal length
#' (e.g., 150-km segments, for bootstrap estimation of density variance),
#' you are probably willing for discrete periods of effort to be concatenated into a single segment,
#' even if the gaps between effort are as large as 1 or 2 days,
#' in which case you would set `segment_max_interval` to 24 or 48 (hours), respectively.
#' However, if your goal is many smaller segments (e.g., 5-km segments, for habitat modeling),
#' you want to ensure that effort is contiguous so that segment locations
#' can be accurately related to environmental variables,
#' in which case you would set `segment_max_interval` to be very small (e.g., .2 hours, or 12 minutes).
#' Setting this interval to a small number, such as 0.2, also allows
#' the segmentizing function to overlook momentary breaks in effort,
#' such as when an unofficial observer logs a sighting.
#'
#' @param segment_remainder_handling If segmentizing by `"equallength"`,
#' periods of effectively-contiguous effort (as specified by `segment_max_interval`)
#' are unlikely to be perfectly divisible by your `segment_target_km`;
#' there is going to be a remainder. You can handle this remainder in three ways:
#' (1) `"disperse"` allows the function to adjust `segment_target_km` so that
#' there is in fact no remainder, effectively dispersing the remainder evenly
#' across all segments within that period of contiguous effort;
#' (2) `"append"` asks the function to append the remainder to a randomly selected segment,
#' such that most segments are the target length with the exception of one longer one;
#' or (3) `"segment"` asks the function to simply place the remainder in its own segment,
#' placed randomly within the period of contiguous effort.
#' This setting also has a second layer of versatility,
#' because it can accept a one- or two-element character vector.
#' If a two-element vector is provided (e.g., `c("append","segment")`),
#' the first element will be used in the event that the remainder is less than or equal to
#' half your `segment_target_km`; if the remainder is more than half that target length,
#' the second element will be used. This feature allows for replication
#' of the segmentizing methods in Becker et al. (2010).
#'
#' @param beaufort_range A numeric vector indicating the Beaufort sea states (0 - 7) to be accepted within usable segments.
#'
#' @param distance_types  A character vector of the effort types that will be included
#' in detection function estimation,
#' and therefore considered in effort segmentizing.
#' Accepted values are `"S"` (systematic/standard effort), `"F"` (fine-scale effort),
#' and `"N"` (non-systematic/non-standard effort, in which systematic protocols
#' are being used but effort is not occurring along design-based transect routes).
#'
#' @param distance_modes  The effort modes that will be included in
#' detection function estimation,
#' and therefore considered in effort segmentizing.
#' Accepted values are `"P"` (passing) and `"C"` (closing)
#'
#' @param distance_on_off  The value(s) of `OnEffort`
#' (On Effort is `TRUE`, Off Effort is `FALSE`) that will be included in
#' detection function estimation,
#' and therefore considered in effort segmentizing.
#'
#' @param to_plot  Boolean, with default `FALSE`, indicating whether or not histograms showing segment lengths should be produced.
#'
#' @param debug_mode  Boolean, with default `FALSE`, indicating whether details should be printed to the Console that facilitate debugging.
#'
#' @param verbose Boolean, with default `FALSE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A modified `cruz` object, in which each cohort slot contains a list with the following slots:
#' `das` (with two new columns, `seg_id`, indicating the segment corresponding to each `DAS` row;
#' and `use`, indicating whether or not (`TRUE` or `FALSE`) the segment is to be included in the analysis.)
#' and `segments` (a dataframe summarizing each segment).
#'
#' @import dplyr
#' @export
#'
segmentize <- function(cruz,
                       segment_method = NULL,
                       segment_target_km = NULL,
                       segment_max_interval = NULL,
                       segment_remainder_handling = NULL,
                       beaufort_range = NULL,
                       distance_types = NULL,
                       distance_modes = NULL,
                       distance_on_off = NULL,
                       to_plot = TRUE,
                       debug_mode = FALSE,
                       verbose=FALSE){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    data(example_settings)
    settings <- example_settings
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    cruz <- process_strata(das, settings)
    cruz <- das_format(cruz)
    verbose=TRUE
    debug_mode = TRUE
    segment_method = NULL
    segment_target_km = NULL
    segment_max_interval = NULL
    segment_remainder_handling = NULL
    beaufort_range <- NULL
    distance_types <- NULL
    distance_modes <- NULL
    distance_on_off <- NULL

    # segment_remainder_handling <- c('append', 'segment')
    # segment_method <- 'day'
    # segment_max_interval <- 24
    # segment_target_km <- 30

    # try it
    cruz_demo <- segmentize(cruz, segment_method = 'day', segment_max_interval = 2, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'day', segment_max_interval = 6, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'day', segment_max_interval = 24, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'day', segment_max_interval = 48, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 5, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 10, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 50, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 150, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_max_interval = 3600, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_max_interval = 600, verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_remainder_handling = 'disperse', verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_remainder_handling = 'append', verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_remainder_handling = 'segment', verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_remainder_handling = c('append','segment'), verbose=TRUE)
    cruz_demo <- segmentize(cruz, segment_method = 'equallength', segment_target_km = 20, segment_remainder_handling = c('disperse','segment'), verbose=TRUE)

  } # end debugging
  #=============================================================================
  # Handle settings and inputs =================================================
  # if inputs are given, they override the settings slot of cruz

  settings <- cruz$settings

  if(is.null(segment_method)){
    segment_method <- settings$survey$segment_method
  }
  if(is.null(segment_target_km)){
    segment_target_km <- settings$survey$segment_target_km
  }
  if(is.null(segment_max_interval)){
    segment_max_interval <- settings$survey$segment_max_interval
  }
  if(is.null(segment_remainder_handling)){
    segment_remainder_handling <- settings$survey$segment_remainder_handling
  }

  # Stage cohort-specific input arguments
  beaufort_range_input <- beaufort_range
  distance_types_input <- distance_types
  distance_modes_input <- distance_modes
  distance_on_off_input <- distance_on_off

  # General prep  ==============================================================

  # Convert maximum time interval from hours to seconds
  segment_max_interval <- segment_max_interval * 3600
  #print(segment_max_interval)

  ##############################################################################
  # Main process

  cohorts <- cruz$cohorts # get cohorts list in cruz object
  cohort_settings <- cruz$settings$cohorts # get cohort settings list too
  cohorts_new <- list() # stage results
  length(cohorts) # review
  cohort_i <- 1 # for debugging

  # Loop through each cohort
  for(cohort_i in 1:length(cohorts)){
    cohorti <- cohorts[cohort_i] # get cohort data for this cohort
    cohorti_name <- names(cohorti) # get its name
    if(verbose){message('\nSegmentizing data for cohort "',cohorti_name,'" . . . ')}
    dass <- cohorti[[1]] # get data only
    head(dass)

    # Gather settings for this cohort ==========================================

    setti <- cohort_settings[[cohort_i]] ; setti # get cohort settings for this cohort
    # If inputs are given, they override the cohort settings:
    if(is.null(beaufort_range_input)){
      beaufort_range <- setti$beaufort_range
    }else{
      beaufort_range <- beaufort_range_input
    }
    if(is.null(distance_types_input)){
      distance_types <- setti$distance_types
    }else{
      distance_types <- distance_types_input
    }
    if(is.null(distance_modes_input)){
      distance_modes <- setti$distance_modes
    }else{
      distance_modes <- distance_modes_input
    }
    if(is.null(distance_on_off_input)){
      distance_on_off <- setti$distance_on_off
    }else{
      distance_on_off <- distance_on_off_input
    }

    # Review all settings
    segment_remainder_handling #%>% print
    segment_method #%>% print
    segment_max_interval #%>% print
    segment_target_km #%>% print
    beaufort_range # %>% print
    distance_types #%>% print
    distance_modes #%>% print
    distance_on_off #%>% print

    # Separate out 'out' stratum events, if any
    (outs <- which(dass$stratum == 'out'))
    if(length(outs)>0){
      dass_out <- dass[outs,] # keep to add back later
      dass <- dass[-outs,]
      if(debug){
        message('\n', length(outs),' DAS rows fell outside of a stratum. They will not be segmentized.\n')
      }
    }

    #===========================================================================
    # SEGMENTIZE
    #===========================================================================

    # Stage results
    cohorts_new$new <- list()
    cohorts_new$new$density <- list()

    # Perform segmentization for this cohort.

    # accommodate NA's in `modes`
    distance_modes <- c(distance_modes, NA)

    # Setup effort blocs =====================================================
    # (sections of survey with the same effort details / date / cruise / ship / stratum etc )

    if(verbose){
      message('--- Grouping survey data according to "effort scenario" ...')
      message("       (stretches of effort from same cruise-ship-stratum-date,")
      message("       split further into effort that will/won't be used in detection function modeling)")
    }

    blocs <-
      dass %>%
      # Create new columns indicating how to parse type/mode/onoff
      # these booleans will inform us whether to include or exclude the segment from analysis
      mutate(effdate = lubridate::date(DateTime)) %>%
      mutate(mode_group = Mode %in% distance_modes,
             onoff_group = OnEffort %in% distance_on_off,
             type_group = EffType %in% distance_types,
             bft_group = round(Bft) %in% beaufort_range) %>%
      # based on those directions, create a column deciding
      # whether this row will be used in density estimation
      mutate(use = ifelse(mode_group &
                            onoff_group &
                            type_group &
                            bft_group,
                          TRUE, FALSE)) %>%
      # Add type-type column detailing effort type in the rows for which use == TRUE
      mutate(type_type = ifelse(use, EffType, 'Off' )) %>%
      # make a column summarizing the effort scenario
      mutate(effort_scenario = paste(Cruise,
                                     ship,
                                     stratum,
                                     #effdate,
                                     use,
                                     type_type,
                                     sep=' - ')) %>%
      # Determine when the scenario changes & make bloc ID
      mutate(eff_bloc = as.numeric(factor(effort_scenario))) %>%
      # Get distance covered within effort bloc
      ungroup()

    # End of effort bloc setup ===============================================

    if(verbose){
      message('--- --- survey was grouped into ', length(unique(blocs$eff_bloc)),' effort-scenario blocs.')
    }

    if(debug_mode){ # Double check that use was assigned correctly
      problems <-
        blocs %>%
        filter(use == FALSE,
               stratum %in% setti$strata,
               EffType %in% distance_types,
               Mode %in% distance_modes,
               OnEffort %in% distance_on_off,
               round(Bft) %in% beaufort_range) %>%
        as.data.frame
      problems
    }

    if(verbose){
      message('\n--- Checking for excessive time gaps, based on setting `segment_max_interval` of ', segment_max_interval ,' seconds ...')
    }

    # further cut up eff_blocs based on time gaps
    blocs2 <-
      blocs %>%
      # make simple time column
      mutate(this_time = as.numeric(DateTime)) %>%

      # in each bloc, look for instances of big time gaps
      group_by(eff_bloc) %>%

      mutate(last_time = lag(this_time)) %>%
      mutate(int_time = this_time - last_time) %>%
      mutate(int_flag = ifelse(int_time >= segment_max_interval, 1, 0)) %>%
      mutate(int_flag = ifelse(is.na(int_flag), 0, int_flag)) %>%
      # where big time gaps exist, make new eff bloc by adding a suffix to bloc number
      mutate(new_bloc = paste0(eff_bloc[1], '-', cumsum(int_flag))) %>%

      # mutate(next_time = lead(this_time)) %>%
      # mutate(int_time = next_time - this_time) %>%
      # mutate(int_flag = ifelse(int_time >= segment_max_interval, 1, 0)) %>%
      # mutate(int_flag = ifelse(is.na(int_flag), 0, int_flag)) %>%
      # # where big time gaps exist, make new eff bloc by adding 0.01 to bloc number
      # mutate(new_bloc = paste0(eff_bloc[1], '-', cumsum(int_flag))) %>%
      #
      ungroup() %>%

      # Replace eff_bloc
      mutate(eff_bloc = new_bloc) %>%
      # Get distance covered within new effort bloc
      group_by(eff_bloc) %>%
      # Make sure lines are arranged chronologically
      arrange(DateTime) %>%
      # Replace NAs with 0
      mutate(km_int = tidyr::replace_na(km_int, 0)) %>%
      # Get length logged
      mutate(cum_bloc_km = cumsum(km_int),
             tot_bloc_km = sum(km_int, na.rm=TRUE),
             target_km = segment_target_km) %>%
      ungroup()

    if(verbose){
      # Were any blocs added based on time gaps?
      (npre <- blocs$eff_bloc %>% unique %>% length)
      (npos <- blocs2$eff_bloc %>% unique %>% length)
      if(npos > npre){
        message('--- --- found ', npos - npre ,' excessive time gaps; there are now ', npos ,' effort-scenario blocs.')
      }else{
        message('--- --- no further time gaps found. Keeping same number of effort-scenario blocs.')
      }
    }

    # Segmentize blocs =======================================================

    if(FALSE){
      # Values for debugging
      segment_method <- 'equallength'
      segment_method <- 'day'
      segment_max_interval <- 60*60 # 60 minutes
      segment_target_km <- 20
      segment_remainder_handling <- c('disperse', 'segment')
    }

    # If segment_remainder_handling is length one, duplicate it
    if(length(segment_remainder_handling)==1){
      segment_remainder_handling <- c(segment_remainder_handling,
                                      segment_remainder_handling)
    }
    segment_remainder_handling

    if(segment_method == 'day'){ #============================================
      if(verbose){ message('\n--- Segmentizing by day...') }
      segs <-
        blocs2 %>%
        group_by(eff_bloc, effdate) %>%
        mutate(seg_id = cur_group_id()) %>%
        ungroup() %>%
        #arrange(Cruise, DateTime) %>%
        #mutate(seg_id = eff_bloc) %>%
        group_by(seg_id) %>%
        mutate(tot_seg_km = sum(km_int, na.rm=TRUE)) %>%
        ungroup() %>%
        as.data.frame

      segs$seg_id %>% unique %>% length
      #segs$seg_id %>% table
      #segs$tot_seg_km %>% hist
      if(verbose){ message('--- --- Finished!') }
    }  #======================================================================

    if(segment_method == 'equallength'){ #====================================
      if(verbose){
        message('\n--- Segmentizing into stretches of equal length...')
        message('\n--- --- Determining the remainder for each bloc...')
      }

      blocs3 <-
        blocs2 %>%
        # For each bloc...
        group_by(new_bloc) %>%
        # Based on target_km, get n_segments & remainder
        mutate(n_seg_raw = tot_bloc_km / target_km) %>%
        mutate(remainder = tot_bloc_km %% target_km) %>%
        # Determine how to handle the remainder
        mutate(handling = ifelse(remainder <= (target_km/2),
                                 segment_remainder_handling[1],
                                 segment_remainder_handling[2])) %>%
        ungroup()

      #=======================================================================
      # Segmentize by one of the remainder handling methods

      (disperse_blocs <- blocs3 %>% filter(handling == 'disperse')) %>% nrow
      (segment_blocs <- blocs3 %>% filter(handling == 'segment')) %>% nrow
      (append_blocs <- blocs3 %>% filter(handling == 'append')) %>% nrow

      #(bs <- segment_blocs$new_bloc %>% unique)
      #(ba <- append_blocs$new_bloc %>% unique)
      #which(bs %in% ba)
      #which(ba %in% bs)

      segs <- data.frame() # put segmentized data here

      # If handling is disperse ... ==========================================
      if(nrow(disperse_blocs)>0){
        if(verbose){
          message('\n--- --- Segmentizing ', nrow(disperse_blocs),' rows of data using the "disperse" method ...')
        }

        segi <-
          disperse_blocs %>%
          # For each bloc...
          group_by(new_bloc) %>%
          # Adjust the target_km to perfectly fit bloc km
          mutate(remainder_seg = NA) %>% # placeholder so that seg methods can be combined
          mutate(target_km = ifelse(handling == 'disperse',
                                    tot_bloc_km / floor(n_seg_raw),
                                    target_km)) %>%
          # Assign segment IDs within bloc...
          mutate(bloc_seg_id = ceiling(cum_bloc_km / target_km)) %>%
          # then make sure they are globally unique
          mutate(seg_id_char = paste0(handling,'-',new_bloc,'-', bloc_seg_id)) %>%
          ungroup()

        segs <- rbind(segs, segi)
        if(verbose){ message('--- --- Finished!') }
      }

      # If handling is segment ... ===========================================
      if(nrow(segment_blocs)>0){
        if(verbose){
          message('\n--- --- Segmentizing ', nrow(segment_blocs),' rows of data using the "segment" method ...')
        }

        # use one of the longest use blocs as an example
        #segment_blocs %>% filter(use == TRUE) %>% arrange(desc(tot_bloc_km)) %>% as.data.frame %>% head

        # This is the function that will assign each row to a segment
        seg_assign <- function(cum_bloc_km, target_km, remainder, remainder_seg, n_seg_raw){
          # Stage a vector of km breaks; each break is a multiple of target length
          # make this vector one longer than the number of full-length segments, to make a placeholder for the remainder
          (km_breaks_raw <- rep(target_km,times=floor(n_seg_raw) + 1))
          # Replace one of these breaks with the remainder
          (km_breaks_raw[remainder_seg] <- remainder)
          # Convert to cumulative sum
          (km_breaks <- km_breaks_raw %>% cumsum)
          if(length(km_breaks)>1){km_breaks <- km_breaks[1:(length(km_breaks)-1)]}
          km_breaks
          # Decide which segment this cum_bloc_km belongs to
          (good_breaks <- which(km_breaks >= cum_bloc_km))
          if(length(good_breaks) > 0){
            seg_assi <- good_breaks[1]
          }else{
            seg_assi <- length(km_breaks) + 1
          }
          seg_assi
          return(seg_assi)
        }

        # Code for testing the seg_assign function
        if(FALSE){
          for(i in 0:202){
            message('km ',i,' goes in segment ',
                    seg_assign(cum_bloc_km = i, target_km = 20, remainder = 2.708868, remainder_seg = 3, n_seg_raw = 10.13544)
            )}
          # Test what happens with length 0 segments
          seg_assign(0, 20, 0, 0, 0)
        }

        # Segmentize
        segi <-
          segment_blocs %>%
          #filter(eff_bloc == 97) %>% # use this as a test bloc during debugging
          # For each bloc...
          group_by(new_bloc) %>%
          # randomly select a segment in which to place the remainder
          mutate(remainder_seg = sample(1:(floor(n_seg_raw[1])+1), 1)) %>%
          # Assign each row to a segment ID
          rowwise() %>%
          mutate(bloc_seg_id = seg_assign(cum_bloc_km, target_km, remainder, remainder_seg, n_seg_raw)) %>%
          # Ungroup and convert seg id into universally unique number
          ungroup() %>%
          ungroup() %>%
          arrange(DateTime) %>%
          mutate(seg_id_char = paste0(handling,'-',new_bloc,' plus ', bloc_seg_id)) %>%
          ungroup()

        segs <- rbind(segs, segi)
        if(verbose){ message('--- --- Finished!') }
      }

      # If handling is append ... ============================================
      if(nrow(append_blocs)>0){
        if(verbose){
          message('\n--- --- Segmentizing ', nrow(append_blocs),' rows of data using the "append" method ...')
        }
        # use one of the longest use blocs as an example
        #segment_blocs %>% filter(use == TRUE) %>% arrange(desc(tot_bloc_km)) %>% as.data.frame %>% head

        # This is the function that will assign each row to a segment
        seg_assign <- function(cum_bloc_km, target_km, remainder, remainder_seg, n_seg_raw){
          # Stage a vector of km breaks; each break is a multiple of target length
          # make this vector one longer than the number of full-length segments, to make a placeholder for the remainder
          (km_breaks_raw <- rep(target_km,times=floor(n_seg_raw)))
          # Append one of these breaks with the remainder
          (km_breaks_raw[remainder_seg] <- km_breaks_raw[remainder_seg] + remainder)
          # Convert to cumulative sum
          (km_breaks <- km_breaks_raw %>% cumsum)
          if(length(km_breaks)>1){km_breaks <- km_breaks[1:(length(km_breaks)-1)]}
          km_breaks
          # Decide which segment this cum_bloc_km belongs to
          (good_breaks <- which(km_breaks >= cum_bloc_km))
          if(length(good_breaks) > 0){
            seg_assi <- good_breaks[1]
          }else{
            seg_assi <- length(km_breaks) + 1
          }
          seg_assi
          return(seg_assi)
        }

        if(FALSE){ # Code for testing the seg_assign function
          for(i in 0:202){
            message('km ',i,' goes in segment ', seg_assign(cum_bloc_km = i,
                                                            target_km = 20,
                                                            remainder = 2.708868,
                                                            remainder_seg = 3,
                                                            n_seg_raw = 10.13544))
          }
          # Test what happens with length 0 segments
          seg_assign(0, 20, 0, 0, 0)
        }

        # Segmentize
        segi <-
          append_blocs %>%
          #filter(eff_bloc == 97) %>% # use this as a test bloc during debugging
          # For each bloc...
          group_by(new_bloc) %>%
          # randomly select a segment in which to place the remainder
          mutate(remainder_seg = sample(1:(floor(n_seg_raw[1])), 1)) %>%
          # Assign each row to a segment ID
          rowwise() %>%
          mutate(bloc_seg_id = seg_assign(cum_bloc_km, target_km, remainder, remainder_seg, n_seg_raw)) %>%
          # Ungroup and convert seg id into universally unique number
          ungroup() %>%
          ungroup() %>%
          arrange(DateTime) %>%
          mutate(seg_id_char = paste0(handling,'-',new_bloc,' plus ', bloc_seg_id)) %>%
          ungroup()

        segs <- rbind(segs, segi)
        if(verbose){ message('--- --- Finished!') }
      }

      # Finalize formatting!
      segs <-
        segs %>%
        # Assign segment IDs
        mutate(seg_id = seg_id_char %>% factor %>% as.numeric) %>%
        # Get distance covered by each segment
        group_by(seg_id) %>%
        mutate(tot_seg_km = sum(km_int, na.rm=TRUE)) %>%
        ungroup() %>%
        as.data.frame

    } # end equal length =====================================================

    # Status report
    if(verbose){
      message('\n--- Segmentizing complete!')
      message('--- --- unique segments created = ', segs$seg_id %>% unique %>% length)
      message('--- --- mean length = ', segs$tot_seg_km %>% mean(na.rm=TRUE) %>% round(2),' km')
      message('--- --- median length = ', segs$tot_seg_km %>% median(na.rm=TRUE) %>% round(2),' km')
    }

    # internal debugging tests
    if(FALSE){
      segs$use %>% table
    }

    if(to_plot){
      par(mfrow=c(2,1), mar=c(4.2,4,3,1))
      segs %>%
        group_by(seg_id) %>%
        filter(use == TRUE) %>%
        filter(row_number() == 1) %>%
        pull(tot_seg_km) %>%
        hist(breaks=20, main='use == TRUE', xlab='Segment length (km)')
      segs %>%
        group_by(seg_id) %>%
        filter(use == FALSE) %>%
        filter(row_number() == 1) %>%
        pull(tot_seg_km) %>%
        hist(breaks=20, main='use == FALSE', xlab='Segment length (km)')
      par(mfrow=c(1,1), mar=c(4.2,4,3,1))
    }

    # End segmentize blocs ===================================================

    # Summarize segments =======================================================

    if(verbose){message('\n--- Summarizing each segment ...')}

    seg_summary <-
      segs %>%
      group_by(seg_id) %>%
      summarize(Cruise = Cruise[1],
                ship = ship [1],
                stratum = stratum[1],
                use = paste(use %>% unique, collapse='-'),
                Mode = paste(Mode %>% unique, collapse='-'),
                EffType = paste(EffType %>% unique, collapse='-'),
                OnEffort = paste(OnEffort %>% unique, collapse='-'),
                ESWsides = ESWsides[1],
                dist = tot_seg_km[1],
                minutes = round(sum(int_time)/60, 3),
                n_rows = n(),
                min_line = line_num[1],
                max_line = line_num[n()],
                year = year[1],
                month = month[1],
                day = day[1],
                lat1 = Lat[1],
                lon1 = Lon[1],
                DateTime1 = DateTime[1],
                timestamp1 = as.numeric(DateTime[1]),
                yday1 = yday[1],
                lat2 = Lat[n()],
                lon2 = Lon[n()],
                DateTime2 = DateTime[n()],
                timestamp2 = as.numeric(DateTime[n()]),
                yday2 = yday[n()],
                mlat = Lat[mean(1:n())],
                mlon = Lon[mean(1:n())],
                mDateTime = DateTime[mean(1:n())],
                mtimestamp = as.numeric(DateTime[mean(1:n())]),
                avgBft = stats::weighted.mean(Bft, km_int, na.rm=TRUE),
                avgSwellHght = stats::weighted.mean(SwellHght, km_int, na.rm=TRUE),
                avgHorizSun = stats::weighted.mean(HorizSun, km_int, na.rm=TRUE),
                avgVertSun = stats::weighted.mean(VertSun, km_int, na.rm=TRUE),
                avgGlare = stats::weighted.mean(Glare, km_int, na.rm=TRUE),
                avgVis = stats::weighted.mean(Vis, km_int, na.rm=TRUE),
                avgCourse = stats::weighted.mean(Course, km_int, na.rm=TRUE),
                avgSpdKt = stats::weighted.mean(SpdKt, km_int, na.rm=TRUE))

    if(verbose){message('--- --- Finished!')}

    # end summarize segments ===================================================

    # internal debugging tests
    if(FALSE){
      seg_summary %>% nrow
      seg_summary %>% as.data.frame %>% head
      seg_summary$use %>% table
      seg_summary$OnEffort %>% table
      seg_summary$Mode %>% table
      seg_summary$EffType %>% table
      (same_day_test <- seg_summary %>% filter(yday1 != yday2))
    }

    # Confirm that all segments occurred on the same day
    #(same_day_test <- seg_summary %>% filter(use == TRUE, yday1 != yday2))
    (same_day_test <- seg_summary %>% filter(yday1 != yday2))
    if(nrow(same_day_test)>0){
      message('\n ******* FYI: Some segments spanned dates. See printed rows below. This is not necessarily bad, just an FYI. *******')
      print(same_day_test)

      print(same_day_test %>% as.data.frame)
    }

    # Clean up data (remove all the columns you added during intermediate steps
    segs_clean <- segs %>% select(Event:stratum, use, eff_bloc, seg_id)

    # Prepare this cohort's list
    cohort_list <- list(segments = seg_summary,
                        das = segs_clean)

    # end summarize segments =======================================================

    # segmentize_wrapper is a LTabundR function.-- see its documentation: ?segmentize_wrapper()
    # segments <- segmentize_wrapper(dass,
    #                                segment_method = segment_method,
    #                                segment_target_km = segment_target_km,
    #                                segment_max_interval = segment_max_interval,
    #                                segment_remainder_handling = segment_remainder_handling,
    #                                beaufort_range = beaufort_range,
    #                                types = distance_types,
    #                                modes = distance_modes,
    #                                on_off = distance_on_off,
    #                                debug_mode = FALSE,
    #                                verbose = verbose)

    # Summarize segments
    # also using a LTabundR function: ?segmentize_summarize()
    # if(verbose){message('--- --- calculating details for each segment ...')}
    # cohort_list <- segmentize_summarize(segments,
    #                                     dass,
    #                                     debug_mode = FALSE,
    #                                     to_plot = FALSE,
    #                                     verbose = verbose)
    # if(verbose){message('\n')}

    #===========================================================================
    # Get diagnostics on which segments were excluded
    if(debug_mode){

      (usefalse <-
         cohort_list$segments %>%
         arrange(DateTime1) %>%
         filter(use == FALSE)
      ) %>% head

      message('\nSTART OF DIAGNOSTICS: segments with use == FALSE (n = ', nrow(usefalse),'):')
      message('\n ---- FYI: Per your settings, use will be TRUE for this cohort in the following conditions:')
      message(' ---- ---- column `stratum` is one of these values: ',       paste(setti$strata, collapse=', '))
      message(' ---- ---- column `EffType` is one of these values: ', paste(distance_types, collapse=', '))
      message(' ---- ---- column `Mode` is one of these values: ', paste(distance_modes, collapse=', '))
      message(' ---- ---- column `OnEffort` is one of these values: ', paste(distance_on_off, collapse=', '))
      message(' ---- ---- column `Bft` within segment, when averaged, can be rounded to one of these values: ', paste(beaufort_range, collapse=', '))
      message('\n')

      if(nrow(usefalse)>0){

        falsi=1
        for(falsi in 1:nrow(usefalse)){
          (usi <- usefalse[falsi,])
          message(' ---- Segment ID = ',usi$seg_id,' | DAS rows = ',usi$n_rows,
                  ' (line_num ', usi$min_line,'-', usi$max_line,', ',
                  round(((usi$timestamp2 - usi$timestamp1)/60)),' mins) | start = ',
                  usi$DateTime1,' | stratum = ',usi$stratum,
                  ' | EffType(s) = ', usi$EffType,' | Mode(s) = ', usi$Mode,' | OnEffort(s) = ', usi$OnEffort,
                  ' | avg Bft = ', round(usi$avgBft))
        }
        message('\n')
        message('END OF DIAGNOSTICS: The above segments have use == FALSE (n = ', nrow(usefalse),'):')
        message('\n ---- FYI: Per your settings, use will be TRUE for this cohort in the following conditions:')
        message(' ---- ---- column `stratum` is one of these values: ',       paste(setti$strata, collapse=', '))
        message(' ---- ---- column `EffType` is one of these values: ', paste(distance_types, collapse=', '))
        message(' ---- ---- column `Mode` is one of these values: ', paste(distance_modes, collapse=', '))
        message(' ---- ---- column `OnEffort` is one of these values: ', paste(distance_on_off, collapse=', '))
        message(' ---- ---- column `Bft` within segment, when averaged, can be rounded to one of these values: ', paste(beaufort_range, collapse=', '))
        message('\n')
      }
    }
    #===========================================================================

    # Add back out events
    if(length(outs)>0){
      dass_out$seg_id <- NA # add these columns
      dass_out$use <- FALSE
      dass_out$km_int[dass_out$km_int > 30] <- 0
      cohort_list$das <- rbind(cohort_list$das, dass_out)
    }

    # Review
    names(cohort_list)
    cohort_list$segments %>% names
    cohort_list$das %>% names
    cohort_list$das$use %>% table
    cohort_list$segments$use %>% table

    # save results to density list for this cohort
    cohorts_new$new <- cohort_list

    # Now rename the cohort list that you've been building, with the cohort's name
    names(cohorts_new)[which(names(cohorts_new) == 'new')] <- cohorti_name

  } # end of cohort loop

  # review
  cohorts_new %>% names

  # Replace cohorts with newly segmentized cohorts
  cruz_new <- cruz
  cruz_new$cohorts <- cohorts_new
  if(verbose){message('\nFinished!')}
  return(cruz_new)
}
