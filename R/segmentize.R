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
#' this setting allows you to specify the time gaps in effort
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
    cruz_demo <- segmentize(cruz, verbose=TRUE)


    morts <- c()
    for(i in 1:10000){
      morti <- (runif(10, 0, 1) < .20) %>% sum()
      morts <- c(morts, morti)
    }

    #hist(morts, main = B)
    (decadi <- length(which(morts == 0)) / length(morts))

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
    if(verbose){message('Segmentizing data for cohort "',cohorti_name,'" . . . ')}
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
    outs <- which(dass$stratum == 'out')
    if(length(outs)>0){
      dass_out <- dass[outs,] # keep to add back later
      dass <- dass[-outs,]
    }

    #===========================================================================
    # SEGMENTIZE
    #===========================================================================

    # Stage results
    cohorts_new$new <- list()
    cohorts_new$new$density <- list()

    # Perform segmentization for this cohort.
    # segmentize_wrapper is a LTabundR function.-- see its documentation: ?segmentize_wrapper()
    segments <- segmentize_wrapper(dass,
                                   segment_method = segment_method,
                                   segment_target_km = segment_target_km,
                                   segment_max_interval = segment_max_interval,
                                   segment_remainder_handling = segment_remainder_handling,
                                   beaufort_range = beaufort_range,
                                   types = distance_types,
                                   modes = distance_modes,
                                   on_off = distance_on_off,
                                   debug_mode = FALSE,
                                   verbose = verbose)

    # Summarize segments
    # also using a LTabundR function: ?segmentize_summarize()
    if(verbose){message('--- --- calculating details for each segment ...')}
    cohort_list <- segmentize_summarize(segments,
                                        dass,
                                        debug_mode = FALSE,
                                        to_plot = FALSE,
                                        verbose = verbose)
    if(verbose){message('\n')}

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
    #if(verbose){message('---')}

  } # end of cohort loop

  # review
  cohorts_new %>% names

  # Replace cohorts with newly segmentized cohorts
  cruz_new <- cruz
  cruz_new$cohorts <- cohorts_new
  return(cruz_new)
}
