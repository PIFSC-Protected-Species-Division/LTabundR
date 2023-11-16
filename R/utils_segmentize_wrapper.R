#' Segmentizer wrapper to prevent redundant code
#'
#' This is an internal function typically not called by a user directly.
#' It is called within `segmentize()`.  This function parses `DAS` survey data
#' into "effort blocs", in which effort conditions are the same.
#' It then calls a subroutine, `segmentize_core()` to segmentize the data within
#' each effort bloc.
#'
#' @param das  The `DAS` survey data.
#' @param segment_method See documentation for `segmentize()`.
#' @param segment_target_km See documentation for `segmentize()`.
#' @param segment_max_interval See documentation for `segmentize()`.
#' @param segment_remainder_handling See documentation for `segmentize()`.
#' @param types See documentation for `segmentize()`.
#' @param modes See documentation for `segmentize()`.
#' @param on_off See documentation for `segmentize()`.
#' @param debug_mode  Boolean, with default `FALSE`, indicating whether details should be printed to the Console that facilitate debugging.
#' @param verbose Boolean indicating whether or not updates should be printed to the Console.
#'
#' @return A list in which each slot is a chunk of survey effort corresponding to a single segment.
#' This result is passed back to `segmentize()`.
#'
#' @export
#'
segmentize_wrapper <- function(das,
                               segment_method,
                               segment_target_km,
                               segment_max_interval,
                               segment_remainder_handling,
                               beaufort_range,
                               types,
                               modes,
                               on_off,
                               debug_mode,
                               verbose){

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
    das <- cruz$cohorts$default

    segment_remainder_handling <- c('append', 'segment')
    segment_method <- 'day'
    segment_max_interval <- 24 #48 # hours
    segment_target_km <- 30
    beaufort_range = 0:6
    types <- c('S','F')
    modes <- c('P','C')
    on_off <- TRUE
  }
  #=============================================================================

  # This function takes DAS data for a single cohort and all the segment-related settings for that cohort

  # Save safe copy of the input data
  dassi <- das
  head(dassi)

  # accommodate NA's in `modes`
  modes <- c(modes, NA)

  # Create new columns indicating how to parse type/mode/onoff
  # these booleans will inform us whether to include or exclude the segment from analysis
  dassi <- dassi %>%
    dplyr::mutate(mode_group = Mode %in% modes,
                  onoff_group = OnEffort %in% on_off,
                  type_group = EffType %in% types,
                  bft_group = Bft >= min(beaufort_range) & Bft <= max(beaufort_range))

  #dassi %>% head

  # based on those directions, create a column deciding
  # whether this row will be used in density estimation
  dassi$use <- FALSE # initialize new column with assumption that data will be excluded
  # modify new column to TRUE (use) if all inclusion criteria are met:
  dassi$use[which(dassi$mode_group &
                    dassi$onoff_group &
                    dassi$type_group &
                    dassi$bft_group)] <- TRUE
  table(dassi$use) # review

  # Add type-type column detailing effort type in the rows for which use == TRUE
  dassi$type_type <- 'Off' # initialize column with assumption row will not be used
  # modify new column for rows in which use == TRUE
  dassi$type_type[dassi$use] <- dassi$EffType[dassi$use]
  table(dassi$type_type) # review

  # Summarize parsing plan -- find 'bloc's of unique effort scenarios
  parse_summary <- dassi %>%
    dplyr::group_by(Cruise, stratum, year,use,type_type) %>%
    dplyr::tally()
  if(debug_mode){parse_summary %>% print} # review

  # Parse data into list
  # (group data by 'bloc' then relegate each group into its own slot in a list)
  dassplits <- dassi %>%
    dplyr::group_by(Cruise, stratum, year,use,type_type) %>%
    dplyr::group_split()

  # review
  class(dassplits)
  length(dassplits)

  # Now process each split =====================================================

  segments <- list()   # stage results
  spliti = 1 # for debugging
  for(spliti in 1:length(dassplits)){
    if(verbose){message('--- --- segmentizing effort bloc ',spliti,' of ',length(dassplits),' . . .')}
    dasspliti <- dassplits[[spliti]] # das data for a single bloc

    # Perform segmentization for this set of DAS data
    # segmentize_core() is a LTabundR function -- find its R code in 'utils_segmentize_core.R'
    segmenti <-
      segmentize_core(dasspliti,
                    segment_method = segment_method,
                    segment_target_km = segment_target_km,
                    segment_max_interval = segment_max_interval,
                    segment_remainder_handling = segment_remainder_handling,
                    debug_mode = FALSE,
                    verbose = FALSE)

    # that function returns a list in which each slot is a segment of data
    # add each of these segments to your results list `segments`
    for(i in 1:length(segmenti)){
      segments[[length(segments)+1]] <- segmenti[[i]]
    }
  } # end of looping through each bloc

  # review
  length(segments)

  return(segments)
}
