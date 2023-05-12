#' Group size estimation wrapper
#'
#'  This is an internal function typically not called by a user directly.
#'  This function is used as a subroutine in `process_sightings()`.
#'  It produces school size estimates for each species within a sighting,
#'  based upon all estimates of school size and species composition provided by observers.
#'
#' @param grp  A dataframe, in which each row is a school size estimate from a single observer,
#' and each column is a column from the `DAS` dataframe that is relevant to school size estimation
#' (columns Event, year, Bft, and Prob:GsSchoolLow).
#' @param gs_coefficients If not `NULL`, a dataframe of school size calibration
#' coefficients (see description in `load_survey_settings()`).
#' @param calibrate_floor This argument accepts a number
#' indicating the minimum raw school size estimate
#' for which school size calibration will be attempted.
#'  When this function is used withing `process_sightings()`,
#' this setting from the `cruz` object will be provided.
#' @param geometric_mean This argument accepts a Boolean;
#' if `TRUE`, geometric means will be calculated instead of arithmetic means.
#' If school size calibration is carried out,
#' the geometric mean will be weighted by calibration variance, such that
#' estimates from observers with low variance will receive more weight.
#' When this function is used withing `process_sightings()`,
#' this setting from the `cruz` object will be provided.
#' @param use_low_if_na If this setting is `TRUE`,
#' when no observer makes a best estimate of group size,
#' mean group size will be calculated from "low" estimates.
#' This will be done only if no observer has a "best" estimate.
#' When this function is used withing `process_sightings()`,
#' this setting from the `cruz` object will be provided.
#' @param debug_mode  Boolean, with default `FALSE`,
#' indicating whether details should be printed to the Console that facilitate debugging.
#' @param verbose Boolean, with default `TRUE`,
#' indicating whether or not updates should be printed to the Console.
#'
#' @return A dataframe in which each row is a species within the sighting,
#' with final estimates of best / high / low and metadata regarding calibration.
#' Passed back to `process_sightings()`.
#'
#' @export
#'
group_size <- function(grp,
                       gs_coefficients = NULL,
                       calibrate_floor = 0,
                       geometric_mean = FALSE,
                       use_low_if_na = TRUE,
                       debug_mode = FALSE,
                       verbose=TRUE){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    data(example_settings)
    settings <- example_settings
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- load_das(das_file)
    cruz <- process_strata(das, settings)
    cruz <- format_das(cruz)
    cruz <- segmentize(cruz)
    sits <- das_sight(cruz$cohorts$default$das, return.format = 'complete')
    # Example of a mixed-species sightings
    siti <- sits %>% filter(SightNo == '002') ; siti
    # This is the input prepared by the process_sightings() function, which is
    # then passed to this function
    (grp <- siti %>%
        dplyr::filter(Event == 'S') %>%
        dplyr::select(Event, year, Bft, Prob:GsSchoolLow))

    debug_mode = TRUE
    use_low_if_na <- TRUE
    data(group_size_coefficients)
    (gs_coefficients <- group_size_coefficients)
    (geometric_mean <- cruz$settings$cohorts[[1]]$geometric_mean_group)
    calibrate_floor = 0
  }
  #=============================================================================

  # This function takes a dataframe of DAS data for a single sighting,
  # with columns filtered to those relevant to group size estimation
  # It is called from process_sightings().
  # This function contains all code for estimating group size.

  ##############################################################################
  ##############################################################################
  # Mean functions

  geometric_weighted_mean <- function(x, w){
    ss <- NA
    w <- 1/w[is.finite(x)]
    x <- x[is.finite(x)]
    if(length(x)>0){
      ss <- prod(x^w)^(1/sum(w))
    }
    return(ss)
  }

  geometric_unweighted_mean <- function(x){
    ss <- NA
    x <- x[is.finite(x)]
    if(length(x)>0){
      ss <- prod(x, na.rm=TRUE)^(1/length(x[!is.na(x)]))
    }
    return(ss)
  }

  ##############################################################################
  ##############################################################################

  # review input
  grp

  # Stage results
  (sp_mixed <- grp$Mixed %>% unique) # is this a mixed species sighting?
  (sp_prob <- grp$Prob %>% unique) # are IDs probable?
  (sp_n <- grp %>% dplyr::select(nSp) %>% unique %>% as.numeric) # number of species in sighitng

  # Groups size estimates
  (bests <- grp$GsSchoolBest)   # Estimates from each observer...
  (highs <- grp$GsSchoolHigh)
  (lows <- grp$GsSchoolLow)

  # Raw estimates
  if(debug_mode){
    message('Raw estimates:')
    message('--- best = ',paste(bests,collapse=', '))
    message('--- low = ',paste(lows,collapse=', '))
    message('--- high = ',paste(highs,collapse=', '))
  }

  # Simple raw results
  if(debug_mode){
    (gs_besti <- geometric_unweighted_mean(bests))
    message('--- geometric unweighted mean = ',round(gs_besti,2))
    (gs_besti <- mean(bests,na.rm=TRUE))
    message('--- simple arithmetic mean = ',round(gs_besti,2))
  }

  # Calibrate group sizes ======================================================

  best_vars <- rep(NA,times=length(bests)) # stage vector for variance of estimates for each observer

  # If calibration coefficients are provided ...
  if(!is.null(gs_coefficients)){

    # Stage variables
    bft <- grp$Bft[1] %>% as.numeric ; bft
    yr <- grp$year[1] %>% as.numeric ; yr

    # Loop through each observer...
    obs_i <- 3 # for debugging
    debugs <- data.frame() # for debugging
    for(obs_i in 1:nrow(grp)){
      (obsi <- grp$ObsEstimate[obs_i]) # this observer's ID
      (besti <- bests[obs_i]) # her/his best estimate
      (lowi <- lows[obs_i]) # low estimate
      (highi <- highs[obs_i]) # high estimate

      # group_size_calibration() is a LTabundR function.
      # See group_size_calibration.R
      calib_results <- group_size_calibration(obs = obsi,
                                              bft = bft,
                                              yr = yr,
                                              gbest = besti,
                                              glow = lowi,
                                              ghigh = highi,
                                              gs_coefficients = gs_coefficients,
                                              calibrate_floor = calibrate_floor)
      # Return is a one-row dataframe with best estimate, variance, and other parameters
      calib_results # review
      if(debug_mode){ # debugging
        debugs <- rbind(debugs,
                        data.frame(obs = obsi,
                                   best_raw = besti,
                                   low_raw = lowi,
                                   high_raw = highi,
                                   calib_results))
      }

      # if less than 1, coerce to 1
      if(!is.na(calib_results$best)){
        if(calib_results$best < 1){calib_results$best <- 1}
      }

      # add to results vectors (each element corresponds to an observer)
      if(!is.na(besti)){
        bests[obs_i] <- calib_results$best
      }else{
        if(!is.na(lowi)){lows[obs_i] <- calib_results$best}
      }
      best_vars[obs_i] <- calib_results$var
      bests
      best_vars
    }

    # Review in debugging mode
    if(debug_mode){
      message('---')
      message('Calibrated estimates:')
      message('--- best estimates = ',paste(round(bests,3),collapse=', '))
      message('--- variance = ',paste(round(best_vars,3),collapse=', '))
      message('---')
      print(debugs)
      message('---')
    }
  }else{
    if(debug_mode){message(' --- Calibration coefficients unavailable. No calibration!\n---')}
  }

  # Setup final estimate objects
  gs_best <- gs_low <- gs_high <- NULL

  # Geometric weighted mean
  (gs_besti <- geometric_weighted_mean(bests,best_vars))
  (gs_lowi <- geometric_weighted_mean(lows,best_vars))
  (gs_highi <- geometric_weighted_mean(highs,best_vars))
  if(debug_mode){message('--- geometric weighted mean = ',round(gs_besti,2))}

  # Keep this estimate, if settings say so
  if(geometric_mean){
    if(!is.null(gs_coefficients)){
      gs_best <- gs_besti
      gs_low <- gs_lowi
      gs_high <- gs_highi
    }
  }

  # Geometric UN-weighted means
  # No calibration variance to use as weights -- just use simple geometric mean
  (gs_besti <- geometric_unweighted_mean(bests))
  (gs_lowi <- geometric_unweighted_mean(lows))
  (gs_highi <- geometric_unweighted_mean(highs))
  if(debug_mode){message('--- geometric unweighted mean  = ',round(gs_besti,2))}

  # Keep this estimate, if settings say so
  if(geometric_mean & is.null(gs_coefficients)){
    gs_best <- gs_besti
    gs_low <- gs_lowi
    gs_high <- gs_highi
  }

  # Just use arithmetic mean
  (gs_besti <- mean(bests,na.rm=TRUE))
  (gs_lowi <- mean(lows,na.rm=TRUE))
  (gs_highi <- mean(highs,na.rm=TRUE))
  if(debug_mode){message('--- simple arithmetic mean = ',round(gs_besti,2))}

  # Keep this estimate, if settings say so
  if(!geometric_mean){
    gs_best <- gs_besti
    gs_low <- gs_lowi
    gs_high <- gs_highi
  }

  # If best estimate is negative, make it NA
  if(!is.na(gs_best)){if(gs_best < 0){gs_best <- NA}}

  # If best is NA for all users, use the Low
  if(use_low_if_na){
    if(is.na(gs_best)){gs_best <- gs_low}
    if(!is.na(gs_best)){if(gs_best < 0){gs_best <- NA}}
  }
  gs_best ; gs_low ; gs_high

  # If the best group size estimate is still NA, just use 1
  if(is.na(gs_best)){gs_best <- 1}
  gs_best ; gs_low ; gs_high

  if(debug_mode){message('---\nFinal estimate = ',round(gs_best,2))}

  # Get group for each species by percentage ===================================
  (sp_percs <- grp %>% dplyr::select(SpPerc1:SpPerc4))
  (sp_percs <- apply(sp_percs,2,function(x){mean(x,na.rm=TRUE)/100}))

  # Get dataframe of spcodes only
  (spcodes <-(grp %>% dplyr::select(SpCode1:SpCode4))[1,])

  # Loop through each species and create results row
  i=1 # debugging
  grp_results <- data.frame() # stage results
  for(i in 1:length(spcodes)){
    spi <- spcodes[i] ; spi # species in this row
    if(!is.na(spi)){ # if it is not NA
      perci <- sp_percs[i] ; perci # group percentage for this species
      dfi <- data.frame(species = as.character(spi[1,1]), # species code
                        best = gs_best * perci, # number for this species only
                        low = gs_low * perci,
                        high = gs_high * perci,
                        prob = sp_prob,
                        mixed = sp_mixed,
                        ss_tot = gs_best,
                        lnsstot = log(gs_best),
                        ss_percent = perci,
                        n_sp = sp_n,
                        n_obs = nrow(grp),
                        n_best = length(bests[!is.na(bests)]), # how many best estimates were there?
                        n_low = length(bests[!is.na(lows)]),
                        n_high = length(bests[!is.na(highs)]),
                        calibr = !is.null(group_size_coefficients) # was calibration attempted?
      )
      dfi
      grp_results <- rbind(grp_results,dfi)
    }
  }

  # review
  grp_results

  return(grp_results)
}
