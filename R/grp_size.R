#' Group size estimation
#'
#'  This is an internal function typically not called by a user directly.
#'  This function is used as a subroutine in `process_sightings()`.
#'  It produces school size estimates for each species within a sighting,
#'  based upon all estimates of school size and species composition provided by observers.
#'
#' @param grp  A dataframe, in which each row is a school size estimate from a single observer,
#' and each column is a column from the `DAS` dataframe that is relevant to school size estimation
#' (columns Event, year, Bft, and Prob:GsSchoolLow).
#'
#' @param calibrate If `NULL`, group sizes will not be calibrated.
#' Otherwise supply a `list` to specify which calibration approach to use.
#' Currently two approaches are supported: that used by `ABUND 7/8` (the default shown),
#' which takes an observer-specific, species-agnostic approach,
#' and that developed in Gerrodette et al. (2019) and applied in Barlow et al. (2026),
#' which includes a species-specific, observer-agnostic approach. Details in `?load_cohort_settings()`.
#'
#' @param geometric_mean This argument accepts a Boolean;
#' if `TRUE`, geometric means will be calculated instead of arithmetic means.
#' If school size calibration is carried out,
#' the geometric mean will be weighted by calibration variance, such that
#' estimates from observers with low variance will receive more weight.
#' When this function is used withing `process_sightings()`,
#' this setting from the `cruz` object will be provided.
#' Note that, although only the best estimates may be calibrated if specified above (never the highs and lows),
#' the *same* kind of averaging function *is* applied to the highs and lows as is applied to the bests.
#' That is, when `geometric_mean` is `TRUE`, the geometric mean of the highs and the lows is returned.
#' If the best estimates are calibrated, the geometric *weighted* mean will be applied to the highs and lows,
#' using the variance of the calibrated best estimates as weights. If the best estimates are *not* calibrated,
#' the *unweighted* geometric mean is used to estimate the highs, lows, and bests.
#'
#' @param use_low_if_na If this setting is `TRUE`,
#' when no observer makes a best estimate of group size,
#' mean group size will be calculated from "low" estimates.
#' This will be done only if no observer has a "best" estimate.
#' When this function is used withing `process_sightings()`,
#' this setting from the `cruz` object will be provided.
#'
#' @param debug_mode  Boolean, with default `FALSE`,
#' indicating whether details should be printed to the Console that facilitate debugging.
#'
#' @param verbose Boolean, with default `TRUE`,
#' indicating whether or not updates should be printed to the Console.
#'
#' @return A dataframe in which each row is a species within the sighting,
#' with final estimates of best / high / low and metadata regarding calibration.
#' When this function is used internally by `LTabundR`, the results are passed back to `process_sightings()`.
#'
#' @export
#'
grp_size <- function(grp,
                     calibrate = grp_ops_abund,
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
    das <- das_load(das_file)
    cruz <- process_strata(das, settings)
    cruz <- das_format(cruz)
    cruz <- segmentize(cruz)
    sits <- swfscDAS::das_sight(cruz$cohorts$default$das, return.format = 'complete')
    # Example of a mixed-species sightings
    siti <- sits %>% filter(SightNo == '002')
    siti %>% as.data.frame
    # This is the input prepared by the process_sightings() function, which is
    # then passed to this function
    (grp <- siti %>%
        dplyr::filter(Event == 'S') %>%
        dplyr::select(Event, year, Bft, Prob:GsSchoolLow))
    grp
    #grp$ObsEstimate <- 9999

    (calibrate <- grp_ops_abund)
    #(calibrate <- grp_ops_gerrodette)
    debug_mode = TRUE
    use_low_if_na <- TRUE
    geometric_mean <- TRUE
    verbose <- TRUE

    # No calibration ==============
    grp_size(grp = grp,
             calibrate = NULL,
             geometric_mean = geometric_mean,
             use_low_if_na = use_low_if_na,
             debug_mode = debug_mode,
             verbose = verbose)

    # ABUND =======================
    data(grp_ops_abund)
    grp_size(grp = grp,
             calibrate = grp_ops_abund,
             geometric_mean = geometric_mean,
             use_low_if_na = use_low_if_na,
             debug_mode = debug_mode,
             verbose = verbose)

    # Gerrodette =======================
    data(grp_ops_gerrodette)
    grp_ops_gerrodette
    grp_size(grp = grp,
             calibrate = grp_ops_gerrodette,
             geometric_mean = geometric_mean,
             use_low_if_na = use_low_if_na,
             debug_mode = debug_mode,
             verbose = verbose)

  }
  #=============================================================================

  # This function takes a dataframe of DAS data for a single sighting,
  # with columns filtered to those relevant to group size estimation
  # It is called from process_sightings().
  # This function contains all code for estimating group size.
  # The ABUND calibration method is contained within a separate subfunction, grp_calibrate_abund,
  # while the GERRODETTE method is contained within the present function (at least for now).

  ##############################################################################
  ##############################################################################
  # Helper functions

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

  pivot_species <- function(grp){
    grp %>% as.data.frame
    (percs <-
        rbind(data.frame(spp = grp$SpCode1, perc = grp$SpPerc1),
              data.frame(spp = grp$SpCode2, perc = grp$SpPerc2),
              data.frame(spp = grp$SpCode3, perc = grp$SpPerc3),
              data.frame(spp = grp$SpCode4, perc = grp$SpPerc4)))
    return(percs)
  }

  get_spp_max <- function(grp){
    grp %>% as.data.frame
    (percs <- pivot_species(grp))
    (percsumm <-
        percs %>%
        filter(!is.na(spp)) %>% # remove NA species
        group_by(spp) %>% # get mean percent for each species
        summarize(perc = mean(perc, na.rm=TRUE)) %>%
        arrange(desc(perc))) # rank in descending order, w most abundant at top
    (spp_max <- percsumm$spp %>% head(1)) # return most abundant species
    return(spp_max)
  }

  ##############################################################################
  ##############################################################################

  # review input
  grp %>% as.data.frame

  # Stage results
  (sp_mixed <- grp$Mixed %>% unique) # is this a mixed species sighting?
  (sp_prob <- grp$Prob %>% unique) # are IDs probable?
  (sp_n <- grp %>% dplyr::select(nSp) %>% unique %>% as.numeric) # number of species in sighting
  (sp_max <- get_spp_max(grp)) # get max species
  grpnew <- data.frame() # for debugging
  bft <- grp$Bft[1] %>% as.numeric ; bft
  yr <- grp$year[1] %>% as.numeric ; yr
  gs_best_raw <- gs_low_raw <- gs_high_raw <- NA # record of uncalibrated estimates
  gs_best <- gs_low <- gs_high <- NA # final estimate objects

  # Groups size estimates
  (bests <- grp$GsSchoolBest)   # Estimates from each observer...
  (highs <- grp$GsSchoolHigh)
  (lows <- grp$GsSchoolLow)
  best_vars <- rep(NA,times=length(bests)) # stage vector for variance of estimates for each observer

  # Master status variable
  go <- TRUE # will calibration happen?

  #=============================================================================
  # Raw estimates  =============================================================

  if(debug_mode){
    message('Raw estimates:')
    message('--- best = ',paste(bests,collapse=', '))
    message('--- low = ',paste(lows,collapse=', '))
    message('--- high = ',paste(highs,collapse=', '))
  }

  # Simple raw results
  # Arithmetic
  (gs_besti <- mean(bests,na.rm=TRUE))
  if(debug_mode){ message('--- simple arithmetic mean = ',round(gs_besti,2)) }
  if(! geometric_mean){
    gs_best_raw <- mean(bests, na.rm=TRUE)
    gs_high_raw <- mean(highs, na.rm=TRUE)
    gs_low_raw <- mean(lows, na.rm=TRUE)
  }

  # Geometric
  (gs_besti <- geometric_unweighted_mean(bests))
  if(debug_mode){ message('--- geometric unweighted mean = ',round(gs_besti,2)) }
  if(geometric_mean){
    gs_best_raw <- geometric_unweighted_mean(bests)
    gs_high_raw <- geometric_unweighted_mean(highs)
    gs_low_raw <- geometric_unweighted_mean(lows)
  }

  ##############################################################################
  ##############################################################################
  # Calibrate group sizes: which method to use?
  # options: ABUND w calibration, ABUND w/o calibration, & Gerrodette
  # (test that data are available for calibration)
  if(is.null(calibrate)){go <- FALSE}
  if(go){if(is.null(calibrate$coefficients)){go <- FALSE}}
  if(go){if(!is.data.frame(calibrate$coefficients)){go <- FALSE}}
  if(go){ if(!calibrate$method %in% c('ABUND', 'Gerrodette')){go <- FALSE} }
  go
  if(debug_mode){
    if(!go){ message(' --- Calibration data missing. No calibration!') }
  }

  # Gather basic calibration variables (and stage values for final result)
  go_method <- this_beta <- this_intercept <- this_floor <- NA
  coeffs <- NULL
  if(go){
    go_method <- calibrate$method
    coeffs <- calibrate$coefficients
    this_floor <- calibrate$floor
  }
  go ; go_method ; this_floor
  coeffs

  ##############################################################################
  ##############################################################################
  # ABUND method

  if(go & go_method == 'ABUND'){

    # Loop through each observer... ============================================
    obs_i <- 1 # for debugging
    for(obs_i in 1:nrow(grp)){
      # Get this observer's estimates
      (obsi <- grp$ObsEstimate[obs_i])
      (besti <- bests[obs_i])
      (lowi <- lows[obs_i])
      (highi <- highs[obs_i])

      if(FALSE){ # for debugging
        obs = obsi
        gbest = besti
        glow = lowi
        ghigh = highi
        gs_coefficients <- coeffs
        calibrate_floor <- this_floor
      }

      grpi <- grp_calibrate_abund(obs = obsi,
                                  bft = bft,
                                  yr = yr,
                                  gbest = besti,
                                  glow = lowi,
                                  ghigh = highi,
                                  gs_coefficients = coeffs,
                                  calibrate_floor = this_floor)
      grpi # review

      # Validity checks for this observer
      validi <- TRUE
      # Is the resulting best estimate NA?
      if(is.na(grpi$best)){
        validi <- FALSE
        # replace best with low
        grpi$best <- grpi$low_raw
      }
      #  Is the result less than 0?
      if(validi & grpi$best < 0){ validi <- FALSE }
      # if less than 1, coerce to 1 (does not affect validity)
      if(validi & grpi$best < 1){ grpi$best <- 1 }
      validi
      grpi$valid <- validi

      # add to results vectors
      grpnew <- rbind(grpnew, grpi)
    } # end of observer loop
    #===========================================================================

    # Review in debugging mode
    if(debug_mode){
      message('---')
      message('Calibrated estimates:')
      message('--- best estimates = ',paste(round(grpnew$best,3),collapse=', '))
      message('--- variance = ',paste(round(grpnew$var,3),collapse=', '))
      message('--- calibration adjustment? = ',paste(grpnew$calibr,collapse=', '))
      message('--- valid estimates? = ',paste(grpnew$valid,collapse=', '))
      message('---')
      print(grpnew)
      message('---')
    }

    # Filter to only valid estimates
    (grpvalid <- grpnew %>% dplyr::filter(valid == TRUE))
    bests_cal <- best_vars <- NA
    valids <- calibs <- FALSE
    if(nrow(grpvalid)>0){
      (bests_cal <- grpvalid$best)
      (best_vars <- grpvalid$var)
      # track other important qa/qc terms
      (valids <- grpvalid$valid) # best estimate is valid (regardless of calibration)
      (calibs <- grpvalid$calibr)
    }else{
      go <- FALSE
      if(debug_mode){ message(' --- no valid calibrated estimates returned! No calibration!!!') }
    }
    bests_cal
    best_vars
    valids
    calibs
    go

    # Make sure all calibrated estimates were valid
    if(go){if(! all(calibs)){
      go <- FALSE
      if(debug_mode){ message(' --- some calibrated estimates were invalid! Abandoning calibration!!!') }
    }}

    # Geometric weighted mean
    (gs_besti <- geometric_weighted_mean(bests_cal,best_vars))
    (gs_lowi <- geometric_weighted_mean(lows,best_vars))
    (gs_highi <- geometric_weighted_mean(highs,best_vars))
    if(debug_mode){
      message('--- calibrated geometric weighted means (using variance of *best* estimate from calibration for *all three*)')
      message('--- --- best = ',round(gs_besti,2))
      message('--- --- low = ',round(gs_lowi,2))
      message('--- --- high = ',round(gs_highi,2))
    }
    # Keep this estimate, if settings say so
    if(go & geometric_mean){
      gs_best <- gs_besti
      gs_low <- gs_lowi
      gs_high <- gs_highi
    }

    # Geometric UN-weighted means
    # No calibration variance to use as weights -- just use simple geometric mean
    (gs_besti <- geometric_unweighted_mean(bests_cal))
    (gs_lowi <- geometric_unweighted_mean(lows))
    (gs_highi <- geometric_unweighted_mean(highs))
    if(debug_mode){
      message('--- calibrated geometric unweighted means')
      message('--- --- best = ',round(gs_besti,2))
      message('--- --- low = ',round(gs_lowi,2))
      message('--- --- high = ',round(gs_highi,2))
    }
    # Keep this estimate, if settings say so
    if(go & geometric_mean){
      gs_best <- gs_besti
      gs_low <- gs_lowi
      gs_high <- gs_highi
    }

    # Just use arithmetic mean
    (gs_besti <- mean(bests_cal,na.rm=TRUE))
    (gs_lowi <- mean(lows,na.rm=TRUE))
    (gs_highi <- mean(highs,na.rm=TRUE))
    if(debug_mode){
      message('--- simple arithmetic means')
      message('--- --- best = ',round(gs_besti,2))
      message('--- --- low = ',round(gs_lowi,2))
      message('--- --- high = ',round(gs_highi,2))
    }
    # Keep this estimate, if settings say so
    if(go & !geometric_mean){
      gs_best <- gs_besti
      gs_low <- gs_lowi
      gs_high <- gs_highi
    }

  } # end ABUND method

  ##############################################################################
  ##############################################################################
  # Gerrodette method

  if(go & go_method == 'Gerrodette'){

    # stage final values & status objects
    go <- TRUE
    this_beta <- this_intercept <- this_floor <- ss_cal <- NA

    # get geometric mean of observer best estimates
    (ss_tot <- geometric_unweighted_mean(bests))
    if(is.na(ss_tot)){go <- FALSE}

    if(go){
      # get generic values for coefficients
      this_beta <- calibrate$beta
      this_intercept <- calibrate$intercept
      this_floor <- calibrate$floor

      # check for a bespoke mixed beta (will only use if a mixed school)
      beta_mixed_provided <- FALSE
      if(!is.null(calibrate$beta_mixed)){ beta_mixed_provided <- TRUE }

      if(beta_mixed_provided){
        if(sp_mixed){
          this_beta <- calibrate$beta_mixed
          # use generic intercept & floor
        }
      }else{
        if(!sp_mixed){
          # handle non-mixed schools first (simplest case)
          (sppi <- grp$SpCode1 %>% unique %>% head(1))
        }else{
          # handle mixed school -- find most abundant species in group
          (sppi <- get_spp_max(grp)) # this function is defined at top
        }

        # see if species is in calibrate$coefficients
        (coeffs <- calibrate$coefficients) # for simpler code
        # look for match
        (matchi <- which(coeffs$species == sppi))
        # only proceed if the species was found
        if(length(matchi) > 0){
          (coeffi <- coeffs[matchi[1], ])
          # update coeffs with species-specific values, if provided & valid
          if(!is.na(coeffi$beta)){this_beta <- coeffi$beta}
          if(!is.na(coeffi$intercept)){this_intercept <- coeffi$intercept}
          if(!is.na(coeffi$floor)){this_floor <- coeffi$floor}
        }
      }
    } # end of if go

    # check to see if ss_tot is above calibrate_floor
    if(go){if(ss_tot < this_floor){go <- FALSE}}

    # check final coefficient values
    ss_tot ; this_beta ; this_intercept ; this_floor ; go

    # finally, produce calibration estimate
    if(go){
      ss_cal <- exp((log(ss_tot)-this_beta)/this_intercept)
    }
    ss_cal ; ss_tot # compare to raw geometic mean estimate
    #plot(ss_cal ~ ss_tot, type='l') ; abline(a=0,b=1, lty=2)

    # Keep this estimate, if settings say so
    if(go){
      gs_best <- ss_cal
      gs_low <- NA
      gs_high <- NA
    }
  } # end Gerrodette approach

  ##############################################################################
  ##############################################################################
  # Handle NO calibration

  if(!go){
    if(debug_mode){
      message('--- Calibration unsuccessful. Using uncalibrated estimates!')
    }
    gs_best <- gs_best_raw
    gs_high <- gs_high_raw
    gs_low <- gs_low_raw
  }

  ##############################################################################
  ##############################################################################
  # Final validity checks

  go
  go_method
  valids
  gs_best; gs_low; gs_high

  # starting point: all kept observer estimates are valid
  (validi <- all(valids))

  # If best estimate is negative, make it NA
  if(!is.na(gs_best)){ if(gs_best < 0){
    validi <- FALSE
    gs_best <- NA
  }}

  # Is gs_best NA?
  if(is.na(gs_best)){
    validi <- FALSE
    # replace with low estimate?
    if(use_low_if_na){ gs_best <- ifelse(gs_low < 0, NA, gs_low) }
  }

  # If the best group size estimate is still NA, just use 1
  if(is.na(gs_best)){
    validi <- FALSE
    gs_best <- 1
  }

  # Make sure non-na estimates are not less than 1
  if(!is.na(gs_best)){ if(gs_best < 1){
    gs_best <- 1
  }}

  validi; gs_best ; gs_low ; gs_high

  if(debug_mode){
    message('---\nFinal best estimate = ',round(gs_best,2))
    message('Best estimate is valid? = ',validi)
  }

  ##############################################################################
  ##############################################################################
  # Prepare final result

  # Get final list of species with percentages
  (spp_df <-
     pivot_species(grp) %>%
     filter(!is.na(spp)) %>% # remove NA species
     group_by(spp) %>% # get mean percent for each species
     summarize(perc = mean(perc, na.rm=TRUE)) %>%
     arrange(desc(perc))) # rank in descending order, w most abundant at top

  # if there is only one species, force percentage to 100
  if(nrow(spp_df)==1){
    spp_df$perc <- 100
  }

  # Loop through each species and create results row
  i=1 # debugging
  grp_results <- data.frame() # stage results
  for(i in 1:nrow(spp_df)){
    spi <- spp_df[i, ] ; spi # species in this row
    (perci <- spi$perc/100)
    # final validity check -- perc must be valid
    (validi_spi <- ifelse(is.na(perci), FALSE, validi))
    dfi <- data.frame(species = as.character(spi$spp), # species code
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
                      calibr = go, # all(calibs), # was calibration adjustment applied?
                      ss_valid = validi_spi, # is this best estimate valid?
                      spp_max = sp_max,
                      mixed_max = FALSE,
                      geometric_mean = geometric_mean,
                      calibr_method = go_method,
                      calibr_beta = this_beta,
                      calibr_intercept = this_intercept,
                      calibr_floor = this_floor,
                      ss_tot_raw = gs_best_raw,
                      low_raw = gs_low_raw,
                      high_raw = gs_high_raw
    )
    dfi
    grp_results <- rbind(grp_results,dfi)
  }

  # fix cases where best is less than 1 in mixed species after best is scaled by percentage
  grp_results <-
    grp_results %>%
    mutate(best = ifelse(best < 1, 1, best))

  # add mixed-max column, specifying whether a given species is the most abundant one in the group
  grp_results <-
    grp_results %>%
    rowwise %>%
    mutate(mixed_max = ifelse(mixed && species == spp_max, TRUE, mixed_max)) %>%
    ungroup %>%
    as.data.frame

  # review
  grp_results

  return(grp_results)
}
