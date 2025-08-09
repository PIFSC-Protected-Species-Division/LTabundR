#' Process survey sightings
#'
#' This is an internal function typically not called by a user directly.
#' It is the fifth subroutine called within `process_surveys()`, after `segmentize()` and before `process_subgroups()`.
#'
#' @param cruz A `cruz` object passed from `segmentize()`.
#'
#' @param calibrate An argument allowing you to override the settings contained within the `cruz` object.
#' This argument accepts a Boolean; if `TRUE`, school size calibration will be attempted,
#' but only if calibration coefficients are provided in `cruz$settings$survey`.
#' Note that only the best estimates of schol size will be calibrated;
#' the high and low estimates are *never* calibrated.
#'
#' @param calibrate_floor Another argument allowing for settings override.
#' This argument accepts a number indicating the minimum raw school size estimate
#' for which school size calibration will be attempted.
#'
#' @param geometric_mean Another argument allowing settings override.
#' This argument accepts a Boolean;
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
#' @param verbose Boolean, with default `FALSE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A modified `cruz` object, in which a `sightings` dataframe
#' (one row for each species in each sighting, with school size estimates averaged and calibrated according to cohort-specific `settings`)
#' has been added to each `<cohort>` list. The `sightings` dataframe has a new column, `included`, indicating whether or not
#' a sighting will be included in an analysis according to the inclusion criteria specified in cohort-specific `settings`,
#' as well as a new column, `ss_valid`, indicating whether or not the school size estimate
#' for this sighting is valid and appropriate for use in abundance estimation and/or detection function fitting
#' with a school-size covariate. All columns are described in detail within the
#' documentation for the `LTabundR` function, `process_surveys()`.
#'
#' @export
#'
process_sightings <- function(cruz,
                              calibrate = NULL,
                              calibrate_floor = NULL,
                              geometric_mean = NULL,
                              verbose=TRUE){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    data(example_settings)
    settings <- example_settings
    das_file = c("/Users/ekezell/Desktop/projects/noaa ltabundr/swfsc_1986_2020.das")
    #das_file <- '../test_code/eric/cnp/CenPac1986-2020_Final_alb.das'
    #das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    cruz <- process_strata(das, settings)
    cruz <- das_format(cruz)
    cruz <- segmentize(cruz)
    verbose=TRUE
    calibrate = TRUE
    calibrate_floor = 0
    geometric_mean = NULL
    cohorts_i = 1

    # test function
    sits <- process_sightings(cruz, verbose=TRUE)
    sits$cohorts$default$sightings %>% dplyr::filter(ss_valid == FALSE)
    sits <- process_sightings(cruz, calibrate=FALSE, verbose=FALSE)

    grp_check <- function(cruz){
      which(cruz$cohorts[[1]]$sightings$high < cruz$cohorts[[1]]$sightings$best)
    }

    # No problems
    grp_check(sits) # well only two
    sits$settings$survey[1:11]
    sits$settings$survey$group_size_coefficients
    sits$settings$cohorts[[1]]

    # problems
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    grp_check(cruz)

    cruz$settings$survey %>% names
    cruz$settings$survey$group_size_coefficients
    cruz$settings$cohorts[[1]]

  }
  #=============================================================================

  # save a safe copy of the input data
  cruzi <- cruz

  # Loop through each cohort
  for(cohorts_i in 1:length(cruzi$cohorts)){
    cohorti <- cruzi$cohorts[[cohorts_i]] # get cohort data
    cohort_settings <- cruzi$settings$cohorts[[cohorts_i]] # get cohort settings

    ani <- cohorti # rename for convenience
    ani %>% names

    if(verbose){message('Cohort "',names(cruzi$cohorts)[cohorts_i],
                        '": processing sightings ...')}

    ##########################################################################
    # Begin by using swfscDAS function

    # Get sightings using swfscDAS function
    if(verbose){message('--- Harvesting sightings ...')}
    suppressWarnings({ suppressMessages({
      sits <- swfscDAS::das_sight(ani$das, return.format = 'complete')
      #sits <- swfscDAS::das_sight(ani$das %>% dplyr::select(1:40),
      #                            return.format = 'complete')
    }) })

    ##############################################################################
    # Gather settings

    # Survey-wide
    sets <- cruzi$settings$survey
    names(sets)
    species_codes <- sets$species_codes
    group_size_coefficients <- sets$group_size_coefficients
    smear_angles <- sets$smear_angles
    random_seed <- sets$random_seed

    # Cohort settings
    sets <- cohort_settings
    names(sets)
    id <- sets$id
    species <- sets$species
    probable_species <- sets$probable_species
    sighting_method <- sets$sighting_method
    cue_range <- sets$cue_range
    school_size_range <- sets$school_size_range
    school_size_calibrate <- sets$school_size_calibrate
    calibrate_floor_set <- sets$calibration_floor
    use_low_if_na <- sets$use_low_if_na
    abeam_sightings <- sets$abeam_sightings
    io_sightings <- sets$io_sightings
    geometric_mean_group <- sets$geometric_mean_group
    truncation_km <- sets$truncation_km

    # Modify calibration plan according to cohort-specific setting
    if(!school_size_calibrate){group_size_coefficients <- NULL}

    # Adjust settings based on inputs
    if(!is.null(calibrate)){
      if(!calibrate){group_size_coefficients <- NULL}
    }
    if(!is.null(geometric_mean)){
      if(!geometric_mean){geometric_mean_group <- FALSE}
    }
    if(is.null(calibrate_floor)){
      calibrate_floor <- calibrate_floor_set
    }

    ##############################################################################
    # Filter to species included in this cohort

    if(verbose){message('--- filtering to sightings with species of interest ...')}
    sits_sp <- sits # safe copy of sightings

    species # review
    if(!is.null(species)){ # if the cohort is for certain species only
      keep_sits <- c() # stage vector of which sightings to keep

      # Format input codes provided
      species <- stringr::str_pad(species,width=3,side='left',pad='0')

      # Find the rows with species codes within those for this cohort
      test1 <- sapply(sits_sp$SpCode1, function(x){ifelse(is.na(x),0,length(grep(x,species)))})
      keep_sits <- c(keep_sits, which(test1 > 0))

      test2 <- sapply(sits_sp$SpCode2, function(x){ifelse(is.na(x),0,length(grep(x,species)))})
      keep_sits <- c(keep_sits, which(test2 > 0))

      test3 <- sapply(sits_sp$SpCode3, function(x){ifelse(is.na(x),0,length(grep(x,species)))})
      keep_sits <- c(keep_sits, which(test3 > 0))

      test4 <- sapply(sits_sp$SpCode4, function(x){ifelse(is.na(x),0,length(grep(x,species)))})
      keep_sits <- c(keep_sits, which(test4 > 0))

      keep_sits <- unique(keep_sits)
      length(keep_sits)

      # Find the corresponding SightNoDaily for these rows
      sitno_keep <- sits_sp$SightNoDaily[keep_sits]

      # Subset to rows pertaining to these SightNoDaily
      sits_sp <- sits %>% dplyr::filter(SightNoDaily %in% sitno_keep)

    }
    nrow(sits_sp)

    ##############################################################################
    # Process sightings for this cohort

    # stage results
    new_sits <- data.frame()

    # loop through each sighitng
    sitno <- unique(sits_sp$SightNoDaily) # unique sighting numbers
    (sitno <- sitno[!is.na(sitno)])

    # Stage progress
    iprogs <- seq(0.05,0.95,by=.05)*length(sitno)
    iprogs <- round(iprogs) ; iprogs

    #sits_sp %>% names
    #sits_sp %>% filter(year == 2008, month==10, day == 31) %>% select(DateTime, SightNoDaily) %>% head(20)

    #which(sitno == "19861128_1") # debugging
    which(sitno == '20120513_20')
    which(sitno == '20081031_650')
    i=25477
    #i=641 # for debugging
    debugger <- c()
    for(i in 1:length(sitno)){

      # Get status
      if(i %in% iprogs){
        if(verbose){message('--- progress: ',round(i/length(sitno),2)*100,'% complete ...')}
      }

      sitno_i <- sitno[i] ; sitno_i # this sighting number
      siti <- sits_sp %>% dplyr::filter(SightNoDaily == sitno_i) ; siti # data for this sighting number
      (line_num_i <- as.character(siti$line_num[1]))
      (dti <- as.character(siti$DateTime[1]))
      (eventnum_i <- as.character(siti$EventNum[1]))

      # If we can use probable identifications, update SpCodes:
      if(probable_species){
        siti %>% dplyr::select(SpCode1:SpCode4)
        siti %>% dplyr::select(SpCodeProb1:SpCodeProb4)
        for(sii in 1:nrow(siti)){
          if(!is.na(siti$SpCodeProb1[sii])){
            siti$SpCode1[sii] <- siti$SpCodeProb1[sii]
          }
          if(!is.na(siti$SpCodeProb2[sii])){
            siti$SpCode2[sii] <- siti$SpCodeProb2[sii]
          }
          if(!is.na(siti$SpCodeProb3[sii])){
            siti$SpCode3[sii] <- siti$SpCodeProb3[sii]
          }
          if(!is.na(siti$SpCodeProb4[sii])){
            siti$SpCode4[sii] <- siti$SpCodeProb4[sii]
          }
        }
        siti
      }

      # Gather common variables to be used downstream
      (siti_line <- unique(siti$line_num)[1])
      siti_s <- siti %>% dplyr::filter(Event == 'S') # 'S' row for this sighting number
      siti_s
      (brng <- unique(siti_s$Bearing)[1])
      (perpdist <- unique(siti_s$PerpDistKm)[1])
      (radialdist <- unique(siti_s$DistNm)[1] * 1.852)

      # get dataframe with all columns relevant to group size estimation
      (grp <- siti %>%
          dplyr::filter(Event == 'S') %>%
          dplyr::select(Event, year, Bft, Prob:GsSchoolLow)) %>% as.data.frame

      if(nrow(siti_s)>0){
        #stage inclusion variables. These assumptions will be tested/modified by downstream tests.
        included <- TRUE
        print_reason <- TRUE
        reason_excluded <- 'included'

        # Step through inclusion tests.
        # Details are provided in Settings in vignette.

        # Check on-transect effort
        siti_s
        (effs <- siti_s$use)
        (effs <- any(effs))
        effs[is.na(effs)] <- FALSE
        if(all(included, !effs)){
          included <- FALSE
          reason_excluded <- 'Effort does not meet inclusion criteria'
          print_reason <- FALSE
          #print(reason_excluded)
        }

        # Smear angles
        if(smear_angles == TRUE | smear_angles != 0){
          smear_angles
          radialdist
          brng
          perpdist
          #set.seed(random_seed)
          (brng_new <- brng + runif(1,-5,5)) # smear angle
          (brng <- brng_new %>% abs)
          (perpdist <- sin((brng*pi/180)) * radialdist) # recalculate perpdist
        }

        # Method (25x v 7x etc)
        # This code is complicated because the WinCruz input codes differ from the
        # ABUND9 settings codes.
        #
        # Method codes for exclusion (in settings)
        # 0=any method,
        # 1=with 25X only,
        # 2=not with 25x and not from from helo, ie naked eyes or 7x
        # anything else goes off effort
        #
        # Method codes in WinCruz data entry
        # 1 = naked eye
        # 2 = 7x or 10x
        # 4 = 25x
        # 5 = unknown but not 25x
        # 6 = other/unknown
        # 7 = helicopter
        #
        sighting_method # settings method
        methods <- unique(siti_s$Method)[1] # data entry method
        methods
        methods_good <- FALSE
        # compare setings method to data entry method
        if(sighting_method == 0){methods_good = TRUE}
        if(sighting_method == 1 & methods == 4){methods_good = TRUE}
        if(sighting_method == 2 & methods %in% c(1,2,5,6)){methods_good = TRUE}
        methods_good
        if(all(included, !methods_good)){
          included <- FALSE
          reason_excluded <- 'Invalid method code'
          #print(reason_excluded)
        }

        # Cue
        cue_range
        cue_i <- unique(siti_s$Cue)[1]
        cue_i
        cue_good <- cue_i %in% cue_range
        cue_good
        if(all(included, !cue_good)){
          included <- FALSE
          reason_excluded <- 'Invalid cue code'
          #print(reason_excluded)
        }

        # Independent Observer (IO)
        io_sightings # settings method
        io_good <- FALSE
        (io <- unique(as.character(siti_s$ObsInd))[1]) # who is the IO in this sighting?
        (obs_i <- unique(siti_s$Obs)[1]) # who observed the sighting?
        (sit_by_io <- !is.na(io) && io == obs_i) # was the sighting from the IO?
        if(all(sit_by_io, io_sightings == '-1')){io_good <- TRUE} # -1 = include IO sightings with all others
        if(all(!sit_by_io,  io_sightings == '0')){io_good <- TRUE} # 0 = ignore IO sightings
        if(all(sit_by_io,  io_sightings == '0')){io_good <- FALSE} # 0 = ignore IO sightings
        if(all(sit_by_io,  io_sightings == '2')){io_good <- TRUE} # 2 = include only sighting made by IO
        if(all(!sit_by_io, io_sightings == '2')){io_good <- FALSE} # 2 = include only sighting made by IO
        if(all(!sit_by_io, io_sightings == '1', !is.na(io))){io_good <- TRUE}  # 1 = use only sightings made by regular team WHEN IOs are present,
        io_good
        if(all(included, !io_good)){
          included <- FALSE
          reason_excluded <- paste('IO policy not met. IO = ',io,'; Obs = ',obs_i)
          #print(reason_excluded)
        }

        # Standard observer?
        # Check to see if the observer of the sighting is one of those formally on effort
        obs_good <- siti_s$ObsStd[1]
        if(all(included, !obs_good)){
          included <- FALSE
          reason_excluded <- paste('Observer not standard. ObsStd = ',siti_s$ObsStd[1])
          #print(reason_excluded)
        }

        # Truncation distance
        truncation_km
        truncation_good <- FALSE
        if(!is.na(perpdist)){
          truncation_good <- perpdist < truncation_km
        }
        truncation_good
        if(all(included, !truncation_good)){
          included <- FALSE
          reason_excluded <- paste0('Beyond truncation km (',truncation_km,') = ',round(perpdist,1),' km')
          #print(reason_excluded)
        }

        # Past beam?
        if(! abeam_sightings){
          beam_good <- brng <= 90 | brng >= 270
          beam_good
          if(all(included, !beam_good)){
            included <- FALSE
            reason_excluded <- paste0('aft of beam: ',round(brng),' degrees')
            #print(reason_excluded)
          }
        }

        # Estimate group sizes, including calibration coefficients
        # output will have one row for each species in detection:
        grp %>% as.data.frame # review grp dataframe
        # group_size() is a LTabundR function. See 'group_size.R'.
        grp_results <- LTabundR::group_size(grp,
                                            gs_coefficients = group_size_coefficients,
                                            calibrate_floor = calibrate_floor,
                                            geometric_mean = geometric_mean_group,
                                            use_low_if_na = use_low_if_na)
        grp_results

        if(FALSE){
          # for debugging
          gs_coefficients = group_size_coefficients
          calibrate_floor = calibrate_floor
          geometric_mean = geometric_mean_group
          use_low_if_na = use_low_if_na
        }

        # That function incorporates 3 settings:
        # group size coefficients
        # geometric_mean_group and
        # use_low_if_best_na: if the Best group size estimate is NA,
        # mean group size will be calculated from low estimates.
        # This should only be done if NO observer has a best estimate

        # That function returns a dataframe with one row for each species in the sighting (if multi-species)
        # (the vast majority of sightings are single-species, meaning the grp output will be nrow=1)

        # Flag invalid group size estimate
        if(any(grp_results$ss_valid == FALSE)){
          # If any best estimates are valid/finite, flag the the school size is not valid.
          message('--- DateTime = ', dti,', line_num = ',line_num_i, 'i = ', i,' | Species ', paste(grp_results$species, collapse=', '), ' | Some group size best estimates are NOT valid! *****')
        }

        # First add an indicator if this species is the largest part of the mixed school
        grp_results$mixed_max <- FALSE
        grp_results$spp_max <- NA
        if(any(grp_results$ss_valid)){
          max_best <- max(grp_results$best, na.rm=TRUE)
          # Is it the max species?
          grp_results$mixed_max[grp_results$best == max_best] <- TRUE
          # Who is the max species?
          spp_max <- grp_results$species[which(grp_results$best == max_best)[1]]
          grp_results$spp_max <- spp_max
        }

        grp_results

        # Take these results, loop through each species and finish up.
        # Write a results row for each species in the group.
        grp_i=1 # for debugging
        # Loop through each species in the sighting
        for(grp_i in 1:nrow(grp_results)){
          grpi <- grp_results[grp_i,] # group size estimates for this species
          grpi

          included_i <- included # store species-specific version of inclusion status, so that loop does not confound
          reason_i <- reason_excluded # same

          # species code valid?
          sp_found <- TRUE
          if(!is.null(species)){(sp_found <- grpi$species %in% species)}
          if(included_i & !sp_found){
            included_i <- FALSE
            reason_i <- paste0('Species codes not recognized: ',grpi$species)
          }

          # School size range
          school_size_range
          group_good <- grpi$best[1] >= school_size_range[1] & grpi$best[1] <= school_size_range[2]
          if(is.na(group_good)){group_good <- FALSE}
          if(all(included_i, !group_good)){
            included_i <- FALSE
            reason_i <- paste0('Group size out of range (',paste(school_size_range,collapse='-'),'): ',round(grpi$best[1]))
            debugger <- c(debugger, i)
            #print(reason_i)
          }

          # Compile final df
          # Get rows for event 'S'
          siti_final_S <- (siti %>%
                             dplyr::filter(Event == 'S') %>%
                             dplyr::select(Event:Biopsy, CourseSchool:PerpDistKm))[1,]
          siti_final_S
          siti_final_S <- data.frame(siti_final_S, grpi)

          # Get rows for resighings (if any)
          siti_final_s  <- (siti %>%
                              dplyr::filter(Event %in% c('s','K','M','k')) %>%
                              dplyr::select(Event:Biopsy, CourseSchool:PerpDistKm))[1,]
          siti_final_s
          siti_final_s <- siti_final_s[!is.na(siti_final_s$Event),]

          # Bind into final dataframe
          if(nrow(siti_final_s)>0){
            siti_final_s <- data.frame(siti_final_s, grpi)
            siti_final <- rbind(siti_final_S,
                                siti_final_s)
          }else{
            siti_final <- siti_final_S
          }

          # Add refined variables and included column
          siti_final$Bearing <- brng
          siti_final$PerpDistKm <- perpdist
          siti_final$included <- included
          head(siti_final)

          # If excluded, print a reason (if verbose == TRUE)
          if(!included_i){
            if(verbose & print_reason){
              message('--- SightNo = ',sitno_i,
                      ', SpCode = ',grp_i,
                      ' (DateTime = ', dti,
                      ', line_num = ',line_num_i,
                      ', i = ',i,') | Excluded:  ',reason_i,'.')
            }
          }

          # Add this sighting to results table
          new_sits <- rbind(new_sits, siti_final)

        } # end of loop for each species in the group
      } # end of if nrow siti_s > 0
    } # end of sighting loop

    # Add sightings table to new slot in cohort's list
    cruzi$cohorts[[cohorts_i]]$sightings <- new_sits

    if(verbose){message('---\n---')}
  }

  return(cruzi)
}
