#' Finalize formatting of a `DAS` object
#'
#' This function removes invalid rows of data (due to missing Cruise numbers, timestamps, location coordinates, etc.),
#' calculates the distance, in kilometers, between each row of data,
#' adds a `ship` column with the ship name associated with the cruise number,
#' and initiates the `cruz` object structure, with a new `cohorts` slot.
#' \cr \cr
#' This is an internal function typically not called by a user directly.
#' It is the third subroutine called within `process_surveys()`; after `process_strata()` and before `segmentize()`.
#'
#' @param cruz The list produced by `process_strata()`, with two slots: `settings` and `das`.
#' @param verbose Boolean, with default `FALSE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A list with nascent `cruz` object structure, with these slots:
#' \enumerate{
#' \item `settings` holds your settings object;
#' \item `strata` holds a dataframe summarizing the name and area (square km) of each geo-stratum, if any are provided;
#' \item `cohorts` holds a list with slots for each cohort of species as specified in `settings`.
#' Each cohort slot has a copy of the `DAS` data with a new `stratum` column,
#' with a stratum assignment tailored to its cohort-specific settings (specifically, the setting `stratum_overlap_handling`).
#' This list structure will be expanded upon in subsequent steps of `process_surveys()`.
#' }
#'
#' @export
#'
das_format <- function(cruz,
                       verbose=FALSE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    data(example_settings)
    settings <- example_settings
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    cruz <- process_strata(das, settings)
    verbose=TRUE

    # Investigate invalid Cruise values
    bads <- which(is.na(das$Cruise))
    bads
    das[bads,]
    das[1:11,]

    # Investigate invalid Cruise values for CNP dataset
    das_file <- '../test_code/eric/cnp/CenPac1986-2020_Final_alb.das'
    das <- das_load(das_file)
    table(das$Cruise, useNA='ifany')
    (bads <- which(is.na(das$Cruise)))
    das %>%
    #das[bads,] %>%
      mutate(year = lubridate::year(das$DateTime)) %>%
      group_by(year) %>%
      summarize(n=n(),
                n_na = length(which(is.na(Cruise))),
                first_na = head(which(is.na(Cruise)),1),
                last_na = tail(which(is.na(Cruise)), 1)) %>%
      filter(n_na > 0)

  }
  #=============================================================================

  # Save copy of data
  dass <- cruz$das
  settings <- cruz$settings
  strata_summary <- cruz$strata

  return_list <- cruz # prepare final list

  # Gather relevant settings
  strata <- settings$strata
  max_row_interval <- settings$survey$max_row_interval

  # Remove invalid cruise numbers  #############################################

  npre <- nrow(dass) # get rows before filtering
  #dass$Cruise %>% table(useNA='always')
  #unique(dass$Cruise)
  bads <- which(is.na(dass$Cruise))
  if(length(bads)>0){
    dass <- dass[-bads,]
  }
  npos <- nrow(dass) # get rows after filtering
  if(verbose){message('--- removed ',npre - npos, ' invalid Cruise numbers ....')}

  # Remove invalid times  ######################################################

  dt <- lubridate::as_datetime(dass$DateTime)
  npre <- nrow(dass)
  dass <- dass[!is.na(dt),]
  npos <- nrow(dass)
  if(verbose){message('--- removed ',npre - npos, ' invalid times ....')}

  # adding  useful date/time columns
  dass$year <- lubridate::year(dass$DateTime)
  dass$month <- lubridate::month(dass$DateTime)
  dass$day <- lubridate::mday(dass$DateTime)
  dass$yday <- lubridate::yday(dass$DateTime)

  # Remove invalid locations  ##################################################

  npre <- nrow(dass)
  dass <- dass %>% dplyr::filter(!is.na(Lon) & !is.na(Lat))
  npos <- nrow(dass)
  if(verbose){message('--- removed ',npre - npos, ' invalid locations ....')}


  # Calculate distances between each row of data  ##############################
  # this uses as LTabundR function

  if(verbose){message('--- calculating distances ...')}
  km <- process_km(dass,
                   min_interval = 30, # only calculate distances b/w entries 30 sec apart or more
                   max_interval = Inf, # assume entries this long apart represent a gap in effort
                   replacement_interval = .1667, # for large intervals, what interval to assume (10 minutes)
                   max_km_gap = 30, # any km gap beyond this value will be replaced
                   max_km_replace = 0, # replacement value
                   debug_mode = FALSE)

  # debugging code
  #message(round(difftime(Sys.time(),tstart,units='secs'))," seconds : ",round(sum(km))," km")
  #hist(km, breaks=seq(0,max(km)+1,by=.5))
  #hist(km, breaks=seq(0,max(km)+1,by=.5), xlim=c(0,100), ylim=c(0,1000))
  #hist(km, breaks=seq(0,max(km)+1,by=.5), xlim=c(0,30), ylim=c(0,1000))

  # Add columns to data
  dass$km_int <- km # km between rows
  dass$km_cum <- cumsum(dass$km_int) # cumulative distance

  # Add ship variable  #########################################################

  if(verbose){message('--- finding ship name for each cruise number...')}
  ships_use <- settings$survey$ship_list
  ships_use
  # If a ship list was not provided in settings, use the built-in dataset to search for a ship name match
  if(is.null(ships_use)){
    data(ships)
    ships_use <- ships
  }
  ships_use %>% tail # review
  ships_use$comment <- NULL # get rid of this column in the ships dataset

  # Now find the matching ship name for each cruise name present in the data
  das_ship <- rep(NA,times=nrow(dass))
  cruises <- dass$Cruise %>%  unique
  cruises
  i=15
  for(i in 1:length(cruises)){
    cruisi <- cruises[i]
    shipi <- ships$ship[ships$cruise == cruisi] %>% unique %>%  head(1); shipi
    if(length(shipi)>0){
      matches <- which(dass$Cruise == cruisi)
      if(length(matches)>0){
        das_ship[matches] <- rep(shipi, times=length(matches))
      }
    }
  }

  # Add this ship names as a column
  dass$ship <- das_ship


  # Assign a stratum to each row ###############################################
  # Handle stratum overlap according to settings

  # Get overlap handling setting for each cohort
  cohorts <- settings$cohorts
  length(cohorts)
  overlap_i <- which(names(cohorts[[1]]) == 'strata_overlap_handling')
  overlap_i
  overlap_handling <- lapply(cohorts,'[[',overlap_i)
  overlap_handling <- lapply(overlap_handling,'[[',1) %>% unlist
  overlap_handling

  # Update final list by staging a slot for cohorts
  return_list$cohorts <- list()
  return_list

  # The dass dataset comes with a column for each stratum with a decision (0 or 1) in each row,
  # Use that and the strata overlap handling setting to assign each row to a single stratum based on the settings in each cohort

  if(verbose){message('--- preparing stratum assignments for each cohort ...')}

  length(cohorts)
  overlap_handling

  i=2 # for debugging
  # Loop through each cohort
  for(i in 1:length(cohorts)){
    cohorti <- cohorts[[i]] ; cohorti %>% names # get cohort data
    cohorti$id
    if(verbose){message('     --- cohort "',cohorti$id,'" ...')}

    # Subset desired strata
    (cohort_strata <- cohorti$strata) #%>% print
    if(is.null(cohort_strata)){
      (cohort_strata <- names(settings$strata))
    }
    cohort_strata

    # Save copy of DAS to make it cohort-specific
    eff_cohort <- dass

    # Remove strata columns from das that do not pertain to this cohort
    if(!is.null(cohort_strata)){
      cohort_strata
      eff_cohort %>% names
      (stratum_cols <- grep('stratum_',names(eff_cohort),fixed=TRUE))
      stratum_keeps <- c()
      si <- 5
      for(si in 1:length(cohort_strata)){
        (strati <- cohort_strata[si])
        (strati_col <- grep(strati, names(eff_cohort), fixed=TRUE)[1])
        stratum_keeps <- c(stratum_keeps, strati_col)
      }
      stratum_keeps
      (removes <- stratum_cols[ ! stratum_cols %in% stratum_keeps ] )
      if(length(removes) > 0){
        eff_cohort <- eff_cohort[, -removes]
      }
    }
    names(eff_cohort)

    #Prepare a results vector to eventually be a new column in the cohort's das
    stratum <- rep('none',times=nrow(eff_cohort))

    # Only do this if strata were input in the function call
    if(!is.null(cohort_strata)){
      stratum <- rep('out',times=nrow(eff_cohort))

      # Arrange strata summary from largest area to smallest
      strata_summari <-
        strata_summary %>%
        dplyr::filter(stratum %in% cohort_strata) %>%
        dplyr::arrange(dplyr::desc(as.numeric(area)))
      strata_summari

      # What is the strata overlap handling policy for this cohort?
      handling <- overlap_handling[i] ; handling

      # Assign a single stratum based on overlap handling
      if(handling == 'smallest'){
        # Go from largest to smallest stratum
        # in doing so, the final assignment for each row will be the smallest stratum it is contained within
        strat_i=1
        for(strat_i in 1:nrow(strata_summari)){
          strati <- strata_summari$stratum[strat_i] ; strati
          strati_col <- grep(strati, names(eff_cohort), fixed=TRUE)[1]
          strati_ins <- which(eff_cohort[,strati_col])
          stratum[strati_ins] <- strati
        }
      }
      if(handling == 'largest'){
        # Go from smallest to largest stratum
        # in doing so, the final assignment for each row will be the largest stratum it is contained within
        strat_i=1
        for(strat_i in rev(1:nrow(strata_summari))){
          strati <- strata_summari$stratum[strat_i] ; strati
          strati_col <- grep(strati, names(eff_cohort), fixed=TRUE)[1]
          strati_ins <- which(eff_cohort[,strati_col])
          stratum[strati_ins] <- strati
        }
      }
      if(handling == 'each'){
        # For each row, concatenate the names of the strata containing it. Separate names with `&`.
        # Use this concatenated name as the final stratum assignment.
        strati_col <- grep('stratum', names(eff_cohort), fixed=TRUE)
        strati_names <- names(eff_cohort)[strati_col]
        strati_names <<- gsub('stratum_','',strati_names)
        strati_df <- eff_cohort[,strati_col]
        stratum <-
          apply(strati_df,1,function(x){
            paste(unique(strati_names[x]),collapse='&')
          })
      }

    } # end of if strata is null

    # review results
    table(stratum) #%>% print

    # add stratum vector as column to cohorts' das
    eff_cohort$stratum <- gsub('stratum_','',stratum)

    # Add cohort to results list
    return_list$cohorts$new <- eff_cohort
    names(return_list$cohorts)[length(return_list$cohorts)] <- gsub(' ','_',cohorti$id)

  } # end of cohort loop


  # final polish & review ######################################################
  return_list$das <- NULL # remove the raw data from the list
  return_list %>% names
  return_list$cohorts %>% names

  return(return_list)
}
