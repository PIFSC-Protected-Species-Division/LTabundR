#' Process geo-strata
#'
#' Determine in/out status of `DAS` events for each geo-strata provided by `settings`.
#' This function loops through each stratum dataframe you have provided it in `settings$strata`,
#' formats the stratum, and asks whether each DAS row occurs within it.
#' \cr \cr
#' This is an internal function typically not called by a user directly.
#' It is the second subroutine called within `process_surveys()`, after `load_das()` and before `format_das()`.
#'
#' @param das  A `data.frame` of a `DAS` survey data file, created by `load_das()`.
#' @param settings A `settings` object, created by `load_settings()`.
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A list with two named slots: [[1]] `settings` and [[2]] `das`.
#' The `das` slot has the dataframe of your survey data.
#' For each stratum, a column named `stratum_<StratumName>` is added to the `das` dataframe;
#' each row in this column is `TRUE` (included) or `FALSE`.
#'
#' @export
#'
process_strata <- function(das,
                           settings,
                           verbose=TRUE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    data(example_settings)
    settings <- example_settings
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das <- das_load(das_file)
    das <- dasi
    verbose=TRUE
  }
  #=============================================================================

  effi <- das # save copy of input data
  settings # review for debugging

  # Harvest relevant settings
  (strata <- settings$strata)

  # Get out handling (what to do if a row is not in any stratum)
  out_handling <- settings$survey$out_handling[1]

  # Handle locations that cross the Int'l Date Line
  # If effort spans the dateline, coerce all longitudes to be negative.
  bads <- which(effi$Lon > 0)
  negs <- which(effi$Lon < 0)
  if(length(negs)>0 & length(bads) > 0){
  #if(length(bads)>0){
    effi$Lon[bads] <- -180 + (effi$Lon[bads] - 180)
  }

  # Survey strata ##############################################################

  if(is.null(strata)){
    # If no strata are provided....
    strata_summary <- NULL
    #effi$stratum_none <- TRUE
    #effi$stratum <- 'none'
  }else{

    # Stage summary table
    strata_summary <- data.frame()

    if(verbose){message('Testing whether each DAS line is in each stratum ...')}
    i=1
    for(i in 1:length(strata)){
      strati <- strata[i] # get dataframe of coordinates for this stratum
      sname <- names(strati) # get name of stratum
      strati
      if(verbose){message('--- Stratum ',i,' of ',length(strata),' :: ',sname,' ...')}

      suppressWarnings({suppressMessages({
        (polygon_dataframe <- strata[[1]])
        poli_list <- process_polygon(strati[[1]]) # convert from df to polygon (also returns area in km2)
        poli <- poli_list$sf # add sf object to results
        #names(poli_list)
        #st_bbox(poli)
        #plot(poli)

        # Add row to summary
        summi <- data.frame(stratum = sname, area = poli_list$km2)
        strata_summary <- rbind(strata_summary, summi)

        # Process das
        dasloc <- effi %>% dplyr::filter(!is.na(Lon) & !is.na(Lat)) # remove invalid locations
        # the coordinates_in_strata() function is a LTabundR function
        strata_das <- coordinates_in_strata(lon = dasloc$Lon, lat = dasloc$Lat, poli=poli)
      })})
      # returns a decision for every coordinate: 1 = in stratum, 0 = not
      table(strata_das, useNA='ifany') #checkout result (debugging)
      dasloc$new_var <- strata_das # Save these decisions in a new column
      dasloc <- dasloc %>%
        dplyr::select(line_num, new_var) # create a simple df for joining
      effi <- dplyr::left_join(effi, dasloc, by='line_num') # join new column to das
      table(effi$new_var, useNA='ifany') # review (debugging)
      # rename the new column using the stratum name
      names(effi)[which(names(effi)=='new_var')] <- paste0('stratum_',sname)
      head(effi)
    }

    # Handle OUTS -- events that do not occur within a geo-stratum
    if(out_handling == 'remove'){
      (stratum_cols <- grep('stratum',names(effi)))
      #as.data.frame(effi[,stratum_cols])
      ins <- apply(as.data.frame(effi[,stratum_cols]),1,any)
      #ins
      ins %>% table
      #length(ins)
      effi <- effi[which(ins),]
    }

  } # end of if strata are provided


  # Prepare final list #########################################################

  return_list <- list()
  return_list$das <- effi
  return_list$settings <- settings
  return_list$strata <- strata_summary
  return_list

  return(return_list)
}
