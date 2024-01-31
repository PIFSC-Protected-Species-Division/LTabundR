#' Process & prepare `Wincruz` survey data for analysis.
#'
#' This function is the main command you will use to begin working with your survey data in `R`.
#' It takes `Wincruz` survey data -- `.DAS` file(s) -- and user-specified settings
#' to prepare the data for density estimation and/or habitat modeling analyses.
#' \cr \cr
#' This function was designed to be similar to `ABUND9`, the Fortran program
#' written by Jay Barlow (NOAA SWFSC) for the same purpose.
#'
#' @param das_file  One or more filenames/filepaths to DAS files with survey data,
#' supplied as a character vector. If multiple files are supplied, each file will be
#' processed separately then combined using `LTabundR::cruz_combine()`. URL's for
#' online `DAS` reposities are also accepted.
#'
#' @param settings  An object representing the output of `load_settings()`. See that function's documentation for details.
#'
#' @param edits An optional list of staged edits for modifying the `DAS` data (after reading in; not the actual data files themselves)
#' before proceeding with data processing. These edits must take the form of the input for
#' the `LTabundR` function `das_editor()` (see its documentation for details).
#' If `edits` are supplied, a temporary version of the `DAS` data will be created
#' (this temporary version will then be deleted at the end of this function's procedure.)
#'
#' @param delete_edited_files A Boolean, with default `TRUE`, indicating that any edited versions of `DAS` data, which are saved in new files by the function,
#' should be deleted at the end of the function's processing steps. This (1) reduces the file size of your project and (2) ensures that there is only ONE
#' version of your data in existence (the original), and that any updates to it are saved in the list you provide to the `edits` input.
#'
#' @param process_sightings A Boolean, with default `TRUE`, indicating whether or not sightings should be processed in addition to effort.
#' When troubleshooting effort segments, it could be useful to set this to `FALSE` to expedite processing time.
#'
#' @param process_subgroups A Boolean, with default `TRUE`, indicating whether or not subgroups should be found in the survey data and analyzed.
#'
#' @param save_local If TRUE (not the default), the resulting `cruz` object
#' will be saved in the current working directory as a `.RData` file. This may be
#' advantageous, so that this function only needs to be run once in order to have the
#' data formatted and ready for analysis. The `.RData` filename will be the same as `das_file`
#' (or the first one provided, if multiple are input),
#' except with a different extension.
#'
#' @details This function is a wrapper for several subroutines, which are executed in the following order:
#' \enumerate{
#' \item Read and format the survey data contained in your `DAS` file using functions developed in the package `swfscDAS`
#' (this step is carried out using the internal `LTabundR` function `load_das()`).
#' \item Interpolate the `DAS` data, if instructed by settings. See `load_survey_settings()` for details.
#' \item Determine which `DAS` events occur within the geo-strata
#' provided by the user (using the internal `LTabundR` function `process_strata()`).
#' \item Remove invalid entries, determine the ship used in each cruise,
#' calculate the distance transited between each `DAS` row, and initiate the
#' data structure for the eventual `cruz` object output.
#'  (using the internal `LTabundR` function `format_das()`).
#' \item Parse survey effort into "segments", which are used in variance estimation,
#' and determine which segments should be included in the analysis based upon user-specified settings
#'  (using the internal `LTabundR` function `segmentize()`).
#' \item Process sightings (optional) by determining which should be included in the analysis based upon user-specified settings,
#' and refine school size estimates by calibrating observer estimates and averaging estimates from multiple observers.
#'  (using the internal `LTabundR` function `process_sightings()`).
#' \item Process subgroup size estimates (optional) for false killer whales
#'  (using the internal `LTabundR` function `process_subgroups()`).
#'  \item The above process is repeated for each `DAS` file; if there are multiple
#'  `DAS` files, the results are combined using `LTabundR::cruz_combine()`.
#'  \item Optionally save the result as an `RData` object to easily pass the processed data to other `R` scripts.
#' }
#'
#' @return A `cruz` object, which is a nested list with the following primary slots:
#' \enumerate{
#' \item `settings`, containing the `settings` list you provided as an argument.
#' \item `strata`, containing a dataframe summarizing the geostrata provided (their name and area, in square km).
#' \item `cohorts`, containing a named list for each cohort you specified within the `settings` argument.
#' }
#' Each cohort slot has a similar structure:
#' \cr \cr
#' `cruz$cohorts$<cohort>$<details>`
#' \cr \cr
#' \itemize{
#' \item The name of each `<cohort>` slot is drawn from the `id` slot within that cohort's settings (e.g., `settings$cohorts[[1]]$id`).
#' \item All cohorts have the same three slots for `<details>`:
#' \enumerate{
#' \item `segments`, a `data.frame` with metadata for each effort segment (see below).
#' \item `das`, a `data.frame` of the survey data (see below).
#' \item `sightings`, a `data.frame` with details for each sighting (see below).
#' }
#' \item Some cohorts may also have a fourth `<details>` slot, `subgroups`,
#' if subgroups were found in the data for the species specified in the cohort.
#' This slot will contain a list (see below).
#' }
#' \cr
#' **`segments`** data structure \cr
#' A `data.frame` with metadata for each segment; each row is a segment.
#' \enumerate{
#' \item `Cruise`: Cruise number
#' \item `ship`: Ship name initials
#' \item `stratum`: Stratum designation for this segment
#' \item `seg_id`: Unique segment identifier
#' \item `yday`: Numeric day of year
#' \item `dist`: Distance surveyed in this segment
#' \item `lat1`: Start latitude of segment, decimal degrees
#' \item `lon1`: Start longitude of segment, decimal degrees
#' \item `DateTime1`: Date and time for start of segment, formatted as a `lubridate::datetime` object
#' \item `timestamp1`: Numeric timestamp for start of segment (seconds since 00:00:00 UTC on 1 January 1970)
#' \item `lat2`: Ending latitude
#' \item `lon2`: Ending longitude
#' \item `DateTime2`: Date and time of end of segment
#' \item `timestamp2`: Numeric timestamp for end of segment
#' \item `mlat`: Latitude of middle of segment (i.e., the coordinate for the row of `DAS` data that is at `nrow(<segment data>) / 2`)
#' \item `mlon`: Longitude of middle of segment
#' \item `mDateTime`: Date and time of middle of segment
#' \item `mtimestamp`: Numeric timestamp of middle of segment
#' \item `use`: A decision as to whether or not this segment will be included in the analysis, based upon user-specified criteria in settings.
#' \item `Mode`: Effort mode (`P` for passing or `C` for closing)
#' \item `EffType`: Effort type (`S` for systematic, `N` for non-systematic -- i.e., off design-based trackline -- and `F` for fine-scale)
#' \item `OnEffort`: If `TRUE`, standard search protocols are in practice
#' \item `ESWsides`: Number of sides for which the effective strip width (ESW) will apply. When traveling nearshore, this may be only 1
#' \item `year`: Year
#' \item `month`: Numeric month
#' \item `day`: Numeric day of month
#' \item `min_line`: The line number of `DAS` data at the start of this segment
#' \item `max_line`: The final line number
#' \item `n_rows`: Number of rows of `DAS` data in this segment
#' \item `avgBft`: Weighted average Beaufort sea state during this segment
#' \item `avgSwellHght`: Weighted average Swell Height, in feet, during this segment
#' \item `avgHorizSun`: Weighted average horizontal sun angle, corresponding to a clock face, during this segment
#' \item `avgVertSun`: Weighted average vertical sun angle (12 = overhead, 1-3 = at the horizon) during this segment
#' \item `avgGlare`: Weighted average Glare status during this segment
#' \item `avgVis`: Weighted average visibility (defines as the distance, in nautical miles, at which a dolphin could be seen surfacing with the water, not the sky, as the background) during this segment
#' \item `avgCourse`: Weighted average ship heading during this segment
#' \item `avgSpdKt`: Weighted average speed, in knots, during this segment
#' }
#' \cr
#' **`das`** data structure \cr
#' The `data.frame` of `DAS` survey data, as read and formatted by `swfscDAS::das_read()` and `swfscDAS::das_process()`
#' See the latter function documentation for details on columns. We have added the following columns during the preparation
#' of the `cruz` object:
#' \itemize{
#' \item `stratum_<stratum name>` A Boolean indicating whether or not this row of data occurs within this geostratum polygon.
#' There will be a column like this for each geostratum provided in your `settings` object.
#' \item `year` Year
#' \item `month` Numeric month
#' \item `day` Numeric day of month
#' \item `yday` Numeric day of year
#' \item `km_int` Distance, in kilometers, between this row of data and the next. See documentation for `process_km()` for details.
#' \item `km_cum` Cumulative distance traveled up to this point in the `DAS` data.
#' \item `ship` Ship name initials
#' \item `stratum` Final stratum designation, decided based upon user-specific settings.
#' \item `seg_id` Identifier for the segment containing this row of data.
#' \item `use`: A Boolean decision as to whether or not this segment will be included in the analysis, based upon user-specified criteria in settings.
#' }
#' \cr
#' **`sightings`** data structure \cr
#' The `data.frame` of processed sightings, as prepared by `swfscDAS::das_sight(return.format = 'complete')`.
#' See that function's documentation for details on columns. Note that the unique sighting identifier can be
#' found in column `SightNodaily`, not `SightNo`.
#' That function returns up to several rows for each sighting (`DAS` event codes `S`, `s`, `A`, `1`, `2`, ..., etc.).
#' We have processed the result further such that each row represents the school size estimate
#' for a single species within a single sighting. For example, single-species sightings will always have just one row.
#' We have also added the following columns during the preparation
#' of the `cruz` object:
#' \itemize{
#' \item See the columns added to the `das` slot above; those have all been propagated to this `sightings` table.
#' \item `species`: species code (character string) for the species represented by this row.
#' \item `best`: Best estimate of school size (calibrated and averaged across observer estimates according to `settings`)
#' \item `low`: Low estimate of school size (ditto)
#' \item `high`: High estimate of school size (ditto)
#' \item `prob`: If `TRUE`, this species code is a probable identification (`DAS` event code `?`).
#' \item `mixed`: If `TRUE`, this species occurred in a mixed-species school.
#' \item `ss_tot`: Total school size (calibrated and averaged across observer estimates according to `settings`)
#' \item `ss_percent`: Percent of the school comprised by this species (averaged across observer estimates).
#' \item `n_sp`: Number of species in this sighting.
#' \item `n_obs`: Number of observers who contributed a school size estimate for this species.
#' \item `n_best`: Number of valid best estimates of school size.
#' \item `n_low`: Number of valid low estimates.
#' \item `n_high`: Number of valid high estimates.
#' \item `calibr`: Boolean indicating whether school size calibration was possible, if attempted.
#' \item `ss_valid`: Boolean indicating whether or not the school size estimate
#' for this sighting is valid and appropriate for use in abundance estimation and/or detection function fitting
#' with a school-size covariate.
#' \item `included`: Boolean indicating whether the sightings should be included
#' in the analysis based on the specified settings. Any sighting with `use == FALSE`
#' will also have `included == FALSE`, but it *is* possible for sightings
#' to have `use == TRUE` with `included == FALSE`. For example, if the setting
#' `abeam_sightings` is set to `FALSE`, a sighting with a bearing angle beyond
#' the ship's beam can be excluded from the analysis  (`included == FALSE`)
#' even though the effort segment it occurs within will still be used (`use == TRUE`).
#' }
#' \cr
#' **`subgroups`** data structure \cr
#' If subgroup events (`DAS` event code `G`) are found pertaining to the species in your cohort,
#' this slot will have a list with three slots:
#' \enumerate{
#' \item **`sightings`:** A `data.frame` in which each row is a school size estimate
#' for a single phase for a single sighting, with all subgroup school sizes summed together.
#' Columns:
#' \enumerate{
#' \item `Cruise`
#' \item `Date`
#' \item `SightNo`
#' \item `Phase`
#' \item `DateTime` Mean date and time of estimates of this sighting
#' \item `Lat` (same -- mean)
#' \item `Lon` (same -- mean)
#' \item `Species`
#' \item `Angle` (same -- mean)
#' \item `RadDist` (same -- mean)
#' \item `seg_id` Identifier for the segment containing this row of data.
#' \item `PerpDist` (same -- mean)
#' \item `GSBest` Sum of arithmetic means of best estimates of subgroups
#' \item `GSBest_geom` Sum of geometric means
#' \item `EffType` Type of effort (systematic, nonsystematic, or fine-scale)
#' \item `OnEffort` Whether or not standard search protocols are in use (`TRUE` or `FALSE`).
#' \item `use` Whether the segment on which this sighting occurred meets analysis inclusion criteria.
#' \item `stratum_[stratum name]` A set of columns, one for each geostratum in the `cruz` object settings file,
#' indicating whether or not this location occurs within each geostratum.
#' \item `stratum` The geostratum to which this location was assigned, based upon cohort settings.
#' }
#' \item **`subgroups`:** A `data.frame` in which each row is a single phase
#' for a single subgroup, with all school size estimates averaged together (both arithmetically and geometrically). Columns:
#' \enumerate{
#' \item `Cruise`
#' \item `Date`
#' \item `DateTime` First date and time of estimates of this subgroup
#' \item `Lat` (same -- first data point for estimates of this subgroup)
#' \item `Lon` (same)
#' \item `OnEffort` (same)
#' \item `EffType` (same)
#' \item `SightNo` (same)
#' \item `Species` (same)
#' \item `SubGrp` (same)
#' \item `Angle` Mean angle to this subgroup
#' \item `RadDist` Mean radial distance to this subgroup
#' \item `seg_id` Identifier for the segment containing this row of data.
#' \item `PerpDist` Mean perpendicular distance to this subgroup
#' \item `GSBest` Arithmetic mean of best estimates of this subgroup. If no best estimates are given, the `GSL` value will be used here.
#' \item `GSH` Arithmetic mean of high estimates of this subgroup
#' \item `GSL` Arithmetic mean of low estimates of this subgroup
#' \item `GSBest_geom` Geometric mean of best
#' \item `GSH_geom` Geometric mean of high
#' \item `GSL_geom` Geometric mean of low
#' \item `Phase` Phase (1 or 2)
#' \item `use` Whether the segment on which this sighting occurred meets analysis inclusion criteria.
#' \item `stratum_[stratum name]` A set of columns, one for each geostratum in the `cruz` object settings file,
#' indicating whether or not this location occurs within each geostratum.
#' \item `stratum` The geostratum to which this location was assigned, based upon cohort settings.
#' }
#' \item **`events`:** A `data.frame` in which each row is single subgroup size estimate from a single observer for a single phase (Phase 1 -- on effort / passing mode -- or Phase 2 -- off effort / closing mode).
#' This is effectively the "raw" subgroup data. Columns:
#' \enumerate{
#' \item `Cruise` Cruise number
#' \item `Date` Date, in format YYYY-MM-DD
#' \item `DateTime` Date and time, in format YYYY-MM-DD HH:MM:SS
#' \item `Lat` Latitude, in decimal degrees
#' \item `Lon` Longitude, in decimal degrees
#' \item `OnEffort` If `TRUE`, standard search protocols are in use. If `FALSE`, non-standard protocols (or no searching at all).
#' \item `EffType` Effort type (`S` for systematic, `N` for non-systematic, and `F` for fine-scale)
#' \item `SightNo` Sighting number of the day (the count resets every day)
#' \item `Species` Species code (character string)
#' \item `Line` Line number in `DAS` data
#' \item `SubGrp` Subgroup identifier code
#' \item `Event` Count of estimates for this subgroup-phase
#' \item `Obs` Observer code
#' \item `GSBest` Best estimate of subgroup size
#' \item `GSH` High estimate of subgroup size
#' \item `GSL` Low estimate of subgroup size
#' \item `Angle` Angle between bow and subgroup
#' \item `RadDist` Radial distance to subgroup, in km
#' \item `use` Whether the segment on which this sighting occurred meets analysis inclusion criteria.
#' \item `stratum_[stratum name]` A set of columns, one for each geostratum in the `cruz` object settings file,
#' indicating whether or not this location occurs within each geostratum.
#' \item `stratum` The geostratum to which this location was assigned, based upon cohort settings.
#' \item `PerpDist` Perpendicular distance to subgroup from the trackline represented by the ship heading, in km
#' \item `sgid` Unique subgroup identifier
#' \item `sitid` Unique sighting identifier
#' \item `phase` Phase in protocol; all `OnEffort == TRUE` estimates are Phase 1; all `OnEffort == FALSE` estimates are Phase 2.
#' }
#' }
#' \cr\cr
#' This `cruz` object can be carried forward into data exploration (e.g., see the `LTabundR` function `cruz_explorer()`),
#' analyses (e.g., see the `LTabundR` function `lta()`),
#' or passed to mapping functions (see the `LTabundR` functions that begin with `map_...`)
#' or summary functions (see the `LTabundR` functions that begin with `summarize_...`).
#'
#' @export
#'
process_surveys <- function(das_file,
                            settings,
                            edits = NULL,
                            delete_edited_files = TRUE,
                            process_sightings = TRUE,
                            process_subgroups = TRUE,
                            save_local = FALSE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    data(example_settings)
    settings <- example_settings
    edits = NULL
    save_local=FALSE
    delete_edited_files = TRUE
    process_sightings = TRUE
    process_subgroups = TRUE

    # Edits (practice edit: make first 10 rows same lat/long coordinate)
    das_readtext(das_file)$das[20:30] # Preview region of DAS file that will be edited

    edits <- list(list(das_file = das_file,
                       type = 'text',
                       rows = 20:30,
                       chars = 20:39,
                       edit = 'N21:47.99 W159:45.91'))

    # Test
    cruz <- process_surveys(das_file, settings)
  }
  #=============================================================================
  #=============================================================================
  # Execute edits
  # & update the vector of DAS files to edit

  suffix = '_edited'
  das_file_new <-
    das_editor_tofile(das_file,
                      edits,
                      suffix = suffix,
                      verbose = TRUE)
  das_file_new

  # Note the edited files to delete after processing is complete
  (files_to_delete <- das_file_new[grepl(suffix, das_file_new)])

  #=============================================================================
  # Loop through each DAS file

  cruzes <- list()
  length(das_file_new)
  i=1
  for(i in 1:length(das_file_new)){
    fili <- das_file_new[i]
    message('=====================================\n',fili,'\n=====================================\n')

    message('\nReading in DAS file(s) ============================================')
    das <- das_load(fili)

    # Is interpolation requested?
    interp_interval <- 0
    if(!is.null(settings$survey$interpolate)){
      interpi <- as.numeric(settings$survey$interpolate)
      if(!is.na(interpi)){
        if(interpi > 0 & interpi < Inf){
          message('\nInterpolating DAS data =============================================')
          das <- das_interpolate(das,
                                 new_interval = interpi,
                                 verbose = TRUE)
        }
      }
    }

    message('\nProcessing strata =================================================')
    cruz <- process_strata(das, settings, verbose=TRUE)

    message('\nFormatting DAS for analysis =======================================')
    cruz <- das_format(cruz, verbose=TRUE)

    message('\nSegmentizing ======================================================')
    cruz <- segmentize(cruz, verbose=TRUE)

    if(process_sightings){
      message('\nProcessing sightings ==============================================')
      cruz <- process_sightings(cruz, verbose=TRUE)
    }

    if(process_subgroups){
      message('\nProcessing subgroups, if any ======================================')
      cruz <- process_subgroups(cruz, verbose=TRUE)
    }

    cruzes[[length(cruzes)+1]] <- cruz
  }

  #=============================================================================
  # Combine cruz objects if needed

  if(length(cruzes)>1){
    message('\nCombining cruz objects from multiple DAS files into a single output =')
    cruz <- cruz_combine(cruzes)
  }else{
    cruz <- cruzes[[1]]
  }

  #=============================================================================
  # Save locally?

  if(save_local){
    message('\nSaving output locally ===========================================')
    new_fn <- substr(das_file_new[1],1,(nchar(das_file_new[1])-3))
    new_fn <- paste0(new_fn,'RData')
    new_fn
    save(cruz,file=new_fn)
  }

  #=============================================================================
  # Remove any temporary files from the editing stage

  if(delete_edited_files){
    if(length(files_to_delete)>0){
      file.remove(files_to_delete)
    }
  }

  #=============================================================================
  if(FALSE){ # debugging scratchpad
    cruz$cohorts$default$das[1:30,1:5]
  }
  #=============================================================================

  message('\nFinished! =========================================================')

  return(cruz)
}
