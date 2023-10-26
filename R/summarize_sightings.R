#' Inventory sightings and species counts from a `Wincruz` survey
#'
#' Inventory and summarize sightings within a `LTabundR` `cruz` object in various ways.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @return A list with various summary tables. In each table, each row is a count for a single species code.
#' \enumerate{
#' \item `simple_totals`: includes all sightings, even if they will not be included in analysis.
#' Seven columns:
#' `code` (species code), `short_name`, `scientific_name`, `common_name`, `n` (number of sightings), `ss_mean` (mean school size), `ss_sd` (standard devication of school size).
#' \item `analysis_totals`: only includes sightings that meet all inclusion criteria for the analysis.
#' Same columns as `simple_totals`.
#' \item `stratum_simple_totals`: inclues all sightings, even if they will not be included in analysis,
#' parsed by each geostratum. Same columns as `simple_totals`, with the addition of `year`, `Cruise`, and `stratum`.
#' \item `stratum_analysis_totals`: only includes sightings that meet all inclusion criteria for the analysis,
#' parsed by each geostratum. Same columns as `simple_totals`, with the addition of `year`, `Cruise`, and `stratum`.
#' }
#'
#' @export
#'
summarize_sightings <- function(cruz,
                                cohort=1){

  #=============================================================================
  # For debugging -- only
  if(FALSE){
    data(example_cruz)
    cruz <- example_cruz
    species_codes <- NULL
    cohort = 1

    # test
    summarize_sightings(cruz) %>% names
    summarize_sightings(cruz)$simple_totals
    summarize_sightings(cruz)$analysis_totals
    summarize_sightings(cruz)$stratum_simple_totals
    summarize_sightings(cruz)$stratum_analysis_totals
  }
  #=============================================================================

  # Get species codes if nothing input or in settings
  if(is.null(cruz$settings$survey$species_codes)){
    data(species_codes)
  }
  species_codes
  names(species_codes)

  # Filter down to the cohort-analysis specificed
  cohorti <- cruz$cohorts[[cohort]]
  names(cohorti) # review
  survey <- cohorti

  cohort_settings <- cruz$settings$cohorts[[cohort]]
  names(survey) # review

  sits <- survey$sightings
  sits$species %>% table # review

  # Only use the S event
  siti <- sits %>% dplyr::filter(Event == 'S')

  # Join species codes to the sightings data
  siti$code <- siti$species
  siti <- dplyr::left_join(siti, species_codes, by='code')
  siti$common_name1 %>% table

  suppressWarnings({
    suppressMessages({

      # Species roster with simple counts (all detections)
      spp <-
        siti %>%
        dplyr::group_by(species) %>%
        dplyr::summarize(dplyr::across(short_name:common, unique),
                         n = dplyr::n(),
                         ss_mean = mean(best, na.rm=TRUE) %>%  round(2),
                         ss_sd = sd(best, na.rm=TRUE) %>%  round(2)) %>%
        dplyr::rename(code =  species) %>%
        dplyr::arrange(desc(n))
      spp

      # Species roster with simple counts (all detections)
      spp_incl <-
        siti %>%
        dplyr::filter(included == TRUE) %>%
        dplyr::group_by(species) %>%
        dplyr::summarize(dplyr::across(short_name:common, unique),
                         n = dplyr::n(),
                         ss_mean = mean(best, na.rm=TRUE) %>%  round(2),
                         ss_sd = sd(best, na.rm=TRUE) %>%  round(2)) %>%
        dplyr::rename(code =  species) %>%
        dplyr::arrange(desc(n))
      spp_incl

      # Stratum-by-stratum species roster
      stratum_spp <-
        siti %>%
        dplyr::group_by(year, Cruise, stratum, species) %>%
        dplyr::summarize(dplyr::across(short_name:common, unique),
                         n = dplyr::n(),
                         ss_mean = mean(best, na.rm=TRUE) %>%  round(2),
                         ss_sd = sd(best, na.rm=TRUE) %>%  round(2)) %>%
        dplyr::rename(code =  species) %>%
        dplyr::arrange(desc(n))
      stratum_spp

      # Stratum-by-stratum species roster
      stratum_spp_incl <-
        siti %>%
        dplyr::filter(included == TRUE) %>%
        dplyr::group_by(year, Cruise, stratum, species) %>%
        dplyr::summarize(dplyr::across(short_name:common, unique),
                         n = dplyr::n(),
                         ss_mean = mean(best, na.rm=TRUE) %>%  round(2),
                         ss_sd = sd(best, na.rm=TRUE) %>%  round(2)) %>%
        dplyr::rename(code =  species) %>%
        dplyr::arrange(desc(n))
      stratum_spp_incl

    })
  })

  return_list <- list(simple_totals = spp,
                      analysis_totals = spp_incl,
                      stratum_simple_totals = stratum_spp,
                      stratum_analysis_totals = stratum_spp_incl)

  # Return
  return(return_list)
}

