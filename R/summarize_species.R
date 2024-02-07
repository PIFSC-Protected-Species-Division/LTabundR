#' Summarize sightings for a species (or several)
#'
#' @param spp A character vector with codes for the species you wish to summarize.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param filter_to_regions Regions (geostrata) to filter the results down to. Use `cruz$strata` to see the options available to you.
#' @param exclude_regions Regions (geostrata) to exclude from the results. This can be helpful if, for example, you wish to
#' summarize sightings for a large region but not for regions nested within it, such as insular geostrata.
#' @param distance_restrict If `TRUE`, detection distances will only be shown for sightings that will be included in analysis (`included == TRUE`).
#' @param distance_range The range of distances for which to summarize detection distances.
#' @param distance_interval The interval of distances to summarize.
#'
#' @return A list with the following named slots:
#' \enumerate{
#' \item `species`: A dataframe with the codes, common names, and scientific name(s) for the specified species (as found in `data(species_codes)`)
#' \item `n_total`: Total number of sightings (mixed-species are only counted once; this is true for all other slots here as well).
#' \item `n_analysis`: Number of sightings that will qualify for inclusion in the analysis (`included == TRUE`).
#' \item `school_size`: Dataframe with summary metrics of school size for each species.
#' \item `yearly_total`: Dataframe with counts and school sizes metrics for all sightings, parsed by year of sighting.
#' \item `yearly_analysis`: Dataframe with counts and school size metrics for sightings included in the analysis, parsed by year of sighting.
#' \item `regional_total`: Dataframe with counts and school sizes metrics for all sightings, parsed by regional geostratum.
#' \item `regional_analysis`: Dataframe with counts and school size metrics for sightings included in the analysis, parsed by regional geostratum.
#' \item `detection_distances`: A dataframe with sighting counts for various
#' perpendicular distances (in KM) (systematic sightings only).
#' The `percent_beyond` column can be used to identify appropriate truncation distances for this species group.
#' \item `sightings`: Dataframe with all `sightings` information for this species group.
#' }
#' @import dplyr
#' @export
#'
summarize_species <- function(spp,
                              cruz,
                              cohort=1,
                              filter_to_regions = NULL,
                              exclude_regions = NULL,
                              distance_restrict = TRUE,
                              distance_range = c(0,10),
                              distance_interval = 0.5){

  if(FALSE){ # debugging only -- not run! ======================================
    data(example_cruz)
    cruz <- example_cruz
    spp <- '046'

    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    spp <- c('015', '018', '032')

    cohort = 1
    filter_to_regions <- NULL #c('WHICEAS')
    exclude_regions <- NULL
    #exclude_regions <- 'MHI'
    distance_restrict <- FALSE
    distance_range <- c(0,10)
    distance_interval <- .5

    # try it
    summarize_species(spp, cruz) %>% names
    summarize_species(spp, cruz)$species
    summarize_species(spp, cruz)$n_total
    summarize_species(spp, cruz)$n_analysis
    summarize_species(spp, cruz)$school_size
    summarize_species(spp, cruz)$yearly_total
    summarize_species(spp, cruz)$regional_total
    summarize_species(spp, cruz)$detection_distances

    summarize_species('046', cruz)
    sits <- summarize_species('032', cruz)$sightings
    summarize_species('032', cruz, distance_restrict = TRUE)$detection_distances
    sits %>% filter(included == TRUE) %>% pull(PerpDistKm) %>% sort

    summarize_species('046', cruz)$detection_distances
    summarize_species('046', cruz, distance_range=c(0,3),distance_interval=.2)$detection_distances

  } # end debugging not run! ===================================================

  # Filter to correct cohort and analysis
  ani <- cruz$cohorts[[cohort]]
  sits <- ani$sightings

  # Setup truncation distance steps
  (td_range <- c(0, max(sits$PerpDistKm,na.rm=TRUE))) # truncation distance range
  (td_steps <- seq(min(td_range), (max(td_range) + distance_interval), by=distance_interval))
  (td_steps <- c(0, td_steps))

  # Filter to species
  siti <- sits %>% dplyr::filter(species %in% spp) ; nrow(siti)
  cruz$strata

  # Filter to certain region(s)
  if(!is.null(filter_to_regions)){
    filter_to_regions
    (region_cols <- paste0('stratum_',filter_to_regions))
    (region_cols <- which(names(siti) %in% region_cols))
    if(length(region_cols)==0){
      stop('Problem! We cannot find a stratum_ column associated with one of your `filter_to_regions` inputs. Stopping here.')
    }else{
      (filter_decision <- apply(siti %>% dplyr::select(region_cols),1,any))
      siti <- siti[filter_decision,] ; nrow(siti)
    }
  }

  # Exclude certain region(s)
  if(!is.null(exclude_regions)){
    exclude_regions
    (region_cols <- paste0('stratum_',exclude_regions))
    (region_cols <- which(names(siti) %in% region_cols))
    if(length(region_cols)>0){
      (filter_decision <- apply(siti %>% dplyr::select(region_cols),1,any))
      siti <- siti[!filter_decision,] ; nrow(siti)
    }
  }

  # Now summarize
  results <- list()

  # Species info, sought within built-in dataset, data(species_codes)
  #(results$species <- species_translator(spp)[,1:4])
  (results$species <- sapply(spp, species_translator) %>% t %>% as.data.frame)

  # Total sightings
  #(results$n_total <- siti %>% nrow)
  (results$n_total <-
      siti %>%
      pull(SightNoDaily) %>%
      unique() %>% length)

  # Total sightings in analysis
  (results$n_analysis <-
    siti %>%
    dplyr::filter(included == TRUE) %>%
    pull(SightNoDaily) %>%
    unique() %>% length)

  # School size metrics (analysis only)
  (results$school_size <-
    siti %>%
    dplyr::filter(included == TRUE) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(ss_mean = mean(best, na.rm=TRUE),
                     ss_sd = sd(best, na.rm=TRUE),
                     n = length(unique(SightNoDaily))) %>%
    dplyr::mutate(ss_se = ss_sd / sqrt(n),
                  ss_cv = ss_sd / ss_mean))

  # Annual metrics
  (results$yearly_total <-
    siti %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(n=length(unique(SightNoDaily)),
                     ss_mean = mean(best, na.rm=TRUE),
                     ss_sd = sd(best, na.rm=TRUE)) %>%
    dplyr::mutate(ss_se = ss_sd / sqrt(n),
                  ss_cv = ss_sd / ss_mean) %>%
    dplyr::arrange(year) %>%
    as.data.frame)

  # Annual w/ only analysis data
  (results$yearly_analysis <-
    siti %>%
    dplyr::filter(included == TRUE) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(n=length(unique(SightNoDaily)),
                     ss_mean = mean(best, na.rm=TRUE),
                     ss_sd = sd(best, na.rm=TRUE)) %>%
    dplyr::mutate(ss_se = ss_sd / sqrt(n),
                  ss_cv = ss_sd / ss_mean) %>%
    dplyr::arrange(year) %>%
    as.data.frame)

  # Regional
  (results$regional_total <-
    siti %>%
    dplyr::group_by(stratum) %>%
    dplyr::summarize(n=length(unique(SightNoDaily)),
                     ss_mean = mean(best, na.rm=TRUE),
                     ss_sd = sd(best, na.rm=TRUE)) %>%
    dplyr::mutate(ss_se = ss_sd / sqrt(n),
                  ss_cv = ss_sd / ss_mean) %>%
    as.data.frame)

  # Regional with only analysis data
  (results$regional_analysis <-
    siti %>%
    dplyr::filter(included == TRUE) %>%
    dplyr::group_by(stratum) %>%
    dplyr::summarize(n=length(unique(SightNoDaily)),
                     ss_mean = mean(best, na.rm=TRUE),
                     ss_sd = sd(best, na.rm=TRUE)) %>%
    dplyr::mutate(ss_se = ss_sd / sqrt(n),
                  ss_cv = ss_sd / ss_mean) %>%
    as.data.frame)

  # Detection distances ========================================================

  # Get a working version with one row per sighting (not per species)
  sitii <-
    siti %>%
    group_by(SightNoDaily) %>%
    filter(row_number()==1)

  # Check
  siti %>% nrow
  sitii %>% nrow

  if(distance_restrict){
    sitii <- sitii %>% dplyr::filter(included == TRUE)
  }

  #td_steps

  (perps <- sitii$PerpDistKm %>% sort)

  results$detection_distances <- summarize_distances(perps,
                                                     distance_range = distance_range,
                                                     distance_interval = distance_interval)

  # results$detection_distances <-
  #   data.frame(km_min_incl = td_steps,
  #            km_max_excl = lead(td_steps),
  #            sightings = c(hist(perps, breaks=td_steps, plot=FALSE)$counts,0)) %>%
  #   dplyr::mutate(km_mid = km_min_incl + 0.5*(km_max_excl - km_min_incl)) %>%
  #   dplyr::mutate(total_within = cumsum(sightings)) %>%
  #   dplyr::mutate(total_beyond = sum(sightings) - total_within) %>%
  #   dplyr::mutate(percent_beyond = (total_beyond / sum(sightings)*100)) %>%
  #   dplyr::select(km_min_incl, km_max_excl, sightings, percent_beyond, total_beyond, total_within, km_mid) %>%
  #   dplyr::filter(km_min_incl >= min(distance_range),
  #                 km_max_excl <= max(distance_range))

  # Add raw sightings to results ===============================================
  results$sightings <- siti

  # Review & return
  results
  return(results)

}
