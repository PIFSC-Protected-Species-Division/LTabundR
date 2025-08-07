#' Prepare bootstrapped versions of datasets
#'
#' An interior function, typically not called by analysts.
#' This function returns segment and sighting data according to resampled segment IDs,
#' which serve as part of the bootstrap variance estimation process.
#' The function includes an option to specify a selection of segment IDs, which allows
#' the same bootstrap segments to be selected for the abundance estimation stage as was
#' used in the detection function model fitting stage.
#'
#' @param segments Effort segments dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$segments`),
#' already filtered to contain the effort you wish to use to fit the detection function.
#' @param sightings Sightings dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$sightings`),
#' already filtered to contain the sightings you wish to use to fit the detection function.
#' @param segment_picks If segment IDs have been previously selected for this dataset, provide them here.
#' @param seed Set a seed (any integer) to ensure that the result is reproducible.
#' If left `NULL`, the results are liable to differ for each run of this function.
#'
#' @return A list with resampled data: `segments`, `sightings`, and `segment_picks`
#' (a numeric vector of segment IDs that have been re-sampled).
#'
#' @export
#'
prep_bootstrap_datasets <- function(segments,
                                    sightings,
                                    segment_picks = NULL,
                                    seed = NULL){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    segments <- cruz$cohorts$all$segments
    sightings <- cruz$cohorts$all$sightings
    segment_picks = NULL
    seed=NULL

    # no reproducible
    test <- prep_bootstrap_datasets(segments, sightings, segment_picks, seed)
    test$segment_picks %>% head

    # with
    test <- prep_bootstrap_datasets(segments, sightings, segment_picks, seed=123)
    test$segment_picks %>% head
  }
  #=============================================================================

  # If segment_picks are not provided (they usually aren't), get them:
  if(is.null(segment_picks)){
    (bs_strata <- segments$stratum %>% unique) # get strata represented in segments data
    segment_picks <- c() # this vector will hold the re-sampled seg_ids to be used to prep bs versions of the datasets

    # Loop through each geostratum & resample segment id's based on relative effort
    i=1
    for(i in 1:length(bs_strata)){
      (strati <- bs_strata[i]) # stratum name
      strata_segments <- segments %>% dplyr::filter(stratum == strati) # segments in this strata
      strata_segments %>% nrow
      if(!is.null(seed)){set.seed(seed)}
      (bs_segi <- sample(1:nrow(strata_segments), size=nrow(strata_segments), replace=TRUE)) # sample the seg_ids indices w replacement
      (bs_segs <- strata_segments$seg_id[bs_segi])
      segment_picks <- c(segment_picks, bs_segs)
    }
  }
  segment_picks %>% sort


  # Setup a key that links the original seg_id to a new fake_id ================

  (seg_id_picks <- segment_picks %>% sort)
  (fake_id_key <-
      data.frame(seg_id = unique(seg_id_picks)) %>%
      mutate(fake_id = as.numeric(factor(seg_id))))


  # Make bootstrapped segments =================================================

  segments_global <<- segments
  (seg_id_picks_i <- sapply(seg_id_picks,
                            function(i){which(segments_global$seg_id == i)}))
  if(is.list(seg_id_picks_i)){
    seg_id_picks_i <- seg_id_picks_i %>% unlist
  }

  bs_segments <- segments[seg_id_picks_i,]
  bs_segments <- left_join(bs_segments, fake_id_key, by='seg_id')
  #rm(segments_global)

  # Tests
  segments %>% nrow # make sure total number of segments is the same
  bs_segments %>% nrow

  segments$stratum %>% table # make sure n segments from each stratum is same
  bs_segments$stratum %>% table

  segments$seg_id %>% table %>% table    # make sure resampling happened
  bs_segments$seg_id %>% table %>% table # there should be no replicated in og segments,
  # but there should be replicated segments in bs_segments


  # Make bootstrapped sightings  ===============================================

  sightings_global <<- sightings
  (sit_id_picks_i <- sapply(seg_id_picks,
                            function(i){which(sightings_global$seg_id == i)}) %>% unlist)
  bs_sightings <- sightings[sit_id_picks_i,]
  bs_sightings <- left_join(bs_sightings, fake_id_key, by='seg_id')
  bs_sightings$fake_id
  #rm(sightings_global)

  # Tests
  sightings %>% nrow # total number of sightings need not be the same
  bs_sightings %>% nrow

  sightings$stratum %>% table # distributon of sightings in strata need not be the same
  bs_sightings$stratum %>% table

  sightings$seg_id %>% table %>% table # these numbers should be different
  bs_sightings$seg_id %>% table %>% table

  # =============================================================================

  # Return list as output
  return(list(segments = bs_segments,
              sightings = bs_sightings,
              segment_picks = segment_picks))
}
