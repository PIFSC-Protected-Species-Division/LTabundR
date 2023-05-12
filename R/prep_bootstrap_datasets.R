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
#'
#' @return A list with resampled data: `segments`, `sightings`, and `segment_picks`
#' (a numeric vector of segment IDs that have been re-sampled).
#'
#' @export
#'
prep_bootstrap_datasets <- function(segments,
                                    sightings,
                                    segment_picks = NULL){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    segments <- cruz$cohorts$all$segments
    sightings <- cruz$cohorts$all$sightings
    segment_picks = NULL
  }
  #=============================================================================

  # If segment_picks are not provided (they usually aren't), get them:
  if(is.null(segment_picks)){
    (bs_strata <- segments$stratum %>% unique) # get strata represented in segments data
    segment_picks <- c() # this vector will hold the re-sampled seg_ids to be used to prep bs versions of the datasets

    # Loop through each geostratum
    i=1
    for(i in 1:length(bs_strata)){
      (strati <- bs_strata[i]) # stratum name
      strata_segments <- segments %>% dplyr::filter(stratum == strati) # segments in this strata
      strata_segments %>% nrow
      (bs_segi <- sample(1:nrow(strata_segments), size=nrow(strata_segments), replace=TRUE)) # sample the seg_ids indices w replacement
      (bs_segs <- strata_segments$seg_id[bs_segi])
      segment_picks <- c(segment_picks, bs_segs)
    }
  }
  segment_picks %>% sort

  # Now loop through each seg_id selected in segment_picks,
  # and build re-sampled segments and sightings table
  bs_segments <- bs_sightings <- data.frame() # stage dataframes
  fake_id <- 1 # create a fake segment identifier
  j=1
  for(j in 1:length(segment_picks)){
    (bs_segj <- segment_picks[j]) # get seg_id for this resampled segment

    # Add this bs segment to the master bs segments table
    (segi <- segments %>% dplyr::filter(seg_id == bs_segj))
    segments$seg_id %>% sort
    segi$seg_bs <- fake_id
    bs_segments <- rbind(bs_segments, segi)

    # Add sightings in this bs segment to the master bs sightings table
    (siti <- sightings %>% dplyr::filter(seg_id == bs_segj))
    if(nrow(siti)>0){
      siti$seg_bs <- fake_id
      bs_sightings <- rbind(bs_sightings, siti)
    }

    fake_id <- fake_id + 1 # Update fake_id
  } # end loop through bs segment IDs

  # Review
  table(segments$stratum)
  table(bs_segments$stratum)

  table(sightings$stratum)
  table(bs_sightings$stratum)

  table(sightings$stratum) %>% sum
  table(bs_sightings$stratum) %>% sum

  # Return list as output
  return(list(segments = bs_segments,
              sightings = bs_sightings,
              segment_picks = segment_picks))
}
