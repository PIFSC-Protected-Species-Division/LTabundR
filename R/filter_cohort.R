#' Filter segments & sightings for a single cohort by species/region/year, etc.
#'
#' This is an internal function, usually not called directly by analysts,
#' but it certainly can be. Rather than calling a `cruz` object, as `filter_cruz` does,
#' this function just calls for the three relevant constituent tables:
#' `segments`, `sightings`, and the full `das` dataset.
#'
#' @param segments Effort segments dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$segments`)
#' @param sightings Sightings dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$sightings`)
#' @param das Dataframe of DAS data, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$das`)
#' @param analysis_only If `TRUE`, data will be filtered to those viable for use in formal analysis.
#' For `segments`, this means rows in which `segments$use == TRUE`.
#' For `sightings`, this means rows in which `sightings$included == TRUE`.
#' @param spp A character vector of species codes, used to filter `sightings` based on `sightings$species`.
#' @param years A numeric vector of years, used to filter both `segments` and `sightings` to include only data from these years.
#' @param not_years A numeric vector of years, used to filter both `segments` and `sightings` to include only data *not* from these years.
#' Usually not useful and can be left as `NULL`, but may be useful if your data contain many years and it can be
#' more efficient to exclude certain years rather than specify most years in the preceding argument.
#' @param cruises A numeric vector of cruise numbers, used to filter a la `years` above.
#' @param not_cruises A numeric vector of cruise numbers, used to filter a la `not_years` above.
#' @param regions A character vector of geostratum names, used to filter both `segments` and `sightings`.
#' Any segment or sighting occurring within *any* (*not* all) of the provided `regions` will be returned.
#' This holds for nested regions; for example, in analyses from the Central North Pacific, in which the Hawaii EEZ geostratum (`"HI-EEZ"`)
#' is nested within the larger geostratum representing the entire CNP study area (`"OtherCNP"`),
#' an input of `regions = "OtherCNP"` will return segments/sightings *both* inside the Hawaii EEZ *and* outside of it.
#' @param not_regions A character vector of geostratum names, similar to above.
#' Any segment or sighting occurring within any of these `not_regions` will not be returned.
#' Using the example above, if `regions = "OtherCNP"` and `not_regions = "HI-EEZ"`,
#' only segments occuring within `OtherCNP` *and* outside of `HI-EEZ` will be returned.
#' This can be particularly useful for abundance estimates for pelagic stock that exclude nested insular stocks.
#' @param bft_range The  Beaufort Sea State values to filter to, provided as a numeric vector.
#' @param eff_types The effort types ('S' = Systematic, 'F' = Fine-scale, 'N' = Non-systematic) to filter to, provided as a character vector.
#' @param on_off The `OnEffort` values to filter to (`TRUE` and/or `FALSE`), provided as a logical vector.
#' @param lat_range If not `NULL`, a two-element numeric vector indicating latitudinal range of acceptable data. Accepted values are -90 (S) to 90 (N).
#' @param lon_range If not `NULL`, a two-element numeric vector indicating latitudinal range of acceptable data. Accepted values are -180 (W) to 180 (E).
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @details This function works by using the filter arguments to determine which rows of `DAS` data
#' meet all criteria; since that `DAS` dataframe is drawn from a processed `cruz` object,
#' it has a `seg_id` column indicating the segment IDs pertaining to eligible rows of data.
#' Those segment IDs are then used to filter `segments` and `sightings`.
#'
#' Note that when arguments are `NULL`, they are ignored and no filtering by the associated variable occurs.
#'
#' @return A list with two objects: `segments` and `sightings`. These are filtered
#' versions of the same objects contained within the `cruz` object.
#' See `process_surveys()` documentation for details.
#'
#' @export
#'
filter_cohort <- function(segments,
                        sightings,
                        das,
                        analysis_only = FALSE,
                        spp = NULL,
                        years = NULL,
                        not_years = NULL,
                        cruises = NULL,
                        not_cruises = NULL,
                        regions = NULL,
                        not_regions = NULL,
                        bft_range = NULL,
                        eff_types = NULL,
                        on_off = NULL,
                        lat_range = NULL,
                        lon_range = NULL,
                        verbose = TRUE){

  if(FALSE){ # for debugging ===================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    segments <- cruz$cohorts$all$segments
    sightings <- cruz$cohorts$all$sightings
    das <- cruz$cohorts$all$das
    analysis_only <- TRUE
    spp <- NULL
    years = 2017 #c(2010, 2017)
    not_years = NULL
    regions = 'WHICEAS'
    not_regions = 'SD_BI'
    bft_range <- 0:6
    eff_types = c('S')
    on_off = c(TRUE)
    lat_range = NULL
    lon_range = NULL
    toplot=TRUE
    verbose=TRUE
  } # end debugging area  ======================================================

  # Filtering to use in analysis
  if(analysis_only){
    segments <- segments %>% dplyr::filter(use == TRUE)
    sightings <- sightings %>% dplyr::filter(included == TRUE)
    das <- das %>% dplyr::filter(use == TRUE)
  }

  # Filtering by species
  if(!is.null(spp)){
    sightings <- sightings %>% dplyr::filter(species %in% spp)
  }

  # Filtering by year
  if(!is.null(years)){
    segments <- segments %>% dplyr::filter(year %in% years)
    sightings <- sightings %>% dplyr::filter(year %in% years)
    das <- das %>% dplyr::filter (year %in% years)
  }

  # Filtering by *NOT* year
  if(!is.null(not_years)){
    segments <- segments %>% dplyr::filter(! year %in% not_years)
    sightings <- sightings %>% dplyr::filter(! year %in% not_years)
    das <- das %>% dplyr::filter (! year %in% not_years)
  }

  # Filtering by cruise
  if(!is.null(cruises)){
    segments <- segments %>% dplyr::filter(Cruise %in% cruises)
    sightings <- sightings %>% dplyr::filter(Cruise %in% cruises)
    das <- das %>% dplyr::filter (Cruise %in% cruises)
  }

  # Filtering by *NOT* cruise
  if(!is.null(not_cruises)){
    segments <- segments %>% dplyr::filter(! Cruise %in% not_cruises)
    sightings <- sightings %>% dplyr::filter(! Cruise %in% not_cruises)
    das <- das %>% dplyr::filter (! Cruise %in% not_cruises)
  }

  # Filter to certain region(s)
  # Keep only data from the regions to include
  if(!is.null(regions)){
    sits <- sightings
    segs <- segments
    (seg_ids <- region_segments(das, regions))

    # Filter segments and sightings to those seg_ids
    segi <- segs %>% dplyr::filter(seg_id %in% seg_ids)
    siti <- sits %>% dplyr::filter(seg_id %in% seg_ids)

    sightings <- siti
    segments <- segi
  }

  # Exclude certain region(s)
  if(!is.null(not_regions)){
    sits <- sightings
    segs <- segments
    (seg_ids <- region_segments(das, not_regions))

    # Filter segments and sightings to **NOT** those seg_ids
    segi <- segs %>% dplyr::filter(! seg_id %in% seg_ids)
    siti <- sits %>% dplyr::filter(! seg_id %in% seg_ids)

    sightings <- siti
    segments <- segi
  }

  # Filter by bft
  if(!is.null(bft_range)){
    segments <- segments %>% dplyr::filter(avgBft >= min(bft_range),
                                   avgBft <= max(bft_range))
    sightings <- sightings %>% dplyr::filter(Bft >= min(bft_range),
                                   Bft <= max(bft_range))
  }

  # Filter by eff_types
  if(!is.null(eff_types)){
    segments <- segments %>% dplyr::filter(EffType %in% eff_types)
    sightings <- sightings %>% dplyr::filter(EffType %in% eff_types)
  }

  # Filter by OnEffort/OffEffort
  if(!is.null(on_off)){
    segments <- segments %>% dplyr::filter(OnEffort %in% on_off)
    sightings <- sightings %>% dplyr::filter(OnEffort %in% on_off)
  }

  # Filter by Latitude/Longitude
  if(!is.null(lat_range)){
    segments <- segments %>% dplyr::filter(mlat >= min(lat_range),
                                           mlat <= max(lat_range))
    sightings <- sightings %>% dplyr::filter(Lat >= min(lat_range),
                                             Lat <= max(lat_range))
  }
  if(!is.null(lon_range)){
    segments <- segments %>% dplyr::filter(mlon >= min(lon_range),
                                           mlon <= max(lon_range))
    sightings <- sightings %>% dplyr::filter(Lon >= min(lon_range),
                                             Lon <= max(lon_range))
  }

  # Report results
  if(verbose){message('--- --- Segments: ',nrow(segments))}
  if(verbose){message('--- --- Sightings: ',nrow(sightings))}

  # Compile list to return
  return(list(segments = segments, sightings = sightings))

}
