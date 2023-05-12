#' Filter `cruz` objects by species/region/year, etc.
#'
#' Subsets a `cruz` object according to a variety of filters. The same filters
#' apply to all cohorts within the `cruz` object.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
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
#' meet all criteria; since that `DAS` dataframe comes from within the processed `cruz` object,
#' it has a `seg_id` column indicating the segment IDs pertaining to eligible rows of data.
#' Those segment IDs are then used to filter `segments`, `sightings`, and `subgroups`.
#'
#' Note that when arguments are `NULL`, they are ignored and no filtering by the associated variable occurs.
#' Also note that subgroups are not filtered at this time.
#'
#' Note that there is a similar function, `filter_cohort()`, that performs a similar task
#' for only a single cohort, without requiring an in-tact `cruz` object.
#' Instead it asks for the requisite datasets separately.
#'
#' @return A modified `cruz` object. See `process_surveys()` documentation for details.
#'
#' @export
#'
filter_cruz <- function(cruz,
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

    analysis_only = FALSE
    spp = NULL
    years = NULL
    not_years = NULL
    cruises = NULL
    not_cruises = NULL
    regions = NULL
    not_regions = NULL
    bft_range = NULL
    eff_types = NULL
    on_off = NULL
    lat_range = NULL
    lon_range = NULL
    verbose = TRUE

    years = 2017
    regions = 'WHICEAS'
    not_regions = 'SD_BI'
    verbose=TRUE
    analysis_only <- TRUE
    bft_range <- 0:6
    eff_types = c('S')
    on_off = c(TRUE)

    (years = estimati$years)
    (regions = estimati$regions)
    (not_regions = estimati$regions_remove)
    (cruises = estimati$cruises)
    (eff_types = abund_eff_types)
    (bft_range = abund_bft_range)

    # Try it
    newcruz <- filter_cruz(cruz,
                            analysis_only = TRUE,
                            years = 2020,
                            regions = 'WHICEAS',
                            not_regions = 'SD_BI',
                            bft_range = 0:6,
                            eff_types = c('S'),
                            lat_range = c(18, 22),
                            lon_range = c(-160, -154))
    newcruz$cohorts$most$das$Lat %>% range
    newcruz$cohorts$most$das$Lon %>% range
  } # end debugging area =======================================================

  # Stage resulting cruz object
  newcruz <- cruz

  # Loop through each cohort
  ci=1
  for(ci in 1:length(newcruz$cohorts)){

    if(verbose){message('======== Cohort ',names(cruz$cohorts)[ci],' ========')}

    # Parse cohort into discrete data objects
    cohorti <- cruz$cohorts[[ci]]
    segments <- cohorti$segments
    sightings <- cohorti$sightings
    das <- cohorti$das
    (subgroups <- cohorti$subgroups) %>% names
    sg_sightings <- subgroups$sightings
    sg_subgroups <- subgroups$subgroups
    sg_events <- subgroups$events

    # Announce pre-filtering status
    if(verbose){message('--- Pre-filtering:')}
    if(verbose){message('--- --- DAS rows: ',nrow(das))}
    if(verbose){message('--- --- Segments: ',nrow(segments))}
    if(verbose){message('--- --- Sightings: ',nrow(sightings))}
    if(verbose){message('--- --- Subgroups:')}
    if(verbose){message('--- --- --- Sightings: ',nrow(sg_sightings))}
    if(verbose){message('--- --- --- Subgroups: ',nrow(sg_subgroups))}
    if(verbose){message('--- --- --- Events: ',nrow(sg_events))}
    if(verbose){message('\n')}
    if(verbose){message('--- Filtering ...')}
    if(verbose){message('\n')}

    # Filtering to use in analysis
    if(analysis_only){
      segments <- segments %>% dplyr::filter(use == TRUE)
      sightings <- sightings %>% dplyr::filter(included == TRUE)
      das <- das %>% dplyr::filter(use == TRUE)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(use==TRUE)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(use==TRUE)
        sg_events <- sg_events %>% dplyr::filter(use==TRUE)
      }
    }

    # Filtering by species
    if(!is.null(spp)){
      sightings <- sightings %>% dplyr::filter(species %in% spp)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(Species %in% spp)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(Species %in% spp)
        sg_events <- sg_events %>% dplyr::filter(Species %in% spp)
      }
    }

    # Filtering by year
    if(!is.null(years)){
      segments <- segments %>% dplyr::filter(year %in% years)
      sightings <- sightings %>% dplyr::filter(year %in% years)
      das <- das %>% dplyr::filter (year %in% years)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(lubridate::year(lubridate::ymd(Date)) %in% years)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(lubridate::year(lubridate::ymd(Date)) %in% years)
        sg_events <- sg_events %>% dplyr::filter(lubridate::year(lubridate::ymd(Date)) %in% years)
      }
    }

    # Filtering by *NOT* year
    if(!is.null(not_years)){
      segments <- segments %>% dplyr::filter(! year %in% not_years)
      sightings <- sightings %>% dplyr::filter(! year %in% not_years)
      das <- das %>% dplyr::filter (! year %in% not_years)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(! lubridate::year(lubridate::ymd(Date)) %in% not_years)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(! lubridate::year(lubridate::ymd(Date)) %in% not_years)
        sg_events <- sg_events %>% dplyr::filter(! lubridate::year(lubridate::ymd(Date)) %in% not_years)
      }
    }

    # Filtering by cruise
    if(!is.null(cruises)){
      segments <- segments %>% dplyr::filter(Cruise %in% cruises)
      sightings <- sightings %>% dplyr::filter(Cruise %in% cruises)
      das <- das %>% dplyr::filter (Cruise %in% cruises)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter (Cruise %in% cruises)
        sg_subgroups <- sg_subgroups %>% dplyr::filter (Cruise %in% cruises)
        sg_events <- sg_events %>% dplyr::filter (Cruise %in% cruises)
      }
    }

    # Filtering by *NOT* cruise
    if(!is.null(not_cruises)){
      segments <- segments %>% dplyr::filter(! Cruise %in% not_cruises)
      sightings <- sightings %>% dplyr::filter(! Cruise %in% not_cruises)
      das <- das %>% dplyr::filter (! Cruise %in% not_cruises)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter (! Cruise %in% not_cruises)
        sg_subgroups <- sg_subgroups %>% dplyr::filter (! Cruise %in% not_cruises)
        sg_events <- sg_events %>% dplyr::filter (! Cruise %in% not_cruises)
      }
    }

    # Filter to certain region(s)
    # Keep only data from the regions to include
    if(!is.null(regions)){
      sits <- sightings
      segs <- segments
      dasi <- das
      (seg_ids <- region_segments(das, regions))

      # Filter segments and sightings to those seg_ids
      segi <- segs %>% dplyr::filter(seg_id %in% seg_ids)
      siti <- sits %>% dplyr::filter(seg_id %in% seg_ids)
      dasi <- dasi %>% dplyr::filter(seg_id %in% seg_ids)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(seg_id %in% seg_ids)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(seg_id %in% seg_ids)
        sg_events <- sg_events %>% dplyr::filter(seg_id %in% seg_ids)
      }

      sightings <- siti
      segments <- segi
      das <- dasi
    }

    # Exclude certain region(s)
    if(!is.null(not_regions)){
      sits <- sightings
      segs <- segments
      dasi <- das
      (seg_ids <- region_segments(das, not_regions))

      # Filter segments and sightings to **NOT** those seg_ids
      segi <- segs %>% dplyr::filter(! seg_id %in% seg_ids)
      siti <- sits %>% dplyr::filter(! seg_id %in% seg_ids)
      dasi <- dasi %>% dplyr::filter(! seg_id %in% seg_ids)
      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(! seg_id %in% seg_ids)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(! seg_id %in% seg_ids)
        sg_events <- sg_events %>% dplyr::filter(! seg_id %in% seg_ids)
      }

      sightings <- siti
      segments <- segi
      das <- dasi
    }

    # Filter by bft
    if(!is.null(bft_range)){
      segments <- segments %>% dplyr::filter(avgBft >= min(bft_range),
                                             avgBft <= max(bft_range))
      sightings <- sightings %>% dplyr::filter(Bft >= min(bft_range),
                                               Bft <= max(bft_range))
      das <- das %>% dplyr::filter(Bft >= min(bft_range),
                                   Bft <= max(bft_range))

      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(Bft >= min(bft_range),
                                                       Bft <= max(bft_range))
        sg_subgroups <- sg_subgroups %>% dplyr::filter(Bft >= min(bft_range),
                                                       Bft <= max(bft_range))
        sg_events <- sg_events %>% dplyr::filter(Bft >= min(bft_range),
                                                 Bft <= max(bft_range))
      }
    }

    # Filter by eff_types
    if(!is.null(eff_types)){
      segments <- segments %>% dplyr::filter(EffType %in% eff_types)
      sightings <- sightings %>% dplyr::filter(EffType %in% eff_types)
      das <- das %>% dplyr::filter(EffType %in% eff_types)

      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(EffType %in% eff_types)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(EffType %in% eff_types)
        sg_events <- sg_events %>% dplyr::filter(EffType %in% eff_types)
      }
    }

    # Filter by OnEffort/OffEffort
    if(!is.null(on_off)){
      segments <- segments %>% dplyr::filter(OnEffort %in% on_off)
      sightings <- sightings %>% dplyr::filter(OnEffort %in% on_off)
      das <- das %>% dplyr::filter(OnEffort %in% on_off)

      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(OnEffort %in% on_off)
        sg_subgroups <- sg_subgroups %>% dplyr::filter(OnEffort %in% on_off)
        sg_events <- sg_events %>% dplyr::filter(OnEffort %in% on_off)
      }
    }

    # Filter by Latitude/Longitude
    if(!is.null(lat_range)){
      segments <- segments %>% dplyr::filter(mlat >= min(lat_range),
                                             mlat <= max(lat_range))
      sightings <- sightings %>% dplyr::filter(Lat >= min(lat_range),
                                               Lat <= max(lat_range))
      das <- das %>% dplyr::filter(Lat >= min(lat_range),
                                   Lat <= max(lat_range))

      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(Lat >= min(lat_range),
                                                       Lat <= max(lat_range))
        sg_subgroups <- sg_subgroups %>% dplyr::filter(Lat >= min(lat_range),
                                                       Lat <= max(lat_range))
        sg_events <- sg_events %>% dplyr::filter(Lat >= min(lat_range),
                                                 Lat <= max(lat_range))
      }
    }
    if(!is.null(lon_range)){
      segments <- segments %>% dplyr::filter(mlon >= min(lon_range),
                                             mlon <= max(lon_range))
      sightings <- sightings %>% dplyr::filter(Lon >= min(lon_range),
                                               Lon <= max(lon_range))
      das <- das %>% dplyr::filter(Lon >= min(lon_range),
                                   Lon <= max(lon_range))

      if(!is.null(sg_sightings)){
        sg_sightings <- sg_sightings %>% dplyr::filter(Lon >= min(lon_range),
                                                       Lon <= max(lon_range))
        sg_subgroups <- sg_subgroups %>% dplyr::filter(Lon >= min(lon_range),
                                                       Lon <= max(lon_range))
        sg_events <- sg_events %>% dplyr::filter(Lon >= min(lon_range),
                                                 Lon <= max(lon_range))
      }
    }

    # Announce post-filtering results
    if(verbose){message('--- Post-filtering:')}
    if(verbose){message('--- --- DAS rows: ',nrow(das))}
    if(verbose){message('--- --- Segments: ',nrow(segments))}
    if(verbose){message('--- --- Sightings: ',nrow(sightings))}
    if(verbose){message('--- --- Subgroups:')}
    if(verbose){message('--- --- --- Sightings: ',nrow(sg_sightings))}
    if(verbose){message('--- --- --- Subgroups: ',nrow(sg_subgroups))}
    if(verbose){message('--- --- --- Events: ',nrow(sg_events))}
    if(verbose){message('\n')}

    # Add filtered data back to cruz object
    cohorti$sightings <- sightings
    cohorti$segments <- segments
    if(!is.null(sg_sightings)){
      subgroups <- list(sightings = sg_sightings,
                        subgroups = sg_subgroups,
                        events = sg_events)
      cohorti$subgroups <- subgroups
    }
    cohorti$das <- das

    # Update cruz with this filtered cohort
    newcruz$cohorts[[ci]] <- cohorti
  }

  return(newcruz)
}
