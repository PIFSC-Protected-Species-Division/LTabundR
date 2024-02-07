#' Summarize sighting distances in a table
#'
#' @param distances Perpendicular distances to sightings, usually in km.
#' @param distance_range The range of distances for which to summarize detection distances.
#' @param distance_interval The interval of distances to summarize in the table.
#'
#' @return A `dataframe` with a row for each distance interval, summarizing the number of sightings, etc.
#' @export
#' @import dplyr
#'
summarize_distances <- function(distances,
                                distance_range = c(0,10),
                                distance_interval = 0.5){

  if(FALSE){ #==================================================================
    data("cnp_150km_1986_2020")
    (distances <- summarize_species('046',
                                    cnp_150km_1986_2020)$sightings %>%
        filter(included == TRUE) %>%
        pull(PerpDistKm))
    distance_interval = 0.5
    distance_range = c(0,10)
    summarize_distances(distances)
  } #===========================================================================

  # Format distances
  (perps <- distances %>% sort)

  # Setup truncation distance steps
  (td_range <- c(0, max(perps,na.rm=TRUE))) # truncation distance range
  (td_steps <- seq(min(td_range), (max(td_range) + distance_interval), by=distance_interval))
  (td_steps <- c(0, td_steps))

  # Build table
  distance_table <-
    data.frame(km_min_incl = td_steps,
               km_max_excl = lead(td_steps),
               sightings = c(hist(perps, breaks=td_steps, plot=FALSE)$counts,0)) %>%
    dplyr::mutate(km_mid = km_min_incl + 0.5*(km_max_excl - km_min_incl)) %>%
    dplyr::mutate(total_within = cumsum(sightings)) %>%
    dplyr::mutate(total_beyond = sum(sightings) - total_within) %>%
    dplyr::mutate(percent_beyond = (total_beyond / sum(sightings)*100)) %>%
    dplyr::select(km_min_incl, km_max_excl, sightings, percent_beyond, total_beyond, total_within, km_mid) %>%
    dplyr::filter(km_min_incl >= min(distance_range),
                  km_max_excl <= max(distance_range))

  distance_table
  return(distance_table)

}
