#' Find segments occurring within a region / regions
#'
#' This is an internal function, typically not called by the user.
#'
#' @param das The `DAS` table from a `cruz` object.
#' @param regions A character vector of geostrata whose segments you want.
#'
#' @return A vector of segment IDs whose data occur within `regions`,
#' which can be used to filter segments and sightings by the `seg_id` column.
#'
#' @export
#'
region_segments <- function(das, regions){
  # Interior function for finding segments within a stratum (for filtering by region)

  if(FALSE){ # for debugging ===================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    das <- cruz$cohorts$all$das
    regions <- c('MHI')
  } # end debugging area  ======================================================

  (regions <- gsub('-','.',regions))
  (region_cols <- paste0('stratum_',regions))

  # Find the seg_ids that pertain to the 'regions' you want to incldue
  (matches <- which(gsub('-','.',names(das)) %in% region_cols))
  names(das)[matches]
  in_strata <- apply(as.data.frame(das[,matches]),1,
                     function(x){ifelse(any(x),TRUE, FALSE)})
  in_strata %>% table
  (seg_ids <- das$seg_id[in_strata] %>% unique)

  # Troubleshooting / debugging
  # plot(x=das$Lon, y=das$Lat, pch=16, cex=.1, col='grey',
  #      xlim=c(-167, -162), ylim=c(22, 25))
  # points(x=das$Lon[in_strata],
  #       y=das$Lat[in_strata],
  #       pch=16, cex=.2, col='black')
  # points(x=sightings$Lon, y=sightings$Lat, col='red',pch=16)
  # dasi <- cruz$cohorts$most$density$das %>% dplyr::filter(seg_id == 1398)
  # lines(x=dasi$Lon, y=dasi$Lat, col='red')
  # dasi$Lon
  # dasi$Lat

  return(seg_ids)
}
