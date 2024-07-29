#' Find DAS rows occurring within a region / regions
#'
#' This is an internal function, typically not called by the user.
#'
#' @param das The `DAS` table from a `cruz` object.
#' @param regions A character vector of geostrata whose segments you want.
#'
#' @return A vector of `line_num` values whose data occur within any of `regions`,
#' which can be used to filter segments and sightings.
#'
#' @export
#'
region_das <- function(das, regions){
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
  (das_lines <- das$line_num[in_strata])

  # Troubleshooting / debugging
  if(FALSE){
    dasi <- das %>% dplyr::filter(line_num %in% das_lines)
    plot(x=das$Lon, y=das$Lat, pch=16, cex=.1, col='grey')
    points(x=dasi$Lon,
           y=dasi$Lat,
           pch=16, cex=.2, col='black')
    dasi$Lon
    dasi$Lat
  }

  return(das_lines)
}
