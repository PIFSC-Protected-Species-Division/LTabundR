#' Determine whether coordinates occur inside a stratum polygon
#'
#' This is an internal function typically not called by a user directly.
#'
#' @param lon Longitude coordinates in decimal degrees, in which West coordinates are negative.
#' @param lat Latitude coordinates in decimal degrees, in which South coordinates are negative.  Must be the same length as `lon`.
#' @param poli A dataframe of polygon coordinates.
#'
#' @return A logical vector, the same length as the `lon`,
#' indicating whether or not the coordinate falls within the polygon.
#' If the coordinate falls exactly on the polygon boundary, it will be counted as in.
#'
#' @import sf
#' @import dplyr
#' @export
#'
coordinates_in_strata <- function(lon,lat,poli){

  #=============================================================================
  # for debugging only -- not run!
  if(FALSE){
    data(strata_cnp)
    strata_cnp
    poli <- process_polygon(strata_cnp[[2]])$sf
    poli
    data(example_cruz)
    lon <- example_cruz$cohorts$default$das$Lon
    lat <- example_cruz$cohorts$default$das$Lat

    # Test the within v on boundary feature
    lon <- c(-131, -131, -131)
    lat = c(35, 40, 45)
    ggplot() +
      geom_point(mapping=aes(x=lon, y=lat), col='red', size=5) +
      geom_sf(data=poli, alpha=.5)
  }
  #=============================================================================

  # Format polygon for sf functions
  poli_sf <- sf::st_sf(1,sf::st_sfc(poli),crs=4326)

  # Format points as sf object
  pointi <- data.frame(Lon=lon, Lat=lat) ; pointi
  point_sf <- pointi %>% sf::st_as_sf(coords=c('Lon','Lat'),crs=4326)

  # Conduct test: within
  suppressMessages({
    sf::sf_use_s2(FALSE)
    pt_in_poli <- sf::st_join(point_sf, poli_sf)
  })
  (ins <- dplyr::as_tibble(pt_in_poli)[,1])
  (ins <- as.numeric(ins[[1]]))
  (withins <- !is.na(ins))

  # Create results
  #(tests <- data.frame(withins, inters))
  #(intersect_tests <- apply(tests, 1, any))
  intersect_tests <- withins
  table(intersect_tests)

  return(intersect_tests)
}

