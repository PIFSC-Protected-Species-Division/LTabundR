#' Format a CSV into a polygon for mapping
#'
#' This is an internal function typically not called by a user directly.
#' It takes a dataframe of polygon coordinates and returns a list with the polygon formatted in various ways,
#' as well as a calculation of the area of the polygon *with land area removed* (optionally).
#'
#' @param polygon_dataframe  A dataframe in which the first two columns are `Lon` and `Lat`,
#' providing coordinates in decimal degrees in which South and West coordinates are negative.
#' Other columns are allowed, but the first two need to be `Lon` and `Lat` (in that order).
#' @param remove_land If `TRUE`, the area of land occurring within the polygon will
#' be subtracted from the polygon's area, so that the reported area represents aquatic habitat only.
#' @param toplot If `TRUE` (not the default), a map of the polygon will be plotted,
#' and -- if `remove_land` is `TRUE` -- any land inside will be plotted in grey.
#'
#' @return A list with four named slots:
#' \enumerate{
#' \item `coords` The input dataframe.
#' \item `polygon`  The polygon created by `sf::st_polygon()`
#' \item `sf`  The `sf` version of the polygon that will be passed to mapping functions.
#' \item `km2`  The area of the polygon, in square kilometers.
#' }
#'
#' @export
#'
process_polygon <- function(polygon_dataframe,
                            remove_land = TRUE,
                            toplot = FALSE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    remove_land = TRUE
    toplot = TRUE

    data(strata_cnp)
    length(strata_cnp)
    polygon_dataframe <- strata_cnp[[2]]

    data(strata_ccs)
    length(strata_ccs)
    polygon_dataframe <- strata_ccs[[5]]

    process_polygon(polygon_dataframe, remove_land=TRUE, toplot=TRUE)$km2
  }
  #=============================================================================

  # Ensure that coordinates are numeric
  polygon_dataframe$Lon <- as.numeric(polygon_dataframe$Lon)
  polygon_dataframe$Lat <- as.numeric(polygon_dataframe$Lat)

  # Format as sf polygon
  coords <- polygon_dataframe ; coords
  coords$Lon <- as.numeric(coords$Lon)
  coords$Lat <- as.numeric(coords$Lat)
  coords <- data.frame(Lon = coords$Lon, Lat = coords$Lat)
  coords <- coords[complete.cases(coords),]

  # Make sure polygon is closed
  (closed_test <- all(c(coords$Lon[1] == coords$Lon[nrow(coords)],
                        coords$Lat[1] == coords$Lat[nrow(coords)])))
  if(! closed_test){
    coords <- rbind(coords, coords[1,])
  }

  # Finalize sf formatting
  coords_mx <- as.matrix(coords[,1:2])
  poli <- sf::st_polygon(list(coords_mx))
  sf_poli <- sf::st_geometry(poli)

  # Calculate area
  lng <- coords$Lon
  lat <- coords$Lat
  d <- cbind(lng, lat)
  poli_area <- geosphere::areaPolygon(d) *10^(-6)
  poli_area_w_land <- poli_area

  sfpol <- sf_poli
  class(sf_poli)
  if(toplot){
    par(mfrow=c(1,1), mar=c(4.5,4.5,.5,.5))
    plot(sfpol, axes=TRUE, lwd=2)
  }

  # Find any land occurring within the polygon
  if(remove_land){
    sf::sf_use_s2(FALSE)
    sfpol <- sf::st_make_valid(sfpol)

    # Bring in land dataset
    data(land, package='LTabundR')
    sf::st_crs(sfpol) <- sf::st_crs(land)

    # Define a bounding box by which to crop land
    box <- c(xmin = min(polygon_dataframe$Lon),
             ymin = min(polygon_dataframe$Lat),
             xmax = max(polygon_dataframe$Lon),
             ymax = max(polygon_dataframe$Lat))
    box

    # remove land
    landcrop <- sf::st_crop(land, box)
    if(length(landcrop)>0){
      suppressWarnings({suppressMessages({
        (landint <- sf::st_intersection(landcrop, sfpol))
        (areas_m2 <- sf::st_area(landint$geometry))
      })})
      (areas_km2 <- as.numeric(areas_m2) / (10^6))
      (areas_tot <- sum(areas_km2))
      (poli_area <- poli_area - areas_tot)
      if(toplot){
        plot(landcrop, add=TRUE, col='grey')
        plot(landint, add=TRUE, col='grey40')
      }
    }
  }

  return_list <- list(coords = coords,
                      polygon = poli,
                      sf = sf_poli,
                      km2 = poli_area,
                      km2_with_land = poli_area_w_land)

  return(return_list)
}
