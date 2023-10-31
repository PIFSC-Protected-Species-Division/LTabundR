#' Format a CSV into a polygon for mapping
#'
#' This is an internal function typically not called by a user directly.
#' It takes a dataframe of polygon coordinates and returns a list with the polygon formatted in various ways,
#' as well as a calculation of the area of the polygon *with land area removed* (optionally).
#'
#' @param polygon_dataframe  A dataframe in which the first two columns are `Lon` and `Lat`,
#' providing coordinates in decimal degrees in which South and West coordinates are negative.
#' It is acceptable if vertices in the eastern hemisphere are described using negative longitudes below -180, e.g., -185.
#' (The function will correct these to proper decimal degrees, e.g., -185 will become 175.)
#' Other columns are allowed, but the first two need to be `Lon` and `Lat` (in that order).
#' @param remove_land If `TRUE`, the area of land occurring within the polygon will
#' be subtracted from the polygon's area, so that the reported area represents aquatic habitat only.
#' @param toplot If `TRUE` (not the default), a map of the polygon will be plotted,
#' and -- if `remove_land` is `TRUE` -- any land inside will be plotted in grey.
#'
#' @return A list with four named slots:
#' \enumerate{
#' \item `coords` The input dataframe, with longitudes corrected to proper negative decimal degrees, e.g., longitudes supplied as -185 will become 175.
#' \item `polygon`  The polygon created by `sf::st_polygon()`
#' \item `sf`  The `sf` version of the polygon that will be passed to mapping functions.
#' \item `sf_land` The `sf` version of the land polygons included within the polygon, if any.
#' \item `km2`  The area of the polygon, in square kilometers.
#' }
#'
#' @export
#'
process_polygon <- function(polygon_dataframe,
                            remove_land = TRUE,
                            toplot = FALSE){

  # This function now converts all polygon coordinates to proper decimal degrees, ranging from -180 to 180.
  # This is true even if the user supplies longitudes such as -185. (Those will become 175).

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    # No longer doing this
    # coerce_lons If `TRUE` (not the default), all longitudes will be coerced to negative values, e.g., -185. This can avoid complications when strata or survey data span the international date line.

    remove_land = TRUE
    toplot = TRUE
    coerce_lons <- FALSE

    data(strata_cnp)
    length(strata_cnp)
    polygon_dataframe <- strata_cnp[[2]]

    data(strata_ccs)
    length(strata_ccs)
    polygon_dataframe <- strata_ccs[[5]]

    process_polygon(polygon_dataframe, remove_land=TRUE, toplot=TRUE)$km2
    process_polygon(polygon_dataframe, remove_land=FALSE, toplot=TRUE)$km2

    polygon_dataframe <- data.frame(Lon = c(142, 142, 148, 148),
                                    Lat = c(10, 18, 18, 10))
    polygon_dataframe <- data.frame(Lon = c(142, 142, -170, -170),
                                    Lat = c(10, 40, 40, 10))
    process_polygon(polygon_dataframe, remove_land=TRUE, toplot=TRUE)$km2
    process_polygon(polygon_dataframe, remove_land=FALSE, toplot=TRUE)$km2
  }

  #=============================================================================

  # Ensure that coordinates are numeric
  polygon_dataframe$Lon <- as.numeric(polygon_dataframe$Lon)
  polygon_dataframe$Lat <- as.numeric(polygon_dataframe$Lat)
  coords <- polygon_dataframe ; coords
  coords <- data.frame(Lon = coords$Lon, Lat = coords$Lat)
  coords <- coords[complete.cases(coords),]

  # Make sure polygon is closed
  (closed_test <- all(c(coords$Lon[1] == coords$Lon[nrow(coords)],
                        coords$Lat[1] == coords$Lat[nrow(coords)])))
  if(! closed_test){
    coords <- rbind(coords, coords[1,])
  }
  coords

  #=============================================================================

  # Test to see if this polygon crosses the IDL
  (idl_test <- any(c(any(coords$Lon < -180),
                     all(c(any(coords$Lon < 0), any(coords$Lon >= 0))))))

  if(!idl_test){
    # Normal -- does not cross date line
    coords_mx <- as.matrix(coords[,1:2])
    poli <- sf::st_polygon(list(coords_mx))
    sf_pol <- sf::st_geometry(poli)

  }else{
    # Coerce Lons to all negative at first
    (bads <- which(coords$Lon > 0))
    coordi <- coords
    coordi$Lon[bads] <- coordi$Lon[bads] - 360
    coordi
    coords_mx <- as.matrix(coordi[,1:2])
    poli <- sf::st_polygon(list(coords_mx))
    sf_poli <- sf::st_geometry(poli)
    #ggplot(sf_poli) + geom_sf()

    # Split into an East and West polygon
    polw <- st_crop(sf_poli, xmin = -359.99999, ymin = -90, xmax = -180.00000001, ymax = 90)
    # Return these longitudes to original values
    polw <- polw + c(360, 0)
    #ggplot(polw) + geom_sf()
    pole <- st_crop(sf_poli, xmin = 359.99999, ymin = -90, xmax = -179.9999999, ymax = 90)
    #ggplot(pole) + geom_sf()

    # Combine into single sf
    comb_poli <- list(polw, pole)
    sf_poli <- purrr::accumulate(comb_poli, st_union)
    sf_pol <- sf_poli[[length(sf_poli)]]
    sf_pol <- sf::st_geometry(sf_pol)
  }

  # Set CRS
  st_crs(sf_pol) <- 4326
  if(toplot){
    print(ggplot(sf_pol) +
      geom_sf(alpha=.5) +
        labs(title='Polygon before considering land'))
  }

  # Get area
  (x <- st_area(sf_pol))
  (poli_area <- units::set_units(x, km^2))
  poli_area_w_land <- poli_area

  # Find any land occurring within the polygon
  landint <- NULL
  if(remove_land){
    sf::sf_use_s2(FALSE)
    sfpol <- sf::st_make_valid(sf_pol)

    # Bring in land dataset
    data(land, package='LTabundR')
    sf::st_crs(sfpol) <- sf::st_crs(land)

    # remove land
    landcrop <- land
    suppressWarnings({suppressMessages({
      (landint <- sf::st_intersection(landcrop, sfpol))
    })})
    (areas_m2 <- sf::st_area(landint$geometry))
    (area_m2 <- areas_m2 %>% sum)
    (area_km2 <- units::set_units(area_m2, km^2))
    (poli_area <- poli_area - area_km2)
    if(toplot){
      print(ggplot() +
        geom_sf(data=sfpol, alpha=.5) +
        geom_sf(data=landint, alpha=.5, fill='grey40') +
        labs(title='Polygon with land'))
    }
  }

  # Prepare return list
  return_list <- list(coords = coords,
                      polygon = poli,
                      sf = sf_pol,
                      sf_land = landint,
                      km2 = poli_area,
                      km2_with_land = poli_area_w_land)

  return(return_list)
}
