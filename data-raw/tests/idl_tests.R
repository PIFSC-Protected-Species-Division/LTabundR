library(devtools)
document()
library(ggplot2)
library(dplyr)
library(sf)

data(strata_cnp)
strata_cnp %>% names
strata_cnp$OtherCNP

# Build test strata
# This one does not cross the IDL
stratum <- data.frame(Lon = c(-178, -80, -80, -178),
                      Lat = c(-20,  -10, 30,  50))

# This one crosses the IDL
stratum <- data.frame(Lon = c(-160, 160, 160, -160),
                      Lat = c(-20,  -10, 30,  50))

# OtherCNP is supplied using values like -185
(stratum <- strata_cnp$OtherCNP)

# Prove that it works the same with proper values
stratum$Lon[stratum$Lon < -180] <- stratum$Lon[stratum$Lon < -180] + 360
stratum

# Try other polygons
stratum <- strata_cnp$WHICEAS
stratum <- strata_cnp$HI_EEZ
polygon_dataframe <- stratum

plot(stratum)
process_polygon(stratum, toplot=TRUE)


data(example_settings)





# Relevant R files =============

# process_polygon
# process_strata
# strata_area
# map_cruz



#=============================================================================
#=============================================================================
#=============================================================================
#=============================================================================
#=============================================================================
#=============================================================================

# ARCHIVE OF processs_polygon before erasing changes

process_polygon <- function(polygon_dataframe,
                            remove_land = TRUE,
                            coerce_lons = FALSE,
                            toplot = FALSE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
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
  (idl_test <- all(c(any(strati$Lon < 0), any(strati$Lon >= 0))))

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
    ggplot(sf_poli) + geom_sf()

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
  if(toplot){ ggplot(sf_pol) + geom_sf(lwd=1) }

  # Get area
  (x <- st_area(sf_pol))
  (poli_area <- units::set_units(x, km^2))
  poli_area_w_land <- poli_area


  #=============================================================================
  # PASS 1: Get area and subtract land area (if instructed)

  # Finalize sf formatting
  # coords_mx <- as.matrix(coords[,1:2])
  # poli <- sf::st_polygon(list(coords_mx))
  # sf_poli <- sf::st_geometry(poli)
  # #plot(sf_poli)
  #
  # # Calculate area
  # lng <- coords$Lon
  # lat <- coords$Lat
  # d <- cbind(lng, lat)
  # poli_area <- geosphere::areaPolygon(d) *10^(-6)
  # poli_area_w_land <- poli_area
  #
  # sfpol <- sf_poli
  # class(sf_poli)
  # if(toplot){
  #   par(mfrow=c(1,1), mar=c(4.5,4.5,.5,.5))
  #   plot(sfpol, axes=TRUE, lwd=2)
  # }

  # Find any land occurring within the polygon
  landint <- NULL
  if(remove_land){
    sf::sf_use_s2(FALSE)
    sfpol <- sf::st_make_valid(sf_pol)

    # Bring in land dataset
    data(land, package='LTabundR')
    sf::st_crs(sfpol) <- sf::st_crs(land)

    # Define a bounding box by which to crop land
    #box <- c(xmin = min(polygon_dataframe$Lon),
    #         ymin = min(polygon_dataframe$Lat),
    #         xmax = max(polygon_dataframe$Lon),
    #         ymax = max(polygon_dataframe$Lat))
    #box

    # remove land
    landcrop <- land
    #landcrop <- sf::st_crop(land, box)
    #if(length(landcrop)>0){
    suppressWarnings({suppressMessages({
      (landint <- sf::st_intersection(landcrop, sfpol))
    })})
    (areas_m2 <- sf::st_area(landint$geometry))
    (area_m2 <- areas_m2 %>% sum)
    (area_km2 <- units::set_units(area_m2, km^2))
    #(areas_km2 <- as.numeric(areas_m2) / (10^6))
    #(areas_tot <- sum(areas_km2))
    (poli_area <- poli_area - area_km2)
    if(toplot){
      ggplot() +
        geom_sf(data=sfpol) +
        geom_sf(data=landint)
      #plot(landcrop, add=TRUE, col='grey')
      #plot(landint, add=TRUE, col='grey40')
    }
    #}
  }

  #=============================================================================
  # PASS 2: Correct longitudes, if needed, and finalize sf return object

  # # If polygon spans the international date line, coerce to all negative
  # # or if coerce_lons is TRUE
  # coerce_loni <- FALSE
  # if(coerce_lons){coerce_loni <- TRUE}
  # if(coerce_loni == FALSE){
  #   bads <- which(coords$Lon > 0)
  #   negs <- which(coords$Lon < 0)
  #   if(length(negs)>0 & length(bads) > 0){ coerce_loni <- TRUE }
  # }
  # if(coerce_loni){
  #   bads <- which(coords$Lon > 0)
  #   coords$Lon[bads] <- -180 + (coords$Lon[bads] - 180)
  # }
  # coords
  # #coerce_loni
  #
  # # Make sure polygon is closed
  # (closed_test <- all(c(coords$Lon[1] == coords$Lon[nrow(coords)],
  #                       coords$Lat[1] == coords$Lat[nrow(coords)])))
  # if(! closed_test){
  #   coords <- rbind(coords, coords[1,])
  # }
  # coords
  #
  # # Finalize sf formatting
  # coords_mx <- as.matrix(coords[,1:2])
  # poli <- sf::st_polygon(list(coords_mx))
  # sf_poli <- sf::st_geometry(poli)
  # #plot(sf_poli)

  # Prepare reeturn list
  return_list <- list(coords = coords,
                      polygon = poli,
                      sf = sf_poli,
                      sf_land = landint,
                      km2 = poli_area,
                      km2_with_land = poli_area_w_land)

  return(return_list)
}
