#' Base map for plotting survey data
#'
#' Use this function io initiate a map of your survey.
#'
#' @param region  Name of the survey region you wish to map.
#' Options currently supported:
#' `"cnp"` (the default; Central North Pacific), `"ccs"` (California Current System),
#' and `"etp"` (Eastern Tropical Pacific).
#' This argument will define the default bounding box of the map and the EEZ's that are plotted.
#' @param eez_color  Color of the EEZ boundary lines.
#' @param eez_stroke  Width of the EEZ boundary lines.
#' @param land_color  Color of land in the map.
#' @param land_stroke  Width of the borders of land.
#' @param xlims  Custom longitude limits, provided as a two-element numeric vector of decimal degrees with West coordinates negative; useful if you wish to use a "zoom" different from the default set by `region`.
#' @param ylims  Custom latitude limits (South coordinates are negative).
#' @param scale_max  Distance (in km) to be represented by the scale.
#'
#' @return A `tmap` object (the map is built using the package `tmap`), which will print as a plot if not saved as an object.
#' If saved as an object, other features can be added to the map (see `map_effort()`, `map_strata()`, and `map_sightings()`,
#' or other `tmap` functions).
#'
#' @export
#'
map_base <- function(region='cnp',
                     eez_color = 'grey90',
                     eez_stroke = 2,
                     land_color = 'grey60',
                     land_stroke = 0.5,
                     xlims=NULL,
                     ylims=NULL,
                     scale_max=1000){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    region='ccs'
    eez_color = 'grey90'
    eez_stroke = 2
    land_color = 'grey60'
    land_stroke = 0.5
    xlims=NULL
    ylims=NULL
    scale_max=1000

    map_base('cnp')
  }
  #=============================================================================

  # Load coast / land / eez datasets (built into LTabundR)
  data(coastline)
  coast <- coastline
  data(land, package='LTabundR')
  mapland <- land
  data(eez)

  # Subset eez to polygons of interest for preset region options
  caorwa <- sf::st_geometry(eez[eez$REGION=='Pacific Coast',])
  hawaii <- sf::st_geometry(eez[eez$REGION=='Hawaiian Islands',])

  # Save csv of main hawaii eez to use as new stratum polygon
  # (only did this once -- you will not need this code again)
  #hawaii_stratum <- hawaii[8]
  #coords <- data.frame(st_coordinates(hawaii_stratum))
  #names(coords)[1:2] <- c('Lon','Lat')
  #coords$L1 <- coords$L2 <- NULL
  #coords
  #write.csv(coords,file='HI-EEZ_2447634.9.csv',quote=FALSE,row.names=FALSE)

  # If the region input is not recognized...
  if(! tolower(region) %in% c('ccs','cnp','etp')){
    # If xlim / ylim not provided, create limits
    if(is.null(xlims)){xlims <- c(-180,180)}
    if(is.null(ylims)){ylims <- c(-90,90)}

    # Establish the tmap bounding box using those limits
    bb_custom <- tmaptools::bb(xlim=xlims,ylim=ylims)

    # Create map
    m <- tmap::tm_shape(mapland, bbox=bb_custom) +
         tmap::tm_fill(col=land_color) +
         tmap::tm_borders(lwd=land_stroke) +
         tmap::tm_compass(type = "arrow", position = c("left", "top"), size=2, text.size=.5) +
         tmap::tm_scale_bar(breaks = c(0, round(scale_max/2), scale_max), text.size = .5, position=c('right','top')) +
         tmap::tm_graticules(lines=FALSE)
  }else{

    # California Current System map
    if(tolower(region) == 'ccs'){
      # create bounding box
      bb_caorwa <- tmaptools::bb(xlim=c(-132,-117),ylim=c(30,49.2))

      # build map
      m <- tmap::tm_shape(caorwa, bbox=bb_caorwa) +
           tmap::tm_lines(col=eez_color, lwd=eez_stroke) +
           tmap::tm_shape(mapland) +
           tmap::tm_fill(col=land_color) +
           tmap::tm_borders(lwd=land_stroke) +
           tmap::tm_compass(type = "arrow", position = c("right", "bottom"), size=2, text.size=.5) +
           tmap::tm_scale_bar(breaks = c(0, 150, 300), text.size = .5, position=c('left','top')) +
           tmap::tm_graticules(lines=FALSE)
    }

    # Central North Pacific (Hawaii)
    if(tolower(region) == 'cnp'){
      # Since eez spans date line, handle by switching everything to UTM
      hawaii_crs <- "+proj=utm +zone=5 +datum=WGS84"
      #hawaii_utm <- hawaii
      #coast_utm <- coast
      #land_utm <- land
      hawaii_utm <- sf::st_transform(hawaii,crs=hawaii_crs)
      coast_utm <- sf::st_transform(coast,crs=hawaii_crs)
      land_utm <- sf::st_transform(mapland,crs=hawaii_crs)

      # Make sure none of the polygons are empty/missing
      coast_utm_ne = coast_utm[!sf::st_is_empty(coast_utm),drop=FALSE]
      coast_utm_poly = sf::st_polygonize(coast_utm_ne)

      # Build map
      m <- tmap::tm_shape(hawaii_utm) +
           tmap::tm_lines(col=eez_color,lwd=eez_stroke) +
           tmap::tm_scale_bar(breaks = c(0, 150, 300), text.size = .5, position=c('left','top')) +
           tmap::tm_shape(coast_utm_poly, bbox=sf::st_bbox(hawaii_utm)) +
           tmap::tm_fill(col=land_color) +
           tmap::tm_borders(lwd=land_stroke)  +
           tmap::tm_compass(type = "arrow", position = c("right", "bottom"), size=2, text.size=.5) +
           tmap::tm_graticules(lines=FALSE)
    }


    # Eastern Tropical Pacific
    if(tolower(region) == 'etp'){
      # Set bounding box
      bb_etp <- tmaptools::bb(xlim=c(-160,-70),ylim=c(-20,35))

      # Create map
      m <- tmap::tm_shape(mapland, bbox=bb_etp) +
           tmap::tm_fill(col=land_color) +
           tmap::tm_borders(lwd=land_stroke) +
           tmap::tm_compass(type = "arrow", position = c("left", "top"), size=2, text.size=.5) +
           tmap::tm_scale_bar(breaks = c(0, 500, 1000), text.size = .5, position=c('right','top')) +
           tmap::tm_graticules(lines=FALSE)
    }

  }

  # Return base map
  return(m)
}

