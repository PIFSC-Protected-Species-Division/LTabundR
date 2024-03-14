#' Add geostratum polygons to a survey map
#'
#' @param m Your `tmap` map object (produced from `map_base()` or any of the other `LTabundR` mapping functions).
#' @param settings A `settings` object, as produced by `LTabundR::load_settings()`.
#' @param study_area_color Color of study area boundary line.
#' @param study_area_stroke Width of study area boundary line.
#' @param strata_color Color of geostratum boundary line(s).
#' @param strata_stroke Width of geostratum boundary line(s).
#' @param region  Name of the survey region you wish to map.
#' Options currently supported:
#' `"cnp"` (the default; Central North Pacific), `"ccs"` (California Current System),
#' and `"etp"` (Eastern Tropical Pacific). This argument will define how coordinates are re-projected to handle the international date line.
#'
#' @return A `tmap` map with geostratum boundaries shown (the map is built using the package `tmap`), which will print as a plot if not saved as an object.
#' If saved as an object, other features can be added to the map (see `map_effort()`, `map_sightings()`,
#' or other `tmap` functions).
#'
#' @export
#'
map_strata <- function(m,
                       settings,
                       study_area_color = 'forestgreen',
                       study_area_stroke = 1,
                       strata_color = 'black',
                       strata_stroke = 1,
                       region='cnp'){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    m <- map_base('cnp')
    data(example_settings)
    settings <- example_settings
    data(example_cruz)
    cruz <- example_cruz
    region = 'cnp'
    study_area_color = 'forestgreen'
    study_area_stroke = 1
    strata_color = 'black'
    strata_stroke = 1

    # Try it
    map_strata(m, settings)
  }
  #=============================================================================

  # save polygon inputs as simply object names
  strata <- settings$strata
  study_area <- settings$study_area

  # projection to use
  hawaii_crs <- "+proj=utm +zone=5 +datum=WGS84"

  #=============================================================================
  # Add study area

  if(!is.null(study_area)){
    if(!is.numeric(study_area)){

      # process_polygon is a LTabundR function. It converts a dataframe to a sf polygon
      stratum <- process_polygon(study_area)
      poli <- stratum$sf

      # establish projection
      sf::st_crs(poli) <- 'WGS84'
      poli <- sf::st_transform(poli,crs=hawaii_crs)

      # Add to map
      m <-
        m +
        tmap::tm_shape(poli) +
        tmap::tm_borders(col=study_area_color, lwd=study_area_stroke)
    }
  }

  #=============================================================================
  # Add survey strata

  if(!is.null(strata)){
    # loop through each stratum provided in the strata list
    #i=1 # debugging
    for(i in 1:length(strata)){
      # format dataframe into polygon
      stratum <- process_polygon(strata[[i]])
      poli <- stratum$sf
      sf::st_crs(poli) <- 'WGS84'

      # If the region is CNP, transform to UTM to handle date line
      if(tolower(region=='cnp')){
        poli <- sf::st_transform(poli,crs=hawaii_crs)
      }

      # Add stratum to map
      m <-
        m +
        tmap::tm_shape(poli) +
        tmap::tm_borders(col=strata_color,lwd=strata_stroke)
    }
  }

  return(m)
}
