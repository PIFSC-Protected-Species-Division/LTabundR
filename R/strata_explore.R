#' Explore the package's strata polygon gallery
#'
#' The `LTabundR` package comes with several built-in datasets of geographic strata
#' that are commonly used in NOAA/NMFS surveys. The functions `strata_explore()`
#' and `strata_select()` were developed to help you explore those built-in options.
#' \cr \cr
#' `strata_explore()` launches a `leaflet` map displaying the built-in
#' geostrata datasets available for your use in the common NOAA/NMFS survey regions.
#' Each stratum displayed as a separate layer that can be hidden or hovered over to get details.
#'
#' @param region  Name of the survey region of interest. Options currently supported:
#' `"cnp"` (the default; Central North Pacific), `"ccs"` (California Current System),
#' and `"etp"` (Eastern Tropical Pacific).#'
#' @param start_at A number indicating the geostratum index you wish to begin at, if you know you aren't interested in those early on in the list. For example, the ETP has over 70 built-in geostrata available!
#' @param strata_color Color of geostratum.
#' @param begin_hidden If `TRUE` (not the default), the strata will begin hidden, i.e.,
#' unchecked, so that you have to opt to visualize each stratum individually.
#' This can be handy for regions with many geostrata to choose from, such as the
#' ETP, which has 70 geostrata!
#'
#' @return An interactive `leaflet` map.
#' The name of the geostratum, and the numeric identifier for it, are printed
#' in a legend as well as in a pop-up box that appears when you hover over a stratum.
#' Use this identifier as an argument in the companion function, `strata_select()`, to access the datasets of coordinates
#' underlying the polygons displayed.
#'
#' @import leaflet
#'
#' @export
#'
strata_explore <- function(region,
                           start_at = 1,
                           strata_color='darkblue',
                           begin_hidden = FALSE){
  #=============================================================================
  # for debugging only -- not run!
  if(FALSE){
    region <- 'ccs'
    region <- 'cnp'
    start_at = 1
    strata_color = 'darkblue'
    begin_hidden = TRUE
  }
  #=============================================================================

  if(region == 'cnp'){
    data(strata_cnp)
    lf <- strata_cnp
  }
  if(region == 'etp'){
    data(strata_etp)
    lf <- strata_etp
  }
  if(region == 'ccs'){
    data(strata_ccs)
    lf <- strata_ccs
  }

  lf

  # Stage empty leaflet map
  m <- leaflet::leaflet() %>% leaflet::addTiles()
  m

  # Add each polygon separately
  strata_names <- c()
  message('Compiling leaflet map with ',(length(lf) - (start_at + 1)),' geostrata ...')
  i=1
  for(i in start_at:length(lf)){
    (lfi <- lf[i])
    lfi[[1]] <- lfi[[1]][!is.na(lfi[[1]]$Lat) & !is.na(lfi[[1]]$Lon),]
    lfi
    (stratum_name <- names(lfi)[1])
    strata_names <- c(strata_names, paste0(i,'. ',stratum_name))
    message('--- ',i,' :: ', stratum_name, ' ...')
    suppressMessages({stratum_proc <- process_polygon(lfi[[1]])})
    stratum_proc %>% names
    (stratum_km2 <- stratum_proc$km2)
    poli <- stratum_proc$sf

    # add polygon to map as its own group with its own label
    m <-
      m %>% leaflet::addPolygons(data = poli,
                        color = strata_color,
                        opacity = .3,
                        group = paste0(i,'. ', stratum_name),
                        label = paste0(i,'. ', stratum_name,' :: ',format(round(stratum_km2), big.mark=','), ' km2'))

    if(begin_hidden){
      m <- m %>% leaflet::hideGroup(group = paste0(i,'. ', stratum_name))
    }
  }


  # Now add the layer control to check/uncheck polygons
  m %>%
    leaflet::addLayersControl(
      overlayGroups = strata_names,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

}
