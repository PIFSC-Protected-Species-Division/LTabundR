#' Interactive map for `Wincruz` survey
#'
#' Produced an interactive `leaflet` map of your `Wincruz` survey data.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param eez_show Boolean; display EEZ boundaries?
#' @param eez_color Character string; color of EEZ boundary lines, if `eez_show` is `TRUE`.
#' @param eez_weight Single positive numeric; weight (thickness) of EEZ boundary lines, if `eez_show` is `TRUE`.
#' @param eez_opacity Single numeric between 0 and 1; opacity of EEZ boundary lines, if `eez_show` is `TRUE`.
#' @param strata_show Boolean; display geostratum boundaries?
#' @param strata_color Character string; color of geostratum boundary lines, if `strata_show` is `TRUE`.
#' @param strata_weight Single positive numeric; weight (thickness) of geostratum boundary lines, if `strata_show` is `TRUE`.
#' @param strata_opacity Single numeric between 0 and 1; opacity of geostratum boundary lines, if `strata_show` is `TRUE`.
#' @param effort_show Boolean; display survey tracks? Default is `FALSE`, because rendering the tracklines can take a while.
#' If `verbose` is `TRUE`, a progress bar will be printed to the console as the survey tracklines are rendered.
#' @param effort_color Single character string; color of survey tracklines, if `effort_show` is `TRUE`.
#' @param effort_weight Single positive numeric; weight (thickness) of survey tracklines, if `effort_show` is `TRUE`.
#' @param effort_opacity Single numeric between 0 and 1; opacity of survey tracklines, if `effort_show` is `TRUE`.
#' @param sightings_show Boolean; display sightings?
#' @param sightings_color A single character string, indicating the color for sighting markers.
#' **Note:** if you enter `"color code"`, the sightings will be color-coded by `leaflet` according to species code,
#' and a legend will be provided; this argument is ignored if `sightings_show` is `FALSE`.
#' @param sightings_radius A single postive numeric, indicating the size of the sightings markers; this argument is ignored if `sightings_show` is `FALSE`.
#' @param sightings_opacity A single numeric between 0 and 1, indicating the opacity of sightings markers; this argument is ignored if `sightings_show` is `FALSE`.
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#' (The only updates occur in the preparation of effort tracks, if `effort_show` is `TRUE`)
#'
#' @return An interactive `leaflet` map.
#' @export
#'
map_cruz <- function(cruz,
                     cohort=1,

                     eez_show=TRUE,
                     eez_color = 'black',
                     eez_weight = 0.6,
                     eez_opacity = 0.5,

                     strata_show=TRUE,
                     strata_color = 'forestgreen',
                     strata_weight = 1,
                     strata_opacity = 0.5,

                     effort_show=FALSE,
                     #effort_resolution=15,
                     effort_color = 'darkblue',
                     effort_weight = 0.6,
                     effort_opacity = 0.85,

                     sightings_show=TRUE,
                     sightings_color = 'color code',
                     sightings_radius = 1,
                     sightings_opacity = 0.5,
                     verbose=TRUE){

  if(FALSE){ # for debugging only -- not run! ==================================
    m <- map_base()
    data(example_cruz)
    cruz <- example_cruz
    cohort = 1
    eez_show=TRUE
    eez_color = 'black'
    eez_weight = 0.6
    eez_opacity = 0.5

    strata_show=TRUE
    strata_color = 'forestgreen'
    strata_weight = 1
    strata_opacity = 0.5

    effort_show=FALSE
    effort_resolution=15
    effort_color = 'darkblue'
    effort_weight = 0.6
    effort_opacity = 0.85

    sightings_show=TRUE
    sightings_color = 'color code'
    sightings_radius = 1
    sightings_opacity = 0.5

    # @param effort_resolution Single numeric integer equal to or larger than 1, indicating the data resolution of survey tracklines, if `effort_show` is `TRUE`.
    # A value of `1` means full resolution (no data rows are removed), and can take minutes to render.
    # Higher values mean lower resolution; in practice, the number provided represents the number of data lines skipped.
    # For example, a value of `3` means that only every fourth data line is used to build tracks.
    # For your initial call, we recommend the default (`15`).

    # try it
    map_cruz(cruz)
    map_cruz(cruz, sightings_color='firebrick')
    map_cruz(cruz, effort_show=TRUE, sightings_show=FALSE)
    map_cruz(cruz, effort_show=TRUE, effort_resolution=10, sightings_show=TRUE, sightings_color='firebrick')

  } # end of debugging staging area ============================================

  # Filter to correct cohort and analysis
  cohorti <- cruz$cohorts[[cohort]] ; names(cohorti)
  ani <- cohorti ; names(ani)

  # Prepare base datasets
  segments <- ani$segments
  sits <- ani$sightings
  das <- ani$das

  # Load eez data
  #data(eez_ccs)
  #data(eez_hawaii)
  data(eez)

  # Create base leaflet map
  m <-
    leaflet::leaflet() %>%
    leaflet::addTiles(options = leaflet::providerTileOptions(opacity = .5))
    #leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap,
    #                          options = leaflet::providerTileOptions(opacity = .5))

  # Add EEZ
  if(eez_show){
    m <-
      m %>%
      leaflet::addPolylines(data=eez, weight=eez_weight, color=eez_color, opacity=eez_opacity)
      #leaflet::addPolylines(data=eez_hawaii, weight=eez_weight, color=eez_color, opacity=eez_opacity) %>%
      #leaflet::addPolylines(data=eez_ccs, weight=eez_weight, color=eez_color, opacity=eez_opacity)
  }

  # Add geostrata
  if(strata_show){
    # save polygon inputs as simply object names
    strata <- cruz$settings$strata
    study_area <- cruz$settings$study_area

    #=============================================================================
    # Add study area
    if(!is.null(study_area)){
      if(!is.numeric(study_area)){
        # process_polygon is a LTabundR function. It converts a dataframe to a sf polygon
        stratum <- process_polygon(study_area)
        poli <- stratum$sf

        m <-
          m %>%
          leaflet::addPolylines(data=poli,
                                weight=strata_weight,
                                color=strata_color,
                                opacity=strata_opacity)
        }
    }

    #=============================================================================
    # Add survey strata
    if(!is.null(strata)){
      # loop through each stratum provided in the strata list
      i=1
      for(i in 1:length(strata)){
        # format dataframe into polygon
        stratum <- process_polygon(strata[[i]])
        poli <- stratum$sf
        m <-
          m %>%
          leaflet::addPolylines(data=poli,
                                weight=strata_weight,
                                color=strata_color,
                                opacity=strata_opacity,
                                popup= paste0('<b>Geostratum ',names(strata)[i],'</b><br>',
                                              'Area: ',round(stratum$km2),' km\n'))
      }
    }
  }

  #=============================================================================
  # Add effort tracks
  if(effort_show){
    head(das)

    m
    dasi <- das #[1:100000,]
    dasi <- das %>% filter(!is.na(seg_id))
    #(whiches <- c(TRUE,rep(FALSE,times=effort_resolution)))
    #(whiches <- rep(whiches, times=ceiling(nrow(dasi)/effort_resolution)))
    #dasi <- dasi[whiches[1:nrow(dasi)], ]
    dasi$km_int <- process_km(das=dasi, max_km_gap=Inf, debug_mode=FALSE)

    dasii <- data.frame()
    groupid <- 1
    segid_pre <- dasi$seg_id[1]
    if(verbose){pb <- txtProgressBar(1, nrow(dasi), style=3)} # setup progress bar
    for(i in 1:nrow(dasi)){
      (segidi <- dasi$seg_id[i])
      if(is.na(segidi)){segidi <- 9999}
      (kmi <- dasi$km_int[i,1])
      if(segidi != segid_pre | kmi > 20){
        if(nrow(dasii)>1){

          # Filter track
          usei <- dasii$use %>% table %>% sort %>% rev %>% names %>% head(1)
          opi <- ifelse(usei==TRUE, effort_opacity, (effort_opacity / 2))
          colori <- ifelse(usei==TRUE, effort_color, effort_color)
          weighti <- ifelse(usei==TRUE, effort_weight, (effort_weight / 2))

          # Add track to plot
          m <- m %>%
            leaflet::addPolylines(data=dasii,lng = ~Lon,lat = ~Lat,
                                  color = colori,
                                  opacity = opi,
                                  weight = weighti,
                                  popup= paste0('<b>Segment ',segidi,'</b><br>',
                                                       dasii$DateTime[1],'<br>',
                                                       'Cruise ',dasii$Cruise[1],'<br>',
                                                       'EffType: ',dasii$EffType[1],'<br>',
                                                       'OnEffort: ',dasii$OnEffort[1],'<br>',
                                                       'Bft: ',dasii$Bft[1],'\n'))
        }
        # reset variables
        segid_pre <- segidi
        dasii <- data.frame()
      }else{
        # Keep building track
        dasii <- rbind(dasii, dasi[i,])
      }
      if(verbose){setTxtProgressBar(pb, i)} # update progress bar
    }
    if(verbose){message('\n')}
    #m
  }
  #=============================================================================
  #=============================================================================
  # Add sightings
  if(sightings_show){

    if(sightings_color == 'color code'){
      pal <- leaflet::colorFactor(palette = "Dark2", domain = sits$species)
      m <-
        m %>%
        leaflet::addCircleMarkers(data=sits,
                                  lng = ~Lon, lat = ~Lat,
                                  radius=sightings_radius,
                                  opacity=sightings_opacity,
                                  color = ~pal(species),
                                  popup= paste0('<b>Species ',sits$species,'</b><br>',
                                               sits$DateTime,'<br>',
                                               'Cruise ',sits$Cruise,'<br>',
                                               'School size: ',round(sits$best,2),'\n')
                                  ) %>%
        leaflet::addLegend("bottomright", pal = pal, values = sits$species,
                           title = "Species",
                           opacity = sightings_opacity)
    }else{
      m <-
        m %>%
        leaflet::addCircleMarkers(data=sits,
                                  lng = ~Lon, lat = ~Lat,
                                  radius=sightings_radius,
                                  opacity=sightings_opacity,
                                  color = sightings_color,
                                  popup= paste0('<b>Species ',sits$species,'</b><br>',
                                                sits$DateTime,'<br>',
                                                'Cruise ',sits$Cruise,'<br>',
                                                'School size: ',round(sits$best,2),'\n'))
    }
  }

  #=============================================================================
  # Output leaflet map

  print(m)

}
