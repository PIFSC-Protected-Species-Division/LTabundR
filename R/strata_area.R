#' Combine geostrata and compute area
#'
#' This function allows for complex combinations / subtractions of geostratum polygons
#' and calculates the resulting area before and after the removal of dry land.
#' Diagnostic plots are provided that ensure the result is correct.
#'
#' @param strata_all A named `list` in which each slot is a `data.frame` of
#' coordinates for a geostratum polygon. Each `data.frame` must have `Lat` and `Lon`
#' as the first two columns, providing coordinates in decimal degrees in which
#' South and West coordinates are negative. Other columns are allowed,
#' but the first two need to be `Lon` and `Lat`. The name of the slot
#' holding the `data.frame` will be used as a reference name for the stratum.
#' For an example of formatting, see `data(strata_cnp)`.
#' If you are working with a standard NOAA survey region, such as the Central North Pacific (CNP),
#' Eastern Tropical Pacific (ETP), or California Current System (CCS),
#' you can use built-in polygons available in `data(strata_cnp)`, `data(strata_etp)`, or
#' `data(strata_ccs)`, respectively. To explore and/or select strata contained
#' within those built-in datasets, use the functions `strata_explore()` and `strata_select()`.
#'
#' @param strata_keep A character vector of the names of the geostrata
#' within `strata_all` that you want to use
#'
#' @param strata_remove A character vector of the names of the
#' geostrata you want to remove. For example,
#' perhaps you want to keep the `HI_EEZ` stratum but remove insular stock boundaries.
#'
#' @param remove_land A Boolean, with default `TRUE`, indicating whether or not you want to
#' remove land from the resulting survey area.
#'
#' @param toplot If `TRUE` (the default), diagnostic maps will be plotted for your review.
#'
#' @param verbose If `TRUE` (the default), status updates will be printed to the Console.
#'
#' @return A list with the following slots:
#' \enumerate{
#' \item `km2` The area of the resulting survey area polygon, in square km.
#' \item `km2_keep` The area of the `strata_keep` polygons, after combining them to account
#' for any overlapping portions of the polygons, but before removing land.
#' \item `km2_remove` The area of the `strata_remove` polygons, if any were provided,
#' after combining them to account for any overlapping portions of the polygons,
#' but before removing land.
#' \item `km2_with_land` The area of the resulting survey area polygon *before* removing land.
#' \item `sf` The `sf` object of the resulting polygon.
#' }
#' Note that if `toplot` is `TRUE` (the default), a diagnostic plot will also be produced.
#'
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @import ggbreak
#'
#' @export
#'
strata_area <- function(strata_all,
                        strata_keep,
                        strata_remove = NULL,
                        remove_land = TRUE,
                        toplot = TRUE,
                        verbose = TRUE){

  if(FALSE){ # debugging -- not run ============================================

    data(strata_cnp)
    strata_all <- strata_cnp
    strata_keep <- c('HI_EEZ')
    strata_remove <- c('WHICEAS')
    #strata_keep <- c('NWHI', 'MHI', 'WHICEAS')
    #strata_remove <- c('Bottlenose_OUFI', 'Bottlenose_KaNi', 'Bottlenose_BI')
    remove_land <- TRUE
    toplot <- TRUE
    verbose <- TRUE

    # Try it =============================

    strata_area(strata_all = strata_cnp,
                strata_keep = c('NWHI', 'MHI', 'WHICEAS'),
                strata_remove = c('Bottlenose_OUFI', 'Bottlenose_KaNi', 'Bottlenose_BI'))

    strata_area(strata_all = strata_cnp,
                strata_keep = c('HI_EEZ'),
                strata_remove = c('NWHI', 'WHICEAS'))

    # problem
    strata_area(strata_all = strata_cnp,
                strata_keep = c('HI_EEZ', 'WHICEAS'),
                strata_remove = c('Spotted_OU','Spotted_FI','Spotted_BI'))
    #strata_all = strata_cnp
    #strata_keep = c('HI_EEZ', 'WHICEAS')
    #strata_remove = c('Spotted_OU','Spotted_FI','Spotted_BI')

    strata_area(strata_all = strata_cnp,
                strata_keep = c('Bottlenose_KaNi','Bottlenose_OUFI','Bottlenose_BI'))

    strata_area(strata_all = strata_cnp,
                strata_keep = c('MHI','WHICEAS'),
                strata_remove = c('Spotted_OU','Spotted_FI','Spotted_BI',
                                  'Bottlenose_OUFI', 'Bottlenose_KaNi', 'Bottlenose_BI'))

    strata_area(strata_all = strata_cnp,
                strata_keep = c('MHI','WHICEAS'))

    strata_area(strata_all = strata_cnp,
                strata_keep = c('MHI','WHICEAS'),
                remove_land = FALSE)

    strata_area(strata_all = strata_cnp,
                strata_keep = c('MHI','WHICEAS'),
                strata_remove = c('Spotted_OU','Spotted_FI','Spotted_BI'),
                remove_land = FALSE)

  } # end not run ============================================================

  ##############################################################################
  # (1) Process all strata to keep

  if(verbose){ message('\nProcessing geostrata to keep ...') }

  # stage results lists
  keeps <- list()
  keep_polis <- list()
  keep_lat <- list()
  keep_lon <- list()
  keep_idl <- c()

  if(toplot){ gg_keeps <- ggplot() }

  # Loop through each strata to keep
  i=1
  for(i in 1:length(strata_keep)){
    (strati_name <- strata_keep[i])
    if(verbose){ message('--- ',strati_name) }
    (strati <- strata_all[names(strata_all) == strati_name][[1]])

    # Process polygon as sf object
    #polygon_dataframe <- strati
    suppressMessages({ suppressWarnings({ poli <- process_polygon(strati) }) })
    #ggplot(poli$sf) + geom_sf()

    keeps[[length(keeps)+1]] <- poli
    names(keeps)[length(keeps)] <- strati_name

    keep_polis[[length(keep_polis)+1]] <- st_make_valid(poli$sf)
    names(keep_polis)[length(keep_polis)] <- strati_name

    keep_lat[[length(keep_lat)+1]] <- poli$lat_range
    keep_lon[[length(keep_lon)+1]] <- poli$lon_range

    keep_idl[i] <- poli$idl

    if(toplot){
      gg_keeps <- gg_keeps + geom_sf(data=poli$sf, alpha=.3, fill='darkblue')
    }
  }

  # review
  keeps %>% names
  keep_polis %>% names
  keep_idl
  keep_lat
  keep_lon

  # Determine if any polygons to keep cross the IDL
  (idl <- any(keep_idl))

  # Get overall latitude range
  (lat_mins <- lapply(keep_lat, '[[', 1) %>% unlist)
  (lat_maxs <- lapply(keep_lat, '[[', 2) %>% unlist)
  (lat_range <- c(min(lat_mins), max(lat_maxs)))

  # Get overall longitude range
  (lon_mins <- lapply(keep_lon, '[[', 1) %>% unlist)
  (lon_maxs <- lapply(keep_lon, '[[', 2) %>% unlist)
  # Handle IDL for lon_range
  if(idl){
    (lonmin <- lon_mins[lon_mins > 0] %>% max)
    (lonmax <- lon_maxs[lon_maxs < 0] %>% max)
  }else{
    (lonmin <- min(lon_mins))
    (lonmax <- max(lon_maxs))
  }
  (lon_range <- c(lonmin, lonmax))

  #=============================================================================

  if(verbose){ message('---\n--- combining these strata into one geometry ...') }
  keeps_combined <- purrr::accumulate(keep_polis, st_union)
  keeps_combined <- keeps_combined[[length(keeps_combined)]]
  keeps_combined %>% class

  if(toplot){
    gg_keeps_combined <-
      ggplot() +
      geom_sf(data=keeps_combined, alpha = .3, fill = 'darkblue')
  }

  keeps_combined_geo <- st_geometry(keeps_combined)
  keeps_combined_geo <- st_set_crs(keeps_combined_geo, 'EPSG:4326')

  keeps_area_w_land <- as.numeric(st_area(keeps_combined_geo)) / (1000*1000)
  if(verbose){
    message('--- combined area of these polygons, accounting for any overlap = ',
            format(round(keeps_area_w_land), big.mark=','), ' km2')
  }


  ##############################################################################
  # (2) Process all strata to remove

  if(toplot){
    gg_removes <- ggplot()
    gg_removes_caption <- 'No strata will be removed.'
    gg_removes_combined <- ggplot()
    gg_removes_combined_caption <- 'No strata will be removed.'
    gg_plan <-
      ggplot() +
      geom_sf(data=keeps_combined, alpha = .3, fill = 'darkblue')
    gg_plan_caption <- 'Strata to keep (blue) shown\nwith strata to remove (red)'
    gg_survey <- gg_plan
    gg_survey_caption <- 'Survey area after\nremoving strata_remove'
  }

  survey_geo <- keeps_combined_geo
  removes_area_w_land <- 0

  if(!is.null(strata_remove)){

    if(verbose){ message('\nProcessing geostrata to remove ...') }
    removes <- list()
    removes_polis <- list()
    if(toplot){ gg_removes <- ggplot() }
    i=1
    for(i in 1:length(strata_remove)){
      (strati_name <- strata_remove[i])
      if(verbose){ message('--- ',strati_name) }
      (strati <- strata_all[names(strata_all) == strati_name][[1]])

      suppressMessages({ suppressWarnings({ poli <- process_polygon(strati) }) })

      removes[[length(removes)+1]] <- poli
      names(removes)[length(removes)] <- strati_name

      removes_polis[[length(removes_polis)+1]] <- st_make_valid(poli$sf)
      names(removes_polis)[length(removes_polis)] <- strati_name

      if(toplot){
        gg_removes <- gg_removes + geom_sf(data=poli$sf, alpha=.3, fill='firebrick')
        gg_removes_caption <- 'Individual geostrata to remove'
      }
    }
    # review
    removes %>% names
    removes_polis %>% names
    #gg_removes

    if(verbose){ message('---\n--- combining these strata into one geometry ...') }
    removes_combined <- purrr::accumulate(removes_polis, st_union)
    removes_combined <- removes_combined[[length(removes_combined)]]

    if(toplot){
      gg_removes_combined <-
        ggplot() +
        geom_sf(data=removes_combined, alpha = .3, fill = 'firebrick')

      gg_removes_combined_caption <- 'Geostrata to remove,\nafter combining'
    }

    removes_combined_geo <- st_geometry(removes_combined)
    removes_combined_geo <- st_set_crs(removes_combined_geo, 'EPSG:4326')

    removes_area_w_land <- as.numeric(st_area(removes_combined_geo)) / (1000*1000)
    if(verbose){
      message('--- combined area of these polygons, accounting for any overlap = ',
              format(round(removes_area_w_land), big.mark=','), ' km2')
    }

    ##############################################################################
    # Remove these removes_polygons from the keeps_polygons

    # Plan
    if(toplot){
      gg_plan <-
        ggplot() +
        geom_sf(data=keeps_combined, alpha = .3, fill = 'darkblue') +
        geom_sf(data=removes_combined, alpha = .3, fill = 'firebrick')
    }

    if(verbose){ message('\nRemoving strata-to-remove from strata-to-keep ...') }
    survey_poli <- purrr::accumulate(list(keep = keeps_combined,
                                          remove = removes_combined),
                                     st_sym_difference)
    survey_poli <- survey_poli[[length(survey_poli)]]

    if(toplot){
      gg_survey <- ggplot() + geom_sf(data=survey_poli, fill = 'darkblue', alpha = .3)
      #gg_survey
    }

    survey_geo <- st_geometry(survey_poli)
    survey_geo <- st_set_crs(survey_geo, 'EPSG:4326')
  }

  (survey_area_w_land <- as.numeric(st_area(survey_geo)) / (1000*1000))
  if(verbose){
    message('--- study area remaining before land removal = ',
            format(round(survey_area_w_land), big.mark=','), ' km2')
  }

  ##############################################################################
  # Find land

  if(verbose){ message('\nFinding land, if any, in the study area ...') }
  suppressMessages({suppressWarnings({
    #sf::sf_use_s2(FALSE)
    data(land, package='LTabundR')
    sf::st_crs(survey_geo) <- sf::st_crs(land)
    landcrop <- st_crop(land, st_bbox(survey_geo))
  })})

  if(toplot){
    gg_land <- ggplot() +
      geom_sf(data = landcrop, fill='forestgreen') +
      geom_sf(data = survey_geo, fill = 'darkblue', alpha = .3)
    gg_land_caption <- 'Survey area (blue) with land (green)'
    #gg_land
  }

  if(toplot){
    gg_final <- gg_land
    gg_final_caption <- 'Final survey area.'
  }
  final_geo <- survey_geo

  ##############################################################################
  # Remove land

  if(remove_land){
    if(verbose){ message('\nRemoving land, if any, from study area ...') }
    suppressMessages({suppressWarnings({
      st_erase = function(x, y) st_difference(x, st_union(y))
      final_poli <- st_erase(survey_geo, landcrop)

      if(toplot){
        gg_final <-
          ggplot() +
          geom_sf(data=final_poli, fill='darkblue', alpha=.3)
        #gg_final
      }

      final_geo <- st_geometry(final_poli)
      final_geo <- st_set_crs(final_geo, 'EPSG:4326')

    }) }) }

  (final_area <- as.numeric(st_area(final_geo)) / (1000*1000))
  if(verbose){
    message('--- final study area = ',
            format(round(final_area), big.mark=','), ' km2')
  }

  ##############################################################################
  # Handle IDL

  if(idl & toplot){
    gg_keeps <-
      ggplot_idl(gg_keeps, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    gg_keeps_combined <-
      ggplot_idl(gg_keeps_combined, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    gg_removes <-
      ggplot_idl(gg_removes, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    gg_removes_combined <-
      ggplot_idl(gg_removes_combined, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    gg_plan <-
      ggplot_idl(gg_plan, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    gg_survey <-
      ggplot_idl(gg_survey, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    if(!is.null(gg_land)){
      gg_land <-
        ggplot_idl(gg_land, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
    }
    gg_final <-
      ggplot_idl(gg_final, lat_range = lat_range, lon_range = lon_range, axes=FALSE)
  }

  ##############################################################################
  # Add captions

  if(toplot){
    gg_keeps <- gg_keeps + labs(caption = 'Individual geostrata to keep')
    gg_keeps_combined <- gg_keeps_combined + labs(caption = 'Geostrata to keep,\nafter combining')
    gg_removes <- gg_removes + labs(caption = gg_removes_caption)
    gg_removes_combined <- gg_removes_combined + labs(caption = gg_removes_combined_caption)
    gg_plan <- gg_plan + labs(caption = gg_plan_caption)
    gg_survey <- gg_survey + labs(caption = gg_survey_caption)
    if(!is.null(gg_land)){
      gg_land <- gg_land + labs(caption = gg_land_caption)
    }
    gg_final <- gg_final + labs(caption = gg_final_caption)
  }

  ##############################################################################
  # Compile results

  if(toplot){

    if(!is.null(strata_remove)){
      if(remove_land){
        final_plot <- ggpubr::ggarrange(gg_keeps, gg_keeps_combined,
                                        gg_removes, gg_removes_combined,
                                        gg_plan, gg_survey,
                                        gg_land, gg_final,
                                        nrow = 4, ncol = 2)
      }else{
        final_plot <- ggpubr::ggarrange(gg_keeps, gg_keeps_combined,
                                        gg_removes, gg_removes_combined,
                                        gg_plan, gg_survey,
                                        gg_final,
                                        nrow = 4, ncol = 2)
      }
    }else{
      if(remove_land){
        final_plot <- ggpubr::ggarrange(gg_keeps, gg_keeps_combined,
                                        gg_land, gg_final,
                                        nrow = 2, ncol = 2)
      }else{
        final_plot <- ggpubr::ggarrange(gg_keeps, gg_keeps_combined,
                                        gg_final,
                                        nrow = 2, ncol = 2)
      }
    }
    print(final_plot)
  }

  # Prepare output
  results <- list(km2 = final_area,
                  km2_keep = keeps_area_w_land,
                  km2_remove = removes_area_w_land,
                  km2_with_land = survey_area_w_land,
                  sf = final_geo)

  return(results)
}
