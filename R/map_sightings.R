#' Add sightings to a survey map
#'
#' @param m Your `tmap` map object (produced from `map_base()` or any of the other `LTabundR` mapping functions).
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param include_species A character vector of species codes (see `data(species_codes)`), if you wish to only map sightings of certain species.
#' @param color_base  Default color to use.
#' @param color_code  Boolean; if `TRUE`, the underlying `tmap` functions will color-code sightings according to their built-in palettes.
#' @param shape_base  Default point shape to use.
#' @param shape_code  Boolean; if `TRUE`, the underlying `tmap` functions will shape-code sightings according to the `included` column.
#' @param size_base  Default point size to use.
#' @param size_code  Boolen; if `TRUE`, the underlying `tmap` functions will size-code sightings according to the `best` column.
#'
#' @return A `tmap` map with sightings shown as points (the map is built using the package `tmap`), which will print as a plot if not saved as an object.
#' If saved as an object, other features can be added to the map (see `map_effort()`, `map_strata()`,
#' or other `tmap` functions)
#'
#' @export
#'
map_sightings <- function(m,
                          cruz,
                          cohort=1,
                          include_species = NULL,
                          color_base = 'firebrick',
                          color_code = FALSE,
                          shape_base = 21,
                          shape_code = FALSE,
                          size_base = .5,
                          size_code = FALSE
                          ){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    m <- map_base()
    data(example_cruz)
    cruz <- example_cruz
    cohort = 1
    include_species = NULL
    color_base = 'firebrick'
    color_code = FALSE
    shape_base = 21
    shape_code = FALSE
    size_base = .5
    size_code = FALSE

    # test the function
    map_sightings(m,cruz)

    map_sightings(m,cruz,
                  include_species = '033',
                  color_base = 'firebrick',
                  shape_base = 18,
                  size_base = 1)

    map_sightings(m, cruz,
                  include_species = c('076','046'),
                  color_base = 'forestgreen',
                  color_code = TRUE,
                  shape_code = TRUE,
                  size_code = TRUE )
  }
  #=============================================================================

  # save a copy of the input map
  m1 <- m

  # Get the sightings from the cohort specified
  survey <- cruz$cohorts[[cohort]]
  names(survey)
  sits <- survey$sightings
  sits$species %>% table

  # Format legend columns
  sits$`School size` <- sits$best
  sits$`In Analysis` <- sits$included

  # Filter to species
  if(!is.null(include_species)){
    sits <- sits %>% dplyr::filter(species %in% include_species)
  }

  # Format species common names using the species translator
  # apply species_translator to each row
  spp_codes <-
    sapply(sits$species,function(x){
      y <- species_translator(x,
                              match_by_short_name = FALSE,
                              match_by_common = FALSE,
                              match_by_latin = FALSE)$common_name1 %>% as.character()
      if(length(y)==0){y <- " "}
      y[1]
      #length(y)
    })
  spp_codes <- unlist(spp_codes)
  sits$Species <- spp_codes

  # format species names for legend:
  sits$Species <- gsub('Unidentified','UNID', sits$Species)
  sits$Species <- gsub('identified','ID', sits$Species)
  sits$Species[sits$Species == ' '] <- sits$species[sits$Species == ' ']
  sits$Species %>% table

  # Format coordinates for map
  sitxy <- sits
  sp::coordinates(sitxy) <- ~ Lon + Lat
  sitm <- sf::st_as_sf(sitxy)

  # Determine color, shape, size, based on inputs
  tmcol <- ifelse(color_code, 'Species', color_base)
  tmshape <- ifelse(shape_code, 'In Analysis', shape_base)
  tmsize <- ifelse(size_code, 'School size', size_base)

  # Simply test map (debugging)
  #m1 + tmap::tm_shape(sitm) + tmap::tm_dots()

  # Add sightings to map
  m1 <- m1  +
    tmap::tm_shape(sitm) +
    tmap::tm_dots(col = tmcol,
                  palette = 'Dark2',
                  shape = tmshape,
                  border.lwd = .5,
                  alpha = .85,
                  size = tmsize)

  return(m1)
}
