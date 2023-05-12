#' Species name/code search engine
#'
#' Retrieve common and scientific names for a species based on their observer code,
#' or vice versa: retrieve the species code based on common or scientific name.
#'
#' @param id Your search query term; can be a character or numeric. The function will
#' return partial matches and is case insensitive.
#'
#' @param species_codes A `data.frame` of species codes, with the same column structure
#' as the `LTabundR` dataset `data(species_codes)`. If this is not provided, that
#' built-in dataset will be provided.
#'
#' @param match_by_code The remaining inputs are a way to toggle on/off certain
#' methods for searching for matches. This input will try to match your search query
#' to species codes, e.g., `"033"` for the false killer whale.
#'
#' @param match_by_short_name Try matching search query to "short name" species codes,
#'e.g., `"MESOP_PERU"` for the pygmy beaked whale.
#' @param match_by_latin Try matching search query to scientific name.
#' @param match_by_common Try matching search query to common name.
#'
#' @return A `data.frame` with all viable patches to your search query.
#' @export
#'
#' @examples
#' species_translator('046') # match by species code
#' species_translator('46') # partial matches work
#' species_translator(46) # numeric codes work
#' species_translator('RM_WHA') # partial short names work
#' species_translator('Physeter') # latin names work
#' species_translator('orca') # common names work + partial matches + case insensitive
#' species_translator('orca', match_by_latin=FALSE) # toggle match modes
#'
species_translator <- function(id,
                               codes=NULL,
                               match_by_code=TRUE,
                               match_by_short_name=TRUE,
                               match_by_latin=TRUE,
                               match_by_common=TRUE){

  #=============================================================================
  # for debugging only -- not run!
  if(FALSE){
    id <- 45
    codes <- NULL
    match_by_code=TRUE
    match_by_short_name=TRUE
    match_by_latin=TRUE
    match_by_common=TRUE

    species_translator('046') # match by species code
    species_translator('46') # partial matches work
    species_translator(46) # numeric codes work
    species_translator('RM_WHA') # partial short names work
    species_translator('Physeter')
    species_translator('orca') # partial matches / case insensitive
    species_translator('orca', match_by_latin=FALSE) # toggle match modes
  }
  #=============================================================================

  suppressWarnings({
    suppressMessages({

      # If species codes aren't provide, use built-in dataset
      if(is.null(codes)){
        data(species_codes)
        key <- species_codes
      }else{
        key <- codes
      }
      #print(head(key))

      # Look for potential matches
      matches <- c() # stage vector for potential matches
      if(match_by_code){
        matchi <- grep(as.character(id), as.character(key$code),fixed=TRUE)
        matches <- c(matches, matchi)
      }

      if(match_by_short_name){
        matchi <- grep(as.character(id), as.character(key$short_name),fixed=TRUE)
        matches <- c(matches, matchi)
      }

      if(match_by_latin){
        # Scientific name
        matchi <- grep(toupper(as.character(id)), toupper(key$scientific_name))
        matches <- c(matches, matchi)
      }

      if(match_by_common){
        # Common1
        matchi <- grep(toupper(as.character(id)), toupper(key$common_name1))
        matches <- c(matches, matchi)

        # Common2
        matchi <- grep(toupper(as.character(id)), toupper(key$common_name2))
        matches <- c(matches, matchi)

        # Common3
        matchi <- grep(toupper(as.character(id)), toupper(key$common_name3))
        matches <- c(matches, matchi)

        # Common4
        matchi <- grep(toupper(as.character(id)), toupper(key$common_name4))
        matches <- c(matches, matchi)
      }

      # Get unique matches
      matches <- unique(matches)
      matches
      #print(matches)

      # stage results dataframe
      search_result <- data.frame()
      if(length(matches)>0){

        # subset species_codes table to matches
        search_result <- key[matches,]

        # make sure none are NA
        search_result <- search_result[!is.na(search_result$common_name1),]
      }
    })
  })

  return(search_result)
}
