#' Select strata polygons for standard survey regions
#'
#' The `LTabundR` package comes with several built-in datasets of geographic strata
#' that are commonly used in NOAA/NMFS surveys. The functions `strata_explore()`
#' and `strata_select()` were developed to help you explore those built-in options.
#' \cr \cr
#' Those built-in datasets come in the form of a list, in which each slot is a
#' separate geostratum. `strata_select()` allows you to "grab" the underlying data (i.e., the `data.frame`
#' of Lat/Long coordinates) for one of the geostrata displayed by `strata_explore()`.
#'
#' @param selections Numeric index of the geostratum you wish to "grab" from a
#' list of geostrata. This index is printed in the legend produced by `strata_explore()`.
#' @param region  Name of the survey region pertaining to your selection (the same region you passed to `strata_explore()`).
#' Options currently supported:
#' `"cnp"` (the default; Central North Pacific), `"ccs"` (California Current System),
#' and `"etp"` (Eastern Tropical Pacific).
#'
#' @return A `data.frame` of coordinates for the boundary of a geostratum. You can
#' pass this object to the `strata` argument for `load_settings()`.
#'
#' @export
#'
strata_select <- function(selections,
                           region){

  #=============================================================================
  # for debugging only -- not run!
  if(FALSE){
    selections = 3
    region <- 'ccs'
  }
  #=============================================================================

  # Bring in strata datasets based on the specified region
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

  # Select the specified strata from that dataset
  strata <- lf[selections] ; strata

  return(strata)
}
