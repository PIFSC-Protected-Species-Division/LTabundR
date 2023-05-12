#' Build a function that creates `estimates` sub-lists for `lta()`
#'
#' This function can be used to efficiently build up sub-lists for the `estimates` input
#' that you must pass to the `lta()` function for line-transect analysis.
#' Most studies of multiple species endeavor to estimate density/abundance for each species
#' in the same set of year-region scenarios.
#' Doing so typically requires multiple `estimates` sub-lists for each species of interest.
#' Writing those sub-lists manually can be redundant, tedious, and prone to inconsistencies or error.
#' To ensure the exact same scenarios are applied to each species of interest, you
#' can use this function to return a custom function that you can then use to creates a
#' standard set of sub-lists for each of you species.
#'
#' @param scenarios A list of sub-lists. Each sublist specifies one scenario for a density/abundance estimate.
#' The accepted names within each sublist are:
#' \itemize{
#' \item `years`
#' \item `cruises`
#' \item `regions`
#' \item `regions_remove`
#' \item `region_title`
#' \item `forced_effort`
#' \item `area`
#' \item `remove_land`
#' }
#' These arguments are explained under the `estimates` input for the `lta()` function.
#'
#' @return This function returns a custom function named `estimator()`, which accepts the following inputs:
#' \itemize{
#' \item `spp` (required)
#' \item `title` (required)
#' \item `g0` (the remainder are optional)
#' \item `g0_cv`
#' \item `g0_threshold`
#' \item `alt_g0_spp`
#' \item `combine_g0`
#' }
#' These arguments are explained under the `estimates` input for the `lta()` function.
#' The `estimator()` function will return a list of sub-lists, in which the
#' species-specific inputs it accepts are added to each sub-list in the `scenarios` argument you provided above.
#' The result of the `estimator()` function is what you can pass as the `estimates` argument in `lta()`.
#' See the [vignette case studies](https://emk-noaa.github.io/LTAvignette/whiceas.html#density-abundance) for examples.
#'
#' @export
#'
lta_estimates <- function(scenarios){

  if(FALSE){ # for debugging only -- not run! ==================================
    scenarios <- list(
      list(years = 2002,
           regions = 'MHI',
           regions_remove = NULL,
           region_title = NULL,
           forced_effort = NULL,
           area = NULL,
           remove_land = TRUE),
      list(years = 2002,
           regions = 'HI_EEZ',
           regions_remove = 'MHI'),
      list(years = 2010,
           regions = 'HI_EEZ'),
      list(years = 2017,
           regions = 'HI_EEZ'))

    # try it
    estimator <- lta_estimates(scenarios)
    estimator(spp = '013', title = 'Striped dolphin')
  } #  =========================================================================

  # Save input as a global variable
  scenarios <<- scenarios

  # Build function using this global variable
  estimator <- function(spp,
                        title,
                        alt_g0_spp = NULL,
                        combine_g0=NULL,
                        g0 = NULL,
                        g0_cv = NULL){

    if(FALSE){ # for debugging only -- run! ====================================
      spp <- 'spp'
      title <- 'title'
      alt_g0_spp = NULL
      combine_g0=NULL
      g0 = NULL
      g0_cv = NULL
    } # ========================================================================

    # Loop through each scenario
    i=1
    for(i in 1:length(scenarios)){
      (sceni <- scenarios[[i]])

      # Add the inputs to this scenario
      sceni$spp <- spp
      sceni$title <- title
      sceni$alt_g0_spp <- alt_g0_spp
      sceni$g0 <- g0
      sceni$g0_cv <- g0_cv
      sceni$combine_g0 <- combine_g0

      # Update scenario list
      scenarios[[i]] <- sceni
    }
    scenarios
    return(scenarios)
  }

  # Return this function
  return(estimator)
}
