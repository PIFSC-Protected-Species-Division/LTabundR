#' Load settings for processing `WinCruz` surveys
#'
#' You need the output of this function to pass to `process_surveys()`,
#' the primary function for processing `WinCruz` data.
#'
#' @param strata A named list in which each slot is a `data.frame` of coordinates for a geostratum polygon.
#' Each `data.frame` must have `Lon` and `Lat` as the first two columns (in that order),
#' providing coordinates in decimal degrees in which West and South coordinates are negative.
#' Other columns are allowed, but the first two need to be `Lon` and `Lat`.
#' The name of the slot holding the `data.frame` will be used as a reference name for the stratum.
#' For an example of formatting, see `data(strata_cnp)`. If you are working with a standard NOAA survey region,
#' such as the Central North Pacific (CNP), Eastern Tropical Pacific (ETP), or California Current System (CCS),
#' you can use built-in polygons available in `data(strata_cnp)`, `data(strata_etp)`, or `data(strata_ccs)`, respectively.
#' To explore and/or select strata contained within those built-in datasets, use the functions
#' `strata_explore()` and `strata_select()`.
#'
#' @param survey Survey-wide settings, provided as the list that is generated with the
#' command `load_survey_settings()`. See the documentation for that function for full details.
#' if nothing is supplied, the default survey-wide settings (see `load_survey_settings()`)
#' will be applied.
#'
#' @param cohorts  Cohort-specific settings, provided as a named list.
#' Each slot contains the settings for a single cohort, which can be generated with the
#' command `load_cohort_settings()`. See the documentation for that function for full details.
#' Cohort-specific settings apply only to a group of species.
#' Since you can add as many cohorts to a `settings` object as you need,
#' this allows you to stage your entire analysis and run your code once,
#' without modifying code between the analysis of each cohort.
#' If nothing is supplied, the default cohort settings (see `load_cohort_settings()`)
#' will be applied to all species.
#'
#' @return A list with three named slots, equivalent to your three input arguments.
#' Save this output to an object, e.g., "`settings`", and pass it to `process_surveys()`.
#'
#' @export
#'
load_settings <- function(strata = NULL,
                          survey = NULL,
                          cohorts = NULL
                          ){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    data(strata_cnp)
    strata <- strata_cnp
    survey <- load_survey_settings()
    cohorts <- load_cohort_settings()
  }
  #=============================================================================

    if(is.null(survey)){
    # If not provided, use the package defaults
    survey <- load_survey_settings()
  }

  if(is.null(cohorts)){
    # If not provided, use the package defaults
    cohorts <- list(default = load_cohort_settings())
  }

  settings <- list(strata = strata,
                   survey = survey,
                   cohorts = cohorts)

  return(settings)
}

