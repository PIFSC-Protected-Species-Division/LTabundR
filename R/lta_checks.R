#' Check for missing data before running `lta()`.
#'
#' This function is designed for a QA/QC step before running the `lta()` function
#' (line transect analysis). It checks for missing data in covariate columns,
#' group size columns (if `lnsstot` is a desired covariate), and the perpendicular distance column (`PerpDistKm`).
#' The function runs these checks twice: first for all sightings to be used in fitting the detection function
#' (where missing data are not preferred but would not be fatal; sightings with incomplete data would simply be removed,
#' lowering your sample size for the model fit), and second for detections that contribute to each point estimate
#' (here missing data is disastrous; unless the missing data are filled in, the detection would be removed and the density estimate would be affected.)
#' This function can be used to pinpoint and address holes in the data as the user sees fit.
#'
#' @param cruz The same `cruz` object you will pass to `lta()`. See input details in that function's documentation.
#' @param df_settings The same `df_settings` object you will pass to `lta()`. See input details in that function's documentation.
#' @param fit_filters The same `fit_filters` object you will pass to `lta()`. See input details in that function's documentation.
#' @param estimates The same `estimates` object you will pass to `lta()`. See input details in that function's documentation.
#'
#' @return The function prints messages to the Console as it performs its checks,
#' and it also returns a `list` with all details needed to find & edit rows in the sightings data that have missing data.
#'
#' @export
#'
lta_checks <- function(cruz,
                       df_settings,
                       fit_filters,
                       estimates){

  #=============================================================================
  if(FALSE){
    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020

    # Detection function specifications
    fit_filters <-
      list(spp = c('013', '026', '031'),
           pool = 'Multi-species pool 1',
           cohort = 'all',
           truncation_distance = 5,
           other_species = 'remove')

    df_settings <-
      list(covariates = c('bft','lnsstot','cruise','year','ship','species'),
           covariates_factor = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
           covariates_levels = 2,
           covariates_n_per_level = 10,
           simplify_cue = TRUE,
           simplify_bino = TRUE,
           detection_function_base = 'hn',
           base_model = '~1',
           delta_aic = 2)

    # Density / abundance estimation plan
    scenarios <- list(list(years = 2017,
                           regions = 'MHI'),
                      list(years = 2020,
                           regions = 'MHI'))
    estimator <- lta_estimates(scenarios)
    estimates <-
      c(estimator(spp = '013', title = "Striped dolphin",
                  g0=.3, g0_cv=.2),
        estimator(spp = '026', title = "Fraser's dolphin", alt_g0_spp = '013',
                  g0=.3, g0_cv=.2),
        estimator(spp = '031', title = "Melon-headed whale", alt_g0_spp = '013',
                  g0=.3, g0_cv=.2))

    checks <- lta_checks(cruz, df_settings, fit_filters, estimates)
    checks

  }
  #=============================================================================
  ##############################################################################

  # Setup results object
  checks <- list(sits = data.frame(),
                 errors = c(),
                 df = list(covariates = list(),
                           group_size = c(),
                           perp_dist = c()),
                 estimates = list())

  # Get sightings and add id columnb
  (sits <- cruz$cohorts[[fit_filters$cohort]]$sightings)
  sits$check_id <- 1:nrow(sits)

  ##############################################################################
  # Error checks

  # Fit filters
  if(!is.list(fit_filters)){
    checks$errors <- c(checks$errors,
                       '`fit_filters` is not provided as a list.')
  }
  if(is.null(fit_filters$spp)){
    checks$errors <- c(checks$errors,
                       '`fit_filters$spp` is currently NULL. Provide at least one species code.')
  }
  if(is.null(fit_filters$truncation_distance)){
    checks$errors <- c(checks$errors,
                       '`fit_filters$truncation_distance` is currently NULL. Provide a numeric truncation distance.')
  }
  cohort <- fit_filters$cohort
  if(!is.null(cohort) & ! cohort %in% names(cruz$cohorts)){
    checks$errors <- c(checks$errors,'The cohort you specified in `fit_filters$cohort` does not exist in the `cruz` object. Enter an exsiting name for a cohort, or a number between 1 and length(cruz$cohorts).')
  }

  # Detection function defaults
  if(!is.list(df_settings)){
    checks$errors <- c(checks$errors,'`df_settings` is not provided as a list.')
  }
  (covariates <- df_settings$covariates)
  if(!is.null(covariates)){covariates <- tolower(covariates)}
  (covariates_factor <- df_settings$covariates_factor)
  if(length(covariates) != length(covariates_factor)){
    checks$errors <- c(checks$errors,'`df_settings$covariates` is not the same length as `df_settings$covariates_factor.')
  }

  checks

  ##############################################################################
  # Filter to species within truncation distance

  sits <- sits %>%
    dplyr::filter(species %in% fit_filters$spp) %>%
    dplyr::filter(PerpDistKm <= fit_filters$truncation_distance)
  if(nrow(sits)>0){
    checks$sits <- sits
  }else{
    checks$errors <- c(checks$errors,'No sightings match species / truncation distance settings in `fit_filters`')
  }

  ##############################################################################
  # Print errors

  message('Checking for formatting errors in input objects...')
  if(length(checks$errors)>0){
    for(i in 1:length(checks$errors)){
      message('--- ERROR ',i,' :: ', checks$errors[i])
    }
  }else{
    message('--- None detected.')
  }
  message('')

  ##############################################################################
  # Helper functions

  # check covariates ===========================================================
  covariate_checker <- function(covariates, covariates_factor, sits){
    covariates
    covariates_factor
    covariates_status <- list()
    i=1
    for(i in 1:length(covariates)){
      (covi <- covariates[i])
      (covif <- covariates_factor[i] %>% as.logical)
      # look for matching column in sits
      (matchi <- which(tolower(names(sits)) == covi))
      if(length(matchi) > 0){
        # get values
        covi_values <- sits %>% dplyr::pull(matchi)
        # coerce to numeric if instructed
        if(! covif){covi_values <- covi_values %>% as.numeric}
        covi_values
        # get problem values
        problems <- which(is.na(covi_values))
        # save to list
        if(length(problems)>0){
          covariates_status[[i]] <- sits[problems, ]
          message('--- --- covariate = "', covi, '" :: NAs found on row(s) ', paste(problems, collapse=', '), ' ********')
        }else{
          covariates_status[[i]] <- c()
          message('--- --- covariate = "', covi, '" :: no missing data.')
        }
      }else{
        messi <- 'Covariate name does not match any columns in sightings dataset!'
        covariates_status[[i]] <- messi
        message('--- --- covariate = "', covi, '" :: ', messi)
      }
    }
    return(covariates_status)
  }

  # Group size checks
  group_size_checker <- function(sits){
    invalids <- sits %>% dplyr::filter(ss_valid == FALSE)
    if(nrow(invalids)==0){
      invalids <- data.frame()
      message('--- --- all group size estimates are valid.')
    }else{
      message('--- --- invalid group size estimates found on ', nrow(invalids),' row(s): ', paste(invalids$check_id, collapse=', '),' **********')
    }
    return(invalids)
  }

  # PerpDistKM checker
  perp_dist_checker <- function(sits){
    invalids <- sits %>% dplyr::filter(is.na(as.numeric(PerpDistKm)) == TRUE |
                                         as.numeric(PerpDistKm) < 0)
    if(nrow(invalids)==0){
      invalids <- data.frame()
      message('--- --- all perpendicular distances are valid.')
    }else{
      message('--- --- invalid perpendicular distances found on ', nrow(invalids),' row(s): ', paste(invalids$check_id, collapse=', '),' **********')
    }
    return(invalids)
  }

  ##############################################################################
  # Check detection function sightings

  message('=============================================================')
  message('=============================================================')
  message('')
  message('Checking historical sightings used for the detection function ...')
  message('\n--- checking for missing data in covariate columns ...')
  checki <- covariate_checker(covariates, covariates_factor, sits)
  checks$df$covariates <- checki
  #checks

  if('lnsstot' %in% covariates){
    message('\n--- checking for group size estimates flagged as invalid ...')
    checki <- group_size_checker(sits)
    checks$df$group_size <- checki
    #checks
  }

  message('\n--- checking for missing data in PerpDistKm column ...')
  checki <- perp_dist_checker(sits)
  checks$df$perp_dist <- checki
  #checks

  ##############################################################################
  # Loop through all the estimates

  message('')
  message('=============================================================')
  message('=============================================================')
  message('')
  message('Checking each point-estimate scenario...\n')

  estimate_checks <- list()
  estimates
  i=1
  for(i in 1:length(estimates)){
    (estimati <- estimates[[i]])
    message(i,' :: ', estimati$title, ' (', paste(estimati$spp, collapse=', '), ') :: ',
            paste(estimati$regions, collapse=', '), ' :: ',
            paste(estimati$years, collapse=', '),
            ' ================')

    estimate_checks[[i]] <- list(covariates = list(),
                                 group_size = data.frame(),
                                 perp_dist = data.frame())

    # Filter sits
    siti <- sits %>% dplyr::filter(species %in% estimati$spp,
                                   year %in% estimati$years,
                                   stratum %in% estimati$regions,
                                   OnEffort == TRUE,
                                   included == TRUE)

    if(nrow(siti) == 0){
      message('Zero sightings for this estimate scenario. Moving on...\n')

    }else{

      message('\n--- checking for missing data in covariate columns ...')
      checki <- covariate_checker(covariates, covariates_factor, siti)
      estimate_checks[[i]]$covariates <- checki
      #checks

      if('lnsstot' %in% covariates){
        message('\n--- checking for group size estimates flagged as invalid ...')
        checki <- group_size_checker(siti)
        estimate_checks[[i]]$group_size <- checki
        #checks
      }

      message('\n--- checking for missing data in PerpDistKm column ...')
      checki <- perp_dist_checker(siti)
      estimate_checks[[i]]$perp_dist <- checki
      message('')
    }
  }

  # Add to checks object
  checks$estimates <- estimate_checks

  message('')
  message('To edit the sightings dataframe based on these checks, use the following workflow:')
  message('\n# save sightings in an object')
  message('sits <- cruz$cohorts$<cohort name here>$sighitings')
  message('')
  message('# (perform edits on sits based on info within the lta_checks() output ...)')
  message('')
  message('# update the cruz object with revised sightings')
  message('cruz$cohorts$<cohort name here>$sightings <- sits')

  ##############################################################################
  return(checks)
}
