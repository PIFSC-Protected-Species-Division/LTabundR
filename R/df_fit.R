#' Fit a detection function to `WinCruz` data
#'
#' This function is not typically called by the user (though it can be);
#' instead it is called as a subroutine from within the function `lta()`.
#' This function is a wrapper for the detection function fitting routine in the package
#' `mrds` (function `mrds::ddf()`). As a wrapper, this function conducts
#' automated stepwise model-fitting, trying all candidate function keys and covariates,
#' then selecting the best model(s) based upon `AICc`.
#'
#' @param sightings Sightings dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$sightings`),
#' already filtered to contain the sightings you wish to use to fit the detection function.
#'
#' @param truncation_distance The truncation distance to apply to sightings before model fitting.
#'
#' @param covariates Covariates you wish to include as candidates in detection function models,
#' provided as a character vector. The covariates must match columns existing within `sightings`.
#' Common covariates that you will find within `sightings` include `c('Bft','LnSsTot','Cruise','Year','Ship','species')`.
#' Note that the function will ignore case, coercing all covariates to lowercase.
#'
#' @param detection_function_base The base key for the deteion function, provided as a character vector.
#' Accepted values are `"hn"` (half-normal key, the default, which exhibit greater stability
#' when fitting to cetacean survey data; Gerrogette and Forcada 2005), `"hr"` (hazard-rate),
#' or `c("hn", "hr)`, which will loop through both keys and attempt model fitting.
#'
#' @param base_model The initial model formula, upon which to build using candidate covariates,
#' provided as a character vector. The default is `"~ 1"`.
#'
#' @param delta_aic The AIC difference between the model yielding the lowest AICc and
#' other candidate models, used to define the best-fitting models. Typically,
#' AICc differences of less than 2 indicate effectively equal model performance.
#'
#' @param toplot Boolean, with default `TRUE`, indiciating whether detection function plots (`Distance::plot.ds()`)
#' should be displayed as the candidate models are tested.
#'
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @details Model fitting is done in a forward stepwise procedure, starting fresh with each base key
#' provided. In the first round, the base model (no covariates) is fit first.
#' In the second round, each covariate is added one at a time; the covariate, if any, that produces the lowest AICc
#' below the AIC from the previous round is added to the formula.
#' This process is repeated in subsequent rounds until the AICc no longer improves.
#' All models within `delta_aic` of the model with the lowest AICc qualify as best-fitting models.
#'
#' @return A named list:
#' \enumerate{
#' \item `best_models`: A `data.frame` summary of the best-fitting models,
#' based upon the table produced by `Distance::summarize_ds_models()`.
#' See that function's documentation for details.
#' \item `all_models`:  Similar to the preceding slot, a tabular summary of *all* models tested.
#' \item `best_objects`:  A list containing the `ds` objects (produced by package `Distance`)
#' for each of the best-fitting models.
#' \item `sample_size`: A `data.frame` with the detections for each species within the species pool
#' used to fit the detection function.
#' `Ntot` is total detections for each species; `Ndet` is total detections within the truncation distance
#' and therefore used in the detection function fitting routine; `TD` is the truncation distance.
#' \item `tables`: A list of the data tables passed to `Distance::ds()` during model fitting.
#' \item `sightings`: The `sightings` `data.frame` provided, now with an `esw` columns provided estimated Effective Strip half-Width for each sighting.
#' }
#'
#' @export
#'
df_fit <- function(sightings,
                   truncation_distance = Inf,
                   covariates = NULL,
                   detection_function_base = 'hn',
                   base_model = '~1',
                   delta_aic = 2,
                   toplot=TRUE,
                   verbose=TRUE
){

  if(FALSE){ # debugging only -- not run! ======================================
    covariates = NULL
    detection_function_base = 'hn'
    base_model = '~1'
    delta_aic = 2
    toplot=TRUE
    verbose=TRUE
    truncation_distance = 5

    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    strata <- cruz$strata
    sightings <- cruz$cohorts$all$sightings
    sightings <- sightings[1:300,]
  } # end debugging ============================================================

  # stage results
  return_list <- NULL

  # review inputs
  sightings %>% head
  covariates

  if(nrow(sightings)>0){

    # Format table with sightings
    sightings <-
        sightings %>%
        dplyr::mutate(distance = PerpDistKm,
                      object = 1:dplyr::n(),
                      observer = 1,
                      detected = 1)
    data_table <- sightings
    names(data_table) <- tolower(names(data_table))
    head(data_table)

    ############################################################################
    ############################################################################
    if(verbose){message('Step-wise model fitting of detection function...')}

    # Stage results
    ds_summ <- data.frame() # summary table of fit results
    ds_objects <- list() # list of mrds:ddf results
    dso <- NULL # this will be the object name for the df object in each iteration

    # Inititate global variable for base_model
    base_model_global <<- base_model

    # Loop through each function key (hr or hn)
    keyi <- 1 # debugging
    for(keyi in 1:length(detection_function_base)){
      df_key <- detection_function_base[keyi]

      # Fit base model =========================================================

      if(verbose){message('\nKey: ',df_key,' :: Fitting base model ...')}
      if(df_key == 'hn'){
        dso <- mrds::ddf(dsmodel= ~mcds(key='hn', formula=base_model_global),
                         data=data_table, method="ds", meta.data=list(width=truncation_distance))
      }else{
        dso <- mrds::ddf(dsmodel= ~mcds(key='hr', formula=base_model_global),
                         data=data_table, method="ds", meta.data=list(width=truncation_distance))
      }
      if(toplot){plot(dso, main='Base model')}
      ds_objects[[length(ds_objects)+1]] <- dso

      (dso_aic <- MuMIn::AICc(dso) ) # dso$criterion)
      (dso_p_mean <- length(dso$ds$aux$ddfobj$xmat$distance) / dso$Nhat)
      if(verbose){message('     AIC = ',round(dso_aic,3), ' | Pmean = ',round(dso_p_mean,3))}
      (summi <- data.frame(Model = length(ds_objects),
                           Key_function=df_key,
                           Formula='~ 1',
                           Pmean = dso_p_mean,
                           AIC = round(dso_aic, 3)))
      ds_summ <- rbind(ds_summ, summi)

      # Step-wise model fitting ================================================

      formulas <- base_model # initiate some variables to track best AIC model
      best_num <- 1
      best_aic <- summi$AIC
      newterm <- TRUE
      stepi <- i <- 1 # debugging
      dso_pre <- dso

      if(!is.null(covariates)){
        (covar_names <- covariates)
        (n_covars <- covariates %>% length)

        for(stepi in 1:n_covars){ # Add one term at a time
          if(newterm){ # only continue if the model was improved in the previous step
            newterm <- FALSE # assume it will not be improved in this new step

            best_formula <- formulas[best_num]
            formulas <- c() # reset formulas

            i=3
            for(i in 1:n_covars){ # step through each possible covariate
              (cvi <- covar_names[i] %>% tolower) # get this covariate's name

              if(length(grep(cvi,best_formula))<=0){ # only continue with this covariate if the covariate is not yet in the formula

                (new_formula <- paste0(best_formula,' + ',cvi)) # build new formula
                formulas <- c(formulas, new_formula) # add to log of formulas tried

                if(verbose){message('Key: ',df_key,' :: Step ',stepi,' :: Covariate ',i,' :: Formula ::  ', new_formula)}

                # Build real formula
                covar_is_factor <- FALSE # force false for time being
                if(covar_is_factor){
                  matchi <- grep(paste0(cvi,'_'), names(data_table))
                  new_terms <- paste(names(data_table)[matchi],collapse=' + ')
                  (real_formula <- paste0(best_formula,' + ',new_terms))
                }else{
                  real_formula <- new_formula
                }
                # Save formula as global variable
                real_formula <<- as.formula(real_formula)

                # Estimate detection function
                if(exists('dso')){rm(dso)}
                dso <- NULL # reset model object

                # Fit detection function
                try({
                  if(df_key == 'hn'){
                    dso <- mrds::ddf(dsmodel= ~mcds(key='hn', formula = real_formula),
                                     data=data_table, method="ds", meta.data=list(width=truncation_distance), control=list(debug=TRUE))
                  }else{
                    dso <- mrds::ddf(dsmodel= ~mcds(key='hr', formula = real_formula),
                                     data=data_table, method="ds", meta.data=list(width=truncation_distance), control=list(debug=TRUE))
                  }
                })

                # Make sure fit worked
                if(!is.null(dso)){ # if the model fit worked, save summary & compare to previous best
                  if('ds' %in% class(dso) & 'ddf' %in% class(dso)){
                    sum_success <- FALSE
                    try({ # Attempt getting summary info for this ddf model
                      summary(dso) ; sum_success <- TRUE
                    }) # end of try to summarize dso model
                    if(sum_success){# if it works, that means the ddf may be viable

                      # Save results
                      if(toplot){plot(dso, main=new_formula)}
                      ds_objects[[length(ds_objects)+1]] <- dso
                      if(exists('dso_aic')){rm(dso_aic)}
                      if(exists('dso_p_mean')){rm(dso_p_mean)}
                      dso_aic <- dso_p_mean <- NULL
                      (dso_aic <- MuMIn::AICc(dso) ) # dso$criterion)
                      (dso_p_mean <- length(dso$ds$aux$ddfobj$xmat$distance) / dso$Nhat)
                      if(verbose){message('     AIC = ',round(dso_aic,3), ' | Pmean = ',round(dso_p_mean,3))}
                      if(dso_pre$criterion != dso$criterion){ # proceed only if current AIC is not exact same as previous; if it is the model may be a dud
                        (summi <- data.frame(Model = length(ds_objects),
                                             Key_function=df_key,
                                             Formula=new_formula,
                                             Pmean = dso_p_mean, #summary(dso)$average.p,
                                             AIC = round(dso_aic, 3))) #AIC=round(summary(dso)$aic,3)))
                        (new_aic <- summi$AIC)
                        ds_summ <- rbind(ds_summ, summi)
                        if(new_aic < best_aic){ # compare to previous best
                          best_aic <- new_aic
                          best_num <- length(formulas)
                          newterm <- TRUE
                        } # end of if this model is better
                      } # end of if AIC pre != AIC
                      dso_pre <- dso # store the current ddf as the next previous ddf

                    } # end of sum_success
                  } # end of if this modei is a ds and ddf class
                } # end of is this model is NULL
              } # end of if this covariate is already in model
            } # end of covariate loop
          } # end of if newterm == TRUE
        } # end of step loop
      } # end of if covars is null
    } # end of base key loop

    ############################################################################
    ############################################################################
    # Build results model comparison table
    if(verbose){message('\nPreparing model comparison tables...')}
    ds_summ <-
      ds_summ %>%
      dplyr::mutate(`$\\Delta$AIC` = `AIC` - min(`AIC`)) %>%
      dplyr::mutate(`Covariates tested` = paste(covariates,collapse=', ')) %>%
      dplyr::arrange(`$\\Delta$AIC`)
    ds_summ #%>% print

    best_models <-
      ds_summ %>%
      dplyr::filter(`$\\Delta$AIC` <= delta_aic)

    dso_bests <- ds_objects[best_models$Model] ; length(dso_bests)
    dso_bests

    ############################################################################
    ############################################################################
    # Predict Pdot with best-fitting models

    best_aics <- c()
    esw_df <- data.frame()
    length(dso_bests)
    i=1
    for(i in 1:length(dso_bests)){
      dfi <- dso_bests[[i]] ; dfi
      dfi$data %>% head

      suppressWarnings({
        (esws <- predict(object = dfi, newdata = data_table, esw=TRUE)$fitted)
      })

      # Review (debugging only)
      if(FALSE){
        par(mfrow=c(3,1))
        plot(esws, type='b')
        plot(data_table$distance, type='b')
        plot(data_table$lnsstot, type='b')
        par(mfrow=c(1,1))
      }
      (esws <- t(data.frame(esws)))
      names(esws) <- paste0('v',1:length(esws))
      esws

      (esw_df <- rbind(esw_df, esws))
      #best_aics <- c(best_aics, MuMIn::AICc(dso)) #stats::AIC(dfi)$AIC)
      best_aics <- c(best_aics, MuMIn::AICc(dfi)) #stats::AIC(dfi)$AIC)
    }
    nrow(esw_df)
    esw_df

    ############################################################################
    ############################################################################
    # Average the best-fitting models
    # Calculate an average ESW for each sighting from all acceptable models, weighted by exp(-0.5*deltaAICc)

    if(nrow(esw_df) > 1){
      if(verbose){message('Averaging each sighting ESW across top-candidate detection functions ...')}
      esw_df
      (w <<- exp(-0.5*best_aics)) # mirroring Jay's code
      if(all(w==0)){w <<- c(1,1)} # handle if weights are both 0
      esw_df
      (esw_avg <- apply(esw_df,2,function(x){weighted.mean(x,w)}) %>% as.numeric)
    }else{
      esw_avg <- esw_df %>% as.numeric()
    }
    esw_avg

    # Add ESW to sightings dataframe
    sightings$esw <- esw_avg

    ############################################################################
    ############################################################################
    # Prepare outputs

    return_list$best_models <- best_models
    return_list$all_models <- ds_summ
    return_list$best_objects <- dso_bests
    return_list$sightings <- sightings
    return_list %>% names # review

  } # end of if nrow segments and nrow sightings > 0

  return(return_list)
}
