#' Group size calibration
#'
#'  This is an internal function typically not called by a user directly.
#'  It is called by the subroutine `group_size()`, which is itself a subroutine
#'  of `process_sightings()`.
#'  \cr \cr
#'  This function performs school size calibration for a single observer's estimates.
#'
#' @param obs Character vector with single observer ID. If this observer ID is contained
#' within `gs_coefficients`, its specific calibration coefficients will be used.
#' Otherwise the generic calibration coefficients will be used.
#' @param bft Numeric of Beaufort sea state during sighting.
#' @param yr Numeric of year of sighting.
#' @param gbest Numeric of best school size estimate.
#' @param glow Numeric of low school size estimate.
#' @param ghigh Numeric of high school size estimate.
#' @param gs_coefficients Dataframe of school size coefficients. If not provided, calibration will not be attempted.
#' @param calibrate_floor This argument accepts a number indicating the minimum raw school size estimate
#' for which school size calibration will be attempted.
#'
#' @return A one-row dataframe with the best school size estimate, the variance of the estimate,
#' a boolean indicating whether calibration was possible, and the slope and intercept used for calibration.
#' Passed back to `group_size()`.
#'
#' @export
#'
group_size_calibration <- function(obs,
                                   bft,
                                   yr,
                                   gbest,
                                   glow,
                                   ghigh,
                                   gs_coefficients=NULL,
                                   calibrate_floor = 0){
  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    # for debugging
    data(group_size_coefficients)
    gs_coefficients <- group_size_coefficients
    head(gs_coefficients)
    obs = '005' #197'
    bft = 4
    yr = 1987
    gbest = 65
    glow= 50
    ghigh = 90
    calibrate_floor = 0
    group_size_calibration(obs, bft, yr, gbest, glow, ghigh,
                           #gs_coefficients = NULL,
                           gs_coefficients = gs_coefficients,
                           calibrate_floor)
  }
  #=============================================================================

  # this function is called by group_size().
  # it takes the estimates from an observer and performs calibration,
  # if all the requisite data are provided.
  # see vignette for context / references

  # safe/simple copy of the coefficients table
  gsc <- gs_coefficients

  # stage results
  new_gs <- NA # new group size estimate
  gs_var <- NA # variance estimate
  gs_slope <- NA # slope used in calibration model
  gs_intercept <- NA # intercept used in calibration model
  modelno <- B0 <- B1 <- By <- yi <- yi_best <- yi_low <- yi_high <- V <- NA

  # Fix BFt, if needed
  if(is.na(bft)){bft <- 0}
  bft <- bft + 1 # do this, a la ABUND / SKOOLWT

  # Check to see if calibration is possible
  calibr <- all(c(!is.null(gsc),
                   !is.na(gbest),
                   !is.na(bft)))

  # Check to see if you have a known observer
  obs_ok <- all(c(as.numeric(obs) %in% as.numeric(gsc$obs)))

  # Check to see if all data are ok
  # is this observer's code in the coefficient table?
  data_ok <- all(c(calibr, obs_ok))

  # review
  calibr
  obs_ok
  data_ok

  # prepare weighted version of school size estimates, for this observer
  if(data_ok){

    # get coefficient table row that corresponds to observer
    gsci <- gsc[as.numeric(gsc$obs) == as.numeric(obs),]
    gsci

    # Determine best, high, low values after applying observer-specific weights
    w <- as.numeric(gsci$w_best[1])
    (yi_best <- ifelse(w>0, w*gbest, 0))

    w <- as.numeric(gsci$w_high[1])
    (yi_high <- ifelse(w>0, w*ghigh, 0))

    w <- as.numeric(gsci$w_low[1])
    (yi_low <- ifelse(w>0, w*glow, 0))

    # Get sum of weighted best high low estimates to see if at least one of them is valid
    (yi <- yi_best + yi_high + yi_low)

    # If this sum is not valid, change data_ok to FALSE
    if(is.na(yi)){
      data_ok <- FALSE
    }else{
      if(yi <= 0){data_ok <- FALSE}
    }

  }
  data_ok # review

  # prepare other parameters for calibration
  if(data_ok){
    # Get year coefficient, if it is relevant
    By <- 0 # assume it is not relevant
    (b_match <- grep(yr,names(gsci)))
    if(length(b_match)>0){
      By <- gsci[2,b_match] %>% as.numeric
    }
    By # review

    # if using year, that means using model 2
    (modelno <- ifelse(By > 0, 2,1))

    # if using year model, use second row of coefficients
    if(modelno == 1){
      gsci <- gsci[1,]
      B0 <- gsci$model_1 %>% as.numeric
    }
    if(modelno == 2){
      gsci <- gsci[2,]
      B0 <- By
    }
    B0 # review
    gsci

    # Gather other coefficients
    (B1 <- gsci$b1 %>% as.numeric) # Bft coefficient
    (gs_var <- gsci$var %>% as.numeric)
    (gs_intercept <- gsci$b0 %>% as.numeric)
    (gs_slope <-  B0 + B1*bft)

    # Handle slopes of 1
    (V <- ifelse(gs_slope == 1, 0, gs_var))

  }
  data_ok # status

  # Make sure best group size estimate falls within group size range for observer-specific model
  if(data_ok){
    if(gbest < as.numeric(gsci$min) | gbest > as.numeric(gsci$max)){data_ok <- FALSE}
  }
  data_ok # status

  # Make sure model slope is not 0
  if(data_ok){
    if(gs_slope == 0){data_ok <- FALSE}
  }
  data_ok # status

  # if everything looks good, perform calibration
  if(data_ok){
    yi
    gs_slope
    gs_intercept
    (new_gs <- exp((log(yi) - (V/2) - gs_intercept) / gs_slope))

  }else{
    # What to do if some data are missing or obs is not in coeffs file:
    if(calibr){ # only do this if a calibration table has been provided
    # if(!is.null(gsc)){ # only do this if a calibration table has been provided

      # If best is not available, use low
      if(!is.na(gbest) & gbest > 0){
        new_gs <- gbest
      }else{
        new_gs <- glow
      }

      if(!is.na(new_gs)){
        if(new_gs >= calibrate_floor){
          new_gs <- new_gs / 0.8625 # after ABUND9 fortran code
          gs_var <- 0.3166 # after ABUND 9 fortran code
        }
      }
    }
  }

  # Compile results
  result <- data.frame(best = new_gs,
                       var = gs_var,
                       calibr = calibr, # gs coefficients, bft, and best estimates valid
                                        # calibration was attempted
                       data_ok = data_ok, # all data valid throughout all calibration steps
                       obs_ok = obs_ok, # observers all known
                                        # if FALSE, the function used a generic calibration rather than an observer-specific calibration.
                       gs_slope = gs_slope,
                       gs_intercept = gs_intercept,
                       gbest = gbest,
                       glow = glow,
                       ghigh,
                       modelno = modelno,
                       B0 = B0,
                       B1 = B1,
                       By = By,
                       yi = yi,
                       yi_best = yi_best,
                       yi_low = yi_low,
                       yi_high = yi_high,
                       V=V)
  result

  return(result)
}

