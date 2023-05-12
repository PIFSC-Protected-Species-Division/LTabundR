#' Get detection function curve
#'
#' This is an internal function, typically not called by the user.
#' It returns a single detection function curve based on one or more detection function models.
#' If multiple models are provided, their curves will be averaged together to provide a single curve,
#' weighting the average based upon the model AIC.
#'
#' @param models A list of best-fitting detection function model(s) (produced by `LTabundR::fit_df()`).
#' @param covariates If `NULL`, the function will assume that this detection function does not use covariates.
#' If covariates *are* used, this input should be anything *except* `NULL`.
#' @param truncation_distance Truncation distance used, in km.
#'
#' @details This function is adapted from code in `mrds::plot.ds()` and `mrds::detfct()`.
#'
#' @return A `data.frame` of length 101, with two columns:
#' `km` is distance from the trackline, and `p` is the average detection probability at that distance,
#' based on the data's covariates.
#'
#' @export
#'
df_curve <- function(models, covariates = NULL, truncation_distance){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    covariates = NULL
    truncation_distance = 5
    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    sightings <- cruz$cohorts$all$sightings
    df_result <- df_fit(sightings, truncation_distance)
    models <- df_result$best_objects
  } # end debugging
  #=============================================================================
  # This code is an adapation of code from package mrds, and I don't fully understand
  # all of the code. (Eric K)

  # Setup km breaks
  xgrid <- NULL
  width <- truncation_distance
  finebr <- seq(0, width, length.out=101)
  for(i in 1:(length(finebr)-1)){
    x <- (finebr[i]+finebr[i+1])/2
    xgrid <- c(xgrid, x)
  }
  finebr
  xgrid

  # Stage results dataframes
  probs <- data.frame()
  aics <- c()

  # Loop through each model provided
  mi=1
  for(mi in 1:length(models)){
    ds <- models[[mi]]
    aics <- c(aics, AIC(ds)$AIC)
    ddfobj <- ds$ds$aux$ddfobj

    xmat <- ddfobj$xmat
    left <- 0
    width <- truncation_distance
    normalize <- FALSE
    if(is.null(ds$ds$aux$int.range)){
      int.range <- c(0,width)
    }else{
      int.range <- ds$ds$aux$int.range
    }
    if(is.matrix(int.range)){
      max.range <- as.vector(int.range[1,])
      int.range <- int.range[2:dim(int.range)[1],]
      range.varies <- TRUE
    }else{
      max.range <- int.range
      normalize <- FALSE
      range.varies <- FALSE
    }
    pdot <- ds$fitted

    # when we have covariates
    if(!is.null(covariates)){
      linevalues <- NULL
      newdat <- xmat
      for(i in 1:length(xgrid)){
        x <- xgrid[i]
        newdat$distance <- rep(x, nrow(newdat))
        detfct.values <- mrds::detfct(newdat$distance, ddfobj)
        if(!normalize&range.varies){
          detfct.values[x<int.range[, 1] | x>int.range[, 2]] <- 0
        }
        linevalues <- c(linevalues, sum(detfct.values/pdot)/sum(1/pdot))
      }

      ## without covariates
    }else{
      if(!is.null(ddfobj$scale)){
        ddfobj$scale$dm <- ddfobj$scale$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }
      if(!is.null(ddfobj$shape)){
        ddfobj$shape$dm <- ddfobj$shape$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }
      linevalues <- mrds::detfct(xgrid, ddfobj, width=width)
    }
    probs <- rbind(probs, linevalues)
  }

  probs
  aics

  (w <<- exp(-0.5*aics))
  (avg_prob <- apply(probs,2,function(x){weighted.mean(x,w)}) %>% as.numeric)

  if(FALSE){ # QA/QC -- not run!
   plot(2, xlim=c(0, width), ylim=c(0,1))
   lines(as.numeric(probs[1,])~xgrid)
   lines(as.numeric(probs[2,])~xgrid)
   lines(avg_prob~xgrid)
  }

  to_return <- data.frame(km=xgrid, p=avg_prob)

  return(to_return)
}
