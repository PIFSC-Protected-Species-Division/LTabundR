#' Find g0 values Nelder-Mead using optimization
#'
#' This is an internal function used in `LTabundR::lta()` for finding the values
#' (through `optim()`) for the parameters needed to conduct parametric bootstrapping
#' of g0 values for each non-parametric bootstrap of the LTA routine.
#' @param g0 Initial estimate of g(0).
#' @param g0_cv Initial estimate of g(0) coeffiient of variation.
#' @param try_count Number of times to attempt optimization before giving up.
#' @param seed Set a seed (any integer) to ensure that the result is reproducible.
#' If left `NULL`, the results are liable to differ for each run of this function.
#' @param verbose Print updates to the Console?
#'
#' @return A list with three named slots: `[[1]] g0_mean`, the exact mean, according to the optimization results;
#' `[[2]] g0_cv`, the CV according to optimization; and `[[3]] bestFit`, a numeric vector of length 2 containing
#' parameters to pass to the parameteric g(0) bootstrap routine in `LTabundR::lta()`. If the initial g(0) estimate
#' is 1.0, or the initial g(0) CV estimate is 0.0, then these parameters will be returned as `NA`.
#'
#' @export
#'
g0_optimize <- function(g0, g0_cv, try_count = 20, seed = NULL, verbose = TRUE){

  if(FALSE){ # debugging only === not run ========================================
      g0 <- .33
      g0_cv <- .2
      try_count = 20
      seed = NULL
      verbose = TRUE
  } # end not run ================================================================

  # This code is adapted closely from Jay Barlow's / Jeff Moore's code,
  # and I don't fully understand the details (Eric K)

  # Optimization function
  g0_search <- function(b){
    #b <- c(0.33, 0.33) # debugging values
    #g0zero <- .5
    #gzero_cv <- .20
    if(!is.null(seed)){set.seed(seed)}
    (meanG0 <- mean(plogis(rnorm(10000, b[1], b[2]))))
    if(!is.null(seed)){set.seed(seed)}
    (cvG0 <- sd(plogis(rnorm(10000, b[1], b[2]))) / gzero)
    (delta <- 10*(gzero - meanG0)^2 + (gzero_cv - cvG0)^2) # find simultaneous best-fit, weighting strongly from best mean
    return(delta)
  }

  # Optimize in order to estimate mean/CV
  g0mean <- 1
  g0cv <- 0
  bestFit <- c(NA, NA)
  if(g0[1] != 1 & g0_cv[1] != 0){
    gzero <<- g0 # set global variables
    (gzero_cv <<- g0_cv[1])
    if(!is.null(seed)){set.seed(seed)}
    (b=c(qlogis(gzero),0.1))    #initial parameter estimates

    # Attempt optimization 20 times
    icount = 1
    while (((abs(g0mean-gzero) > 0.01)|(abs(g0cv - gzero_cv) > 0.02)) & (icount < try_count)){   #set tolerance for convergence
      bestFit = optim(b,g0_search,hessian=TRUE)	#find best-fit values
      if(!is.null(seed)){set.seed(seed)}
      g0mean = mean(plogis(rnorm(10000,bestFit$par[1],bestFit$par[2])))       #check mean for accuracy
      if(!is.null(seed)){set.seed(seed)}
      g0cv = sd(plogis(rnorm(10000,bestFit$par[1],bestFit$par[2]))) / gzero   #check CV for accuracy
      b = b * 1.2                      #update initial parameter estimates and try again
      icount <- icount + 1
    }
    icount
    if(verbose & icount >= try_count) {message('WARNING! g(0) mean or CV is poor fit to actual') }
    if(verbose){message('--- g(0) estimate: par1 = ', round(bestFit$par[1],5), ' | par2 = ', round(bestFit$par[2], 5),' | mean = ', round(g0mean,5), ' | CV = ', round(g0cv, 5))}
    bestFit <- bestFit$par
  }else{
    if(verbose){message('--- g(0) estimate based on optimization: mean = ', round(g0mean,5), ' | CV = ', round(g0cv, 5))}
  }

  # Return results
  return(list(g0_mean = g0mean,
              g0_cv = g0cv,
              bestFit = bestFit))
}
