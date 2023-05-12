#' Find a single weighted g(0) estimate (and CV) from a set of Beaufort-specific estimates.
#'
#' Use this function to summarize the Beaufort-specific model of relative trackline probabilities
#' (from `LTabundR::g0_bft_model()`) as a single relative g(0) estimate, weighted
#' by the prevalence of each Beaufort sea state within the survey data.
#' This function automatically estimates the CV of this weighted value using an iterative
#' loop with an MCMC core, adapted from Jeff Moore's code.
#'
#' @param Rg0 A numeric vector, length 7, of Relative g(0) estimates
#' for Beaufort sea states 0 - 6. See documentation of `LTabundR::g0_bft_model()`,
#' which provides this vector as a column in its output, for details on the
#' concept of relative trackline detection probabilities.
#' @param Rg0_cv A numeric vector, length 7, of the CV of Relative g(0) estimates.
#' See same documentation as above for details.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort name within your `cruz` object that you want to analyze.
#' @param beta_range The range in distribution parameters for simulating the g(0) mean.
#' @param beta_step The interval between candidate values within the above range.
#' A smaller step means higher resolution and accuracy, but takes longer to process.
#' @param beta_sd_range The range in distribution parameters for simulating the g(0) SD.
#' @param beta_sd_step The interval between candidate values for the above SD range.
#' @param iterations The number of draws used to simulate the underlying distribution.
#' @param ymax The maximum value on the y axis of the diagnostic Rg0 SD plot.
#' @param toplot Boolean, with default `TRUE`, indicating whether results should be plotted.
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @details This code is an adaptation and expansion of Jeff Moore's code (February 2020).
#' This function automatically estimates the CV of this weighted value using an iterative
#' loop with an MCMC core. The MCMC routine is described in Bradford et al. (2021).
#' The iterative loop tries various candidate values for the distribution parameters and calculates
#' the cumulative error of the resulting distribution at each Beaufort state. It then
#' determines the candidate value with the lowest error, weighting each error according to the
#' proportion of effort occurring in the respective Beaufort state.
#'
#' @return The output is provided in the form of a `list` with two slots: `g0` and `bft`.
#' The `g0` slot holds a `data.frame` in which `$wt.mean` is the
#' weighted mean g(0) value across all Beaufort states for the survey data provided,
#' and `$wt.cv` is the CV of that estimate.
#' Other values are reported to facilitate QA/QC, but `$wt.mean` and `$wt.cv` is what
#' you would pass to an abundance estimation routine, such as `LTabundR::lta()`).
#' The `bft` slot holds a dataframe with the proportion of effort in each Beaufort sea state.
#'
#' @import magrittr
#' @import dplyr
#'
#' @export
#'
g0_weighted <- function(Rg0,
                        Rg0_cv,
                        cruz,
                        cohort = 1,
                        iterations = 10000,
                        beta_range = c(-1.5, 0),
                        beta_step = 0.001,
                        beta_sd_range = c(0, 0.3),
                        beta_sd_step = 0.0001,
                        ymax = 0.2,
                        toplot = TRUE,
                        verbose = TRUE){

  if(FALSE){ # not run -- for debugging only ===================================
    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    cruz <- filter_cruz(cruz, years = 2017, regions = 'HI_EEZ')
    iterations = 1000
    cohort = 1
    beta_range <- c(-1.5, 0)
    beta_step <- 0.001
    beta_sd_step <- 0.0001
    beta_sd_range <- c(0,.3)
    toplot = TRUE
    verbose = TRUE
    ymax = .2

    Rg0 = rg0ii$Rg0
    Rg0_cv = rg0ii$Rg0_CV
    cruz = cruzg0

    # Minke whale (no pooling)
    Rg0 = c(1,0.503,0.262,0.148,0.094,0.067,0.050)  # Jay's values, mean and SD (from ALB)
    Rg0_sd = c(0,0.181,0.183,0.130,0.077,0.048,0.046)
    (Rg0_cv <- Rg0_sd / Rg0)
    g0_weighted(Rg0, Rg0_cv, cruz, ymax = 0.3) # try it

    # Cuvier's beaked whale (no pooling, but absolute g0 estimate at bft = 0).
    Rg0 = c(0.584,0.402,0.276,0.190,0.131,0.090,0.062)  # Jay's values, mean and SD
    Rg0_sd = c(0,0.040,0.053,0.049,0.039,0.031,0.023)
    (Rg0_cv <- Rg0_sd / Rg0)
    g0_weighted(Rg0, Rg0_cv, cruz) # try it

    # Pelagic false killer whales (pool 1)
    Rg0 = c(1,1,0.715,0.512,0.366,0.262,0.188)  # from jay's analysis (from ALB)
    Rg0_sd = c(0, 0, 0.112, 0.123, 0.121, 0.111, 0.098) # from jay's analysis
    (Rg0_cv <- Rg0_sd / Rg0)
    g0_weighted(Rg0, Rg0_cv, cruz) # try it

    # Striped dolphins
    Rg0 = c(1,1,0.794,0.516,0.303,0.231,0.234)  # Jay's values, mean and SD (from ALB)
    Rg0_sd = c(0, 0, 0.087, 0.072, 0.033, 0.037, 0.073)
    (Rg0_cv <- Rg0_sd / Rg0)
    g0_weighted(Rg0, Rg0_cv, cruz)  # try it # wt g0 = 0.331, wt cv = 0.146

    # Short-finned pilot whales
    Rg0 = c(1,1,1,0.835,0.631,0.430,0.283)  # Jay's values, mean and SD (from ALB)
    Rg0_sd = c(0, 0, 0, 0.067, 0.095, 0.103, 0.099)
    (Rg0_cv <- Rg0_sd / Rg0)
    g0_weighted(Rg0, Rg0_cv, cruz)  # try it # wt g0 = 0.56, wt cv = 0.158

  }
  # ============================================================================

  # Get effort proportioned by Bft
  # this uses a LTabundR function, summarize_bft()
  bft <<- NULL
  (bft <<- summarize_bft(cruz, cohort = cohort)$overall)

  if(verbose){
    message('\nShare of survey effort in each Beaufort state ...')
    print(bft)
    message('\n')
  }

  # Use this Bft allocation as the weight
  weights <- sapply(0:6, function(x){
    matchi <- which(bft$bftr == x)
    returni <- ifelse(length(matchi)>0, bft$prop[matchi], 0)
    return(returni)
  })
  (wts = weights/sum(weights))  # the effort weights, rescaled to sum to 1

  # Get g0 deets  ==============================================================
  (g0max <- max(Rg0))
  Rg0_var <- Rg0^2 * Rg0_cv^2
  (Rg0_sd <- Rg0 * Rg0_cv)

  # Determine pooling   ========================================================
  pool <- 0
  if(length(unique(Rg0[1:2])) == 1){pool <- 1}
  if(length(unique(Rg0[1:3])) == 1){pool <- 2}
  pool

  ##############################################################################
  ##############################################################################
  # Estimate mean

  # Stage results
  parlist <- list()
  error_df <- data.frame()

  # Begin loop of candidate values for the weighted mean
  if(verbose){message('Determining the distribution parameter to replicate the weighted g(0) mean ...')}

  # Parameter options
  (beta_ops <- seq(beta_range[1], beta_range[2], by=beta_step))
  beti  <- -.36 # debugging values
  beti_sd <- .03
  #bi=982
  if(verbose){pb <- txtProgressBar(1, length(beta_ops), style=3)} # setup progress bar

  for(bi in 1:length(beta_ops)){
    if(verbose){setTxtProgressBar(pb, bi)} # update progress bar
    (beti <- beta_ops[bi])
    beta.i = rnorm(iterations, mean=beti, sd=beti_sd)  # draw random beta's from a normal distribution

    # this is adapted from jeff moore's / jay barlow's code, and i don't fully understand its details (eric k)

    # empty matrix to store the g0(x) vals
    if(g0max < 1){x <- 1:7}else{x <- 1:6}
    g0.ix = matrix(g0max,iterations,length(x))
    g0.ix

    # find the g0(x) values given the inputs for beta and the exponential model
    for(j in pool:length(x)){
      if(pool == 0){ g0.ix[,j]=exp(beta.i*(x[j])) }
      if(pool == 1){ g0.ix[,j]=exp(beta.i*(x[j] - 1)) }
      if(pool == 2){ g0.ix[,j]=exp(beta.i*(x[j] - 2)) }
    }
    g0.ix

    if(g0max < 1){
      g0.ix.new <- g0.ix
    }else{
      g0.ix.new = cbind(rep(g0max,iterations),g0.ix)  # add a column for Bft 0
    }
    g0.ix.new

    wtMean.i = rep(NA,iterations)  # empty vector to hold weighted means
    suppressWarnings({
      for(i in 1:iterations){
        wtMean.i[i] = sum(g0.ix.new[i,] * wts)  # find weighted mean g0 for each replicate
      }
    })
    (bft.means = apply(g0.ix.new,2,mean))  # find mean g0 for each Bft state
    bft.sds = apply(g0.ix.new,2,sd)  # find sd of g0 for each Bft state
    wt.mean = mean(wtMean.i) # mean of the weighted means
    wt.var = var(wtMean.i) # var of the weighted means
    wt.sd = sd(wtMean.i) # sd of the weighted means
    wt.cv = wt.sd/wt.mean # cv of the weighted means

    (mean_errors <- Rg0 - bft.means)
    weighted_errors <- mean_errors * wts
    (tot_error <- weighted_errors %>% sum)

    # Results of this iterations
    outputs = list(beta = beti,
                   beta_sd = beti_sd,
                   wt.mean = wt.mean,
                   wt.var = wt.var,
                   wt.sd = wt.sd,
                   wt.cv = wt.cv,
                   bft.means=bft.means,
                   bft.sds=bft.sds,
                   mean_errors = mean_errors,
                   weighted_errors = weighted_errors,
                   tot_error = tot_error %>% sum)
    outputs
    parlist[[bi]] <- outputs

    # Add to results
    (error_dfi <- data.frame(i = bi, error = tot_error))
    (error_df <- rbind(error_df, error_dfi))
  }

  # Review results
  error_df

  # Find best-fitting candidate parameter based on minimim weighted-mean error
  (best_fit_i <- which.min(abs(error_df$error)))

  # Get the details for the best-fitting parameter
  length(parlist)
  best_fit_mn <- parlist[[best_fit_i]]
  best_fit_mn
  if(verbose){message('\n')}

  # Stage plot
  if(toplot){
    par(mfrow=c(1,2))
    par(mar=c(4.2,4.2,2.5,.5))
    plot(1, type='n', xlim=c(0,6), ylim=c(0,1),
         col='white', xlab='Beaufort Sea State', ylab='Relative g(0)',
         main='Simulation (orange) of estimate mean (black)', cex.main = 1)
    abline(h=seq(0,1,by=.1), col='grey90')
    x <- 0:6
    lines(Rg0 ~ x, type='o', pch=16, lwd=2)
    y <- best_fit_mn$bft.means
    lines(y ~ x, lwd = 7, col = adjustcolor('darkorange',alpha.f=.4))
  }

  ##############################################################################
  ##############################################################################
  # Estimate SD
  # Replicate the above, this time constraining the mean and focusing on optimizing the SD

  if(verbose){message('Determining the distribution parameter for the weighted g(0) SD ...')}

  # Stage results again
  parlist <- list()
  error_df <- data.frame()

  # Set up candidate values for SD
  (beta_ops <- seq(beta_sd_range[1], beta_sd_range[2], by=beta_sd_step))
  (beti  <- best_fit_mn$beta)

  # Begin loop
  #beti_sd <- .03 # debugging values
  #bi=1
  if(verbose){pb <- txtProgressBar(1, length(beta_ops), style=3)} # setup progress bar
  for(bi in 1:length(beta_ops)){
    if(verbose){setTxtProgressBar(pb, bi)} # update progress bar
    (beti_sd <- beta_ops[bi])
    beta.i = rnorm(iterations, mean=beti, sd=beti_sd)  # draw random beta's from a normal distribution

    # empty matrix to store the g0(x) vals
    if(g0max < 1){x <- 1:7}else{x <- 1:6}
    g0.ix = matrix(g0max,iterations,length(x))
    g0.ix

    # find the g0(x) values given the inputs for beta and the exponential model
    for(j in pool:length(x)){
      if(pool == 0){ g0.ix[,j]=exp(beta.i*(x[j])) }
      if(pool == 1){ g0.ix[,j]=exp(beta.i*(x[j] - 1)) }
      if(pool == 2){ g0.ix[,j]=exp(beta.i*(x[j] - 2)) }
    }
    g0.ix

    if(g0max < 1){
      g0.ix.new <- g0.ix
    }else{
      g0.ix.new = cbind(rep(g0max,iterations),g0.ix)  # add a column for Bft 0
    }
    g0.ix.new

    wtMean.i = rep(NA,iterations)  # empty vector to hold weighted means
    suppressWarnings({
      for(i in 1:iterations){
        wtMean.i[i] = sum(g0.ix.new[i,] * wts)  # find weighted mean g0 for each replicate
      }
    })

    bft.means = apply(g0.ix.new,2,mean)  # find mean g0 for each Bft state
    bft.sds = apply(g0.ix.new,2,sd)  # find sd of g0 for each Bft state
    wt.mean = mean(wtMean.i) # mean of the weighted means
    wt.var = var(wtMean.i) # var of the weighted means
    wt.sd = sd(wtMean.i) # sd of the weighted means
    wt.cv = wt.sd/wt.mean # cv of the weighted means

    (mean_errors <- Rg0_sd - bft.sds)
    weighted_errors <- mean_errors * wts
    (tot_error <- weighted_errors %>% sum)

    outputs = list(beta = beti,
                   beta_sd = beti_sd,
                   wt.mean = wt.mean,
                   wt.var = wt.var,
                   wt.sd = wt.sd,
                   wt.cv = wt.cv,
                   bft.means=bft.means,
                   bft.sds=bft.sds,
                   mean_errors = mean_errors,
                   weighted_errors = weighted_errors,
                   tot_error = tot_error %>% sum)
    outputs
    parlist[[bi]] <- outputs

    # Add to results
    (error_dfi <- data.frame(i = bi, error = tot_error))
    (error_df <- rbind(error_df, error_dfi))
  }

  # Review
  error_df

  # Find best-fit
  (best_fit_i <- which.min(abs(error_df$error)))

  # Get details for best-fit
  length(parlist)
  best_fit_sd <- parlist[[best_fit_i]]
  best_fit_sd

  # SDs plot
  # Stage plot
  if(toplot){
    par(mar=c(4.2,4.2,2.5,.5))
    plot(1, type='n', xlim=c(0,6), ylim=c(0,ymax),
         col='white', xlab='Beaufort Sea State', ylab='SD of Rg(0)',
         main='Simulation (blue) of estimate SD (black)', cex.main = 1)
    abline(h=seq(0,ymax,by=.025), col='grey90')
    x <- 0:6
    # Rg0 estimates provided at function input
    lines(Rg0_sd ~ x, type='o', pch=16, lwd=2)
    # Manually fitted line
    y <- best_fit_sd$bft.sds
    lines(y ~ x, lwd = 7, col = adjustcolor('cadetblue4',alpha.f=.5))
  }

  # Prepare outputs objects
  resulti <- list(g0 = data.frame(wt.mean = best_fit_sd$wt.mean %>% round(3),
                                  wt.cv = best_fit_sd$wt.cv %>% round(3),
                                  wt.var = best_fit_sd$wt.var %>% round(3),
                                  wt.sd = best_fit_sd$wt.sd %>% round(3),
                                  plogis_mean = best_fit_sd$beta,
                                  plogis_sd = best_fit_sd$beta_sd),
                  bft = bft)

  # Close out status updates
  if(toplot){par(mfrow=c(1,1))}
  if(verbose){
    message('\n\nFinished!')
    message('Weighted g(0) mean: ', resulti$g0$wt.mean)
    message('CV of weighted g(0) value: ', resulti$g0$wt.cv)
  }

  return(resulti)
}
