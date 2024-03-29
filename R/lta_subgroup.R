#' Subgroup-based line transect analysis
#'
#' A flexible routine for carrying out subgroup-based line-transect analysis
#' using the methodologies described for Hawai'ian false killer whales,
#' *Pseudorca crassidens*, in [Bradford et al. (2020)](https://www.fisheries.noaa.gov/inport/item/59592).
#' The function returns an estimate
#' of density and abundance -- along with estimates of intermediate parameters --
#' with a CV derived from a bootstrapping routine.
#' As part of this process, relative trackline detection probability (`g(0)`) is modeled
#' as a function of Beaufort sea state (using `LTabndR` function `g0_model()`),
#' then a weighted `g(0)` and its CV are estimated using `LTabundR` function `g0_weighted()`.
#'
#' @param df_sits (Required.) A `data.frame` of sightings you want to use to fit the detection function model.
#' For false killer whales in Bradford et al. (2020), this is a combination of systematic sightings
#' prior to 2010 and Phase 1 sightings from 2010 onwards (using the PC protocol).
#' This `dataframe` must have a column named `PerpDistKM` with detection distances in km.
#' No filtering will be applied to these sightings within this function,
#' so make sure you provide the data pre-filtered. Bradford et al. (2020) used a
#' single detection function for all populations of false killer whale.
#'
#' @param truncation_distance (Required.) The truncation distance, in km, to apply during detection function model fitting.
#'
#' @param ss (Required.) A numeric vector of subgroup school sizes to use to find its mean and bootstrapped CV.
#' In Bradford et al. (2020), data come from all Phase 1 and Phase 2 estimates of subgroup sizes from 2010 onwards.
#' These estimates are the geometric mean of repeat estimates from separate observers.
#'
#' @param density_segments (Required.) The survey segments to be used in density/abundance estimation.
#' For example, Bradford et al. (2020) used 150-km segments to estimate false killer whale
#' density in the Hawaiian EEZ in 2017 (these data are available in the built-in dataset
#' `"cnp_150km_1986_2020"`). No filtering will be applied to these segments,
#' so make sure only the segments you wish to use are included and nothing more.
#' For example, in the case above, make sure you are only providing systematic segments for the
#' Hawaiian EEZ in 2017.
#'
#' @param density_das (Required.) The complete survey data corresponding to the above segments.
#' These data will be used to determine the proportion of survey effort occurring in each Beaufort
#' sea state during Relative `g(0)` estimation.
#'
#' @param density_sightings (Required.) The encounters to use in density/abundance estimation.
#' In Bradford et al. (2020), these were the Phase 1 detections of false killer whales
#' within the population-region-year of interest, e.g., Northwest Hawaiian Island population sightings
#' within the Hawaiian EEZ in 2017. No filtering will be applied to these sightings,
#' so make sure only the sightings you wish to use are included and nothing more.
#'
#' @param Rg0 A `data.frame` with estimates of Relative *g(0)* and its CV at each Bft state.
#' If this input is left `NULL`, then these estimates will be produced by the function using the subsequent `g0_` inputs.
#' If this input is not supplied and any of the subsequent `g0_` inputs are missing, then *g(0)* will be assumed to be 1.0 with CV of 0.0.
#' If supplied, this `data.frame` has three required columns:
#' `bft` (Beaufort sea state, numbers between 0 and 7),
#' `Rg0` (*Rg(0)* estimates for each Beaufort state),
#' and `Rg0_CV` (the CV of the *Rg(0)* estimate in each Beaufort state). Other columns are allowed but will be ignored.
#'
#' @param cruz10 A processed `cruz` object with short segment lengths, ideally 10 km or less (hence the 10 in the input name).
#' This `cruz` object will be used to estimate `Rg(0)`, i.e., the relative trackline detection probability.
#' Consider using the built-in dataset `"noaa_10km_1986_2020"`.
#'
#' @param g0_spp A character vector of species codes to use to estimate `Rg(0)`.
#' In most cases this will be a single species, e.g., '033' for false killer whales.
#' Not required if the `Rg0` input is supplied.
#'
#' @param g0_truncation The truncation distance to use when estimating `Rg(0)`.
#' In Bradford et al. (2020) this is 5.5 km.
#'
#' @param g0_constrain_shape Some *Rg(0)* curves will not decline monotonically
#' due to sample size issues at low Bft (0-2) or high Bft (5-6) states.
#' To coerce monotonic decline, set this input to `TRUE`, and the function will use a shape-constrained GAM
#' (`scam()` from package `scam`) instead of a classic `mgcv::gam()`.
#'
#' @param g0_jackknife_fraction The proportion of data to leave out within each jackknife permutation for
#' estimating the CV of *g(0)*. The default is 0.1 (i.e., 10% of the data, yielding 10 jackknife loops), after Barlow (2015).
#'
#' @param abundance_area The area in square km of the region of interest. The density
#' estimate will be scaled by this area.
#'
#' @param iterations Number of iterations to use in the various CV bootstrapping procedures
#' occurring throughout this function, specifically: Effective Strip Half-Width CV estimation,
#' school size CV estimation, weighted `g(0)` CV estimation, encounter rate estimation, and density/abundance estimation.
#'
#' @param output_dir The path in which results `RData` files should be stored. If left `NULL`, no results will be stored.
#' To use your current working directory, simply provide `""`.
#'
#' @param toplot A Boolean, with default `FALSE`, indicating whether to plot various
#' aspects of the analysis.
#'
#' @param verbose A Boolean, with default `TRUE`, indicating whether to print status updates to the Console.
#'
#' @details This function performs the following operations:
#' \enumerate{
#' \item Fits a detection function to `df_sits` without covariates, using the `LTabundR` function `df_fit()`,
#' in order to estimate the effective strip half-width (ESW).
#' \item Conducts bootstrap re-sampling of the detection function fitting routine in order to estimate the CV of ESW.
#' \item Estimates the arithmetic mean of subgroup school size based on the `ss` input.
#' \item Creates a bootstrap-resampled distribution of subgroup school sizes, with which CV is estimated.
#' \item Models the relative g(0) in different survey conditions using the `LTabundR` function `g0_model()`.
#' This function also estimates the CV of the Rg(0) estimate in each Beaufort sea state using jackknife resampling.
#' \item Estimates the encounter rate (subgroup detections / trackline surveyed).
#' \item Creates a bootstrap-resampled distribution of encounter rate estimates.
#' \item Calculates a weighted `g(0)` estimate according to the proportion of effort occurring in each Beaufort sea state,
#' then uses an automated parameter optimization routine (see details in `LTabundR` function `g0_weighted()`) to
#' estimate the CV of the weighted `g(0)` estimate.
#' \item Creates a bootstrap-resampled distribution of the weighted `g(0)` estimate.
#' \item Estimates density using the best estimates of effective strip half-width, school size, `g(0)`, and the encounter rate.
#' \item Estimates abundance by scaling the density estimate by the provided `abundance_area`.
#' \item Creates a bootstrap-resampled distribution of the density estimate by
#' iteratively drawing values (without replacement) from the resampled distributions
#' of the constituent parameters of the density equation.
#' \item Creates a bootstrap-resampled distribtion of the abundance estimate by
#' scaling the density distribution by `abundance_area`.
#' }
#'
#'
#' @return A list.
#' \enumerate{
#' \item `D`: The estimate of density.
#' \item `D_CV`: The CV of the density estimate.
#' \item `D_L95`: The lower 95% confidence interval of density using the BCA method.
#' \item `D_U95`: The upper 95% confidence interval of density using the BCA method.
#' \item `N`: The estimate of abundance.
#' \item `N_CV`: The CV of the abundance estimate.
#' \item `N_L95`: The lower 95% confidence interval of abundance using the BCA method.
#' \item `N_U95`: The upper 95% confidence interval of abundance using the BCA method.
#' \item `ER`: The estimate of the encounter rate.
#' \item  `ESW` = The estimate of the Effective Strip Width (km).
#' \item  `ESW_CV` = Estimate of ESW CV, based on standard deviation of bootstrap estimates of ESW.
#' \item `ss` = Mean subgroup size estimate.
#' \item `n`: Number of sightings used in density estimation.
#' \item `L`: Survey effort, in km, used in density estimation.
#' \item `n_segments`: Number of effort segments used in density estimation.
#' \item `g0`: The empirical weighted mean of *g(0)* for the point estimate, based on sightings conditions in `density_segments`.
#' \item `g0_cv`: The CV of this estimate of the point estimate of the weighted mean of *g(0)*, as estimated by an MCMC routine.
#' \item `g0_details`: A `list` with detailed results from `Rg(0)` estimation (see output details in `?g0_model`).
#' \item `df`: A `list` with detailed results from detection function estimation (see output details in `?df_fit`).
#' \item `bootstraps`: A named `list` with the bootstrapped values for `esw` (effective strip half-width),
#' `ss` (subgroup size), `g0` (relative g(0)), `er` (encounter rate), `D` (density), and `N` (abundance).
#' \item `iterations`: number of bootstrap iterations used for CV estimation.
#' }
#' See the [online vignette](https://emk-noaa.github.io/LTAvignette/subgroup-based-analysis.html) for more details
#'
#' @export
#' @import dplyr
#'
lta_subgroup <- function(df_sits, # DateTime, Lat, Lon, Cruise, PerpDistKm
                         truncation_distance,
                         ss, # numeric vector of sightings
                         density_segments, # already filtered to population
                         density_das,
                         density_sightings,
                         Rg0 = NULL,
                         cruz10 = NULL, # if NULL, load the built-in dataset
                         g0_spp = NULL,
                         g0_truncation = NULL,
                         g0_constrain_shape = FALSE,
                         g0_jackknife_fraction = 0.1,
                         abundance_area = NULL,
                         iterations = 5000,
                         output_dir = NULL,
                         toplot = FALSE,
                         verbose = TRUE){

  ##############################################################################
  # Debugging (not run)

  if(FALSE){ # to develop/debug, use Pelagic population in 2017

    #document()
    # prep cruz  ===============================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cruz$cohorts$all$sightings$stratum %>% table
    cruz$cohorts$pseudorca$subgroups$sightings
    cruz$cohorts$pseudorca$subgroups$subgroups %>% names

    # df_sits ==================================================================

    # For 1986 - 2010, assume all detections are Phase 1
    sits1  <-
      cruz$cohorts$all$sightings %>%
      filter(OnEffort == TRUE,
             year < 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             species == '033',
             mixed == FALSE) %>%
      select(DateTime, Lat, Lon, Cruise, PerpDistKm)

    sits1 %>% nrow

    # For 2011 on, use subgroups data and filter to Phase 1 only
    sits2  <-
      cruz$cohorts$all$subgroups$subgroups %>%
      filter(OnEffort == TRUE,
             lubridate::year(DateTime) >= 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             Species == '033',
             Angle <= 90,
             ObsStd == TRUE,
             Phase == 1) %>%
      select(DateTime, Lat, Lon, Cruise, PerpDistKm = PerpDist)

    sits2 %>% nrow

    # Combine
    df_sits <- rbind(sits1, sits2)
    nrow(df_sits)

    # Truncation_distance ======================================================
    hist(df_sits$PerpDistKm)
    quantile(df_sits$PerpDistKm, c(0.90,0.91,0.92,0.93,0.94,.95))
    abline(v=4.5, col='red', lty=3)
    truncation_distance <- 4.5

    # ss =======================================================================
    # School size of subgroups # based on 2012 - 2020 PC protocol only
    # not doing any phase 1 / phase 2 GLMM for now -- assume phase 1 and phase 2 are eqwally unbiased

    ss  <-
      cruz$cohort$all$subgroups$subgroups %>%
      filter(lubridate::year(DateTime) >= 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             GSBest_geom_valid == TRUE,
             Species == '033') %>%
      pull(GSBest_geom)

    ss %>% length

    # g0 params ================================================================

    data(barlow_2015)
    barlow_2015 %>% pull(title) %>% unique

    Rg0_fkw <- data.frame(title = 'False killer whale',
                          scientific = 'Pseudorca crassidens',
                          spp = '033',
                          truncation = NA,
                          pooling = 'none',
                          regions = 'none',
                          bft = 0:6,
                          Rg0 = c(1, 1, .72, .51, .37, .26, .19),
                          Rg0_CV = c(0, 0, 0.11, 0.22, 0.34, 0.46, 0.59),
                          ESW = c(3.64, 3.37, 3.10, 2.82, 2.56, 2.30, 2.07),
                          ESW_CV = c(.23, .19, .14, .07, .07, .15, .24))
    Rg0_fkw
    Rg0 <- Rg0_fkw

    g0_spp <- '033'
    g0_truncation <- 5.5
    g0_constrain_shape = FALSE
    g0_jackknife_fraction = 0.1

    data("noaa_10km_1986_2020")
    cruz10 <- noaa_10km_1986_2020

    # density_segments =========================================================

    cruz$strata
    cruzi <- filter_cruz(cruz = cruz,
                         analysis_only = TRUE,
                         years = 2017,
                         cruises = c(1705, 1706),
                         regions = 'HI_EEZ',
                         bft_range = 0:6,
                         eff_types = 'S',
                         on_off = TRUE)
    # simplify_strata
    cruzi$cohorts$all$segments$stratum <- 'HI-EEZ'
    density_segments <- cruzi$cohorts$all$segments
    density_das <- cruz$cohorts$all$das

    # density_sightings ========================================================

    density_sightings  <-
      cruz$cohorts$all$subgroups$subgroups %>%
      filter(EffType == 'S',
             OnEffort == TRUE,
             lubridate::year(DateTime) == 2017,
             PerpDist <= truncation_distance,
             Species == '033',
             Phase == 1)
    density_sightings %>% nrow
    density_sightings %>% head

    # abundance_area  ==========================================================
    (abundance_area <- cruz$strata$area[cruz$strata$stratum == 'HI_EEZ'])

    # final params
    iterations <- 20
    output_dir <- '../test_code/subgroup/'
    output_dir <- "/Users/ekezell/Desktop"
    toplot = TRUE
    verbose = TRUE

    # cruz <- cruzi

    # try it ===================================================================
    lta_subgroup(df_sits,
                 truncation_distance,
                 ss,
                 density_segments,
                 density_das,
                 density_sightings,
                 Rg0 = Rg0,
                 cruz10,
                 g0_spp,
                 g0_truncation,
                 g0_constrain_shape,
                 g0_jackknife_fraction,
                 abundance_area,
                 iterations = 20,
                 output_dir,
                 toplot,
                 verbose)

  } # end of debugging staging area

  ##############################################################################
  ##############################################################################
  # Estimate the detection function

  # Fit detection function
  if(verbose){message('--- fitting the detection function ...')}
  df <- df_fit(sightings = df_sits, truncation_distance = truncation_distance)

  # Extract ESW (1 / f0)
  if(verbose){message('--- estimating ESW ...')}
  (esw <- predict(df$best_objects[[1]], esw=TRUE)$fitted[1])

  # Repeat this in a bootstrap method
  esw_boots <- c()
  i = 1
  for(i in 1:iterations){
    resamples <- sample(1:nrow(df_sits), size=nrow(df_sits), replace=TRUE)
    siti <- df_sits[resamples, ]
    dfi <- df_fit(sightings = siti, truncation_distance = truncation_distance, toplot=FALSE, verbose=FALSE)
    (eswi <- predict(dfi$best_objects[[1]], esw=TRUE)$fitted[1] %>% as.numeric)
    message('--- --- ',
            stringr::str_pad(i, width=4, pad=' ', side='left'),
            ' :: re-sampled ESW estimate = ',round(eswi,3), ' km')
    esw_boots <- c(esw_boots, eswi)
    if(!is.null(output_dir)){saveRDS(esw_boots, file=paste0(output_dir,'esw_boots.RData'))}
  }
  esw_boots

  ##############################################################################
  # School size bootstraps

  if(verbose){message('\n--- estimating school size ...')}
  (ss_estimate <- mean(ss))

  # Repeat for bootstrap estimate of CV
  if(verbose){message('--- --- bootstrapping ...')}
  ss_boots <- c()
  i = 1
  for(i in 1:iterations){
    resamples <- sample(1:length(ss), size=length(ss), replace=TRUE)
    (ssi <- ss[resamples] %>% mean)
    ss_boots <- c(ss_boots, ssi)
  }
  ss_boots
  if(toplot){hist(ss_boots)}

  ##############################################################################
  # Rg(0) estimation with bootstrap CV

  if(!is.null(Rg0) & is.data.frame(Rg0)){
    if(verbose){message('\n--- using supplied Rg0 values & skipping Rg0 modeling ....')}
    # Do not generate new estimates of Rg0. Just use the input.
    # but place that input within a fabricated result of g0_model(), to comply with downstream code
    g0_result <- list(summary = Rg0)

  }else{
    # Try modeling Rg0
    # First make sure all inputs are there
    if(all(!sapply(c(g0_spp, g0_truncation, cruz10, g0_constrain_shape, g0_jackknife_fraction),
                   is.null))){
      if(verbose){message('\n--- estimating Rg0 ...')}
      g0_result <-
        g0_model(
          spp = g0_spp,
          truncation_distance = g0_truncation,
          cruz = cruz10,
          constrain_shape = g0_constrain_shape,
          jackknife_fraction = g0_jackknife_fraction,
          toplot = toplot,
          verbose = verbose)
      if(!is.null(output_dir)){saveRDS(g0_result, file=paste0(output_dir,'g0_result.RData'))}
    }else{
      if(verbose){message('\n--- No Rg0 input supplied, and at least one g0_ input is missing.\n--- *** Skipping Rg0 modeling and just using g0=1, CV=0 for all sea states! ***')}
      # Not enough inputs to model Rg0
      g0_result <- list(summary = data.frame(bft = 0:6, Rg0 = 1, Rg0_CV = 0))
    }
  }


  ##############################################################################
  # Encounter rate

  if(verbose){message('\n--- estimating the encounter rate ...')}

  # Get all unique populations in sightings, including probabilities
  (populations <-
      sapply(unique(density_sightings$population),
             function(x){stringr::str_split(x,';')[[1]]}) %>%
      unlist %>%
      unique)

  #=============================================================================
  # convenience function
  #=============================================================================

  er_subgroup <- function(density_sightings,
                          density_segments,
                          populations,
                          stochastic = FALSE){

    if(nrow(density_sightings)>0){
      # Make a key with one sighting per row, summing subgroups in each sighting
      (sit_key <-
         density_sightings %>%
         group_by(sitid) %>%
         summarize(n=n(),
                   population = population[1],
                   pop_prob = pop_prob[1]))


      # Stochastically assign mixed-population sightings to a single pop? ========
      # Handle stochastic assignment
      if(stochastic == TRUE & length(populations) > 1){
        j=6
        for(j in 1:nrow(sit_key)){
          (sitj <- sit_key[j, ])
          (popj <- sitj$population)
          (pop_splits <- (stringr::str_split(popj, ';')[[1]]))
          (probj <- sitj$pop_prob)
          (prob_splits <- (stringr::str_split(probj, ';')[[1]]) %>% as.numeric)
          # See if this sighting's population is split
          if(length(pop_splits)>1 & length(prob_splits)>1){
            pop_splits
            prob_splits

            # Make sure probabilities are ordered
            pop_splits <- pop_splits[order(prob_splits)]
            prob_splits <- prob_splits[order(prob_splits)]

            # Set up a probability table
            (splits <- data.frame(population = pop_splits,
                                  prob = prob_splits) %>%
                mutate(prob_cum = cumsum(prob)))

            # Take a stochastic draw to determine population
            (drawi <- runif(1, 0, 1))
            (popi <- splits$population[splits$prob_cum >= drawi][1])

            # Update population in sit_key
            sit_key$population[j] <- popi
            sit_key$pop_prob[j] <- '1'
          }
        } # end of loop through each sighting
      } # end of stochastic reassignment =========================================
    } # end of if nrow density_sightings > 0

    # Stage encounter rate result
    er <- data.frame()

    # Loop through each population
    i=1
    for(i in 1:length(populations)){
      (popi <- populations[i])

      n <- 0
      if(nrow(density_sightings)>0){
        ni <- c()
        j=6
        for(j in 1:nrow(sit_key)){
          (sitj <- sit_key[j, ])
          (popj <- sitj$population)
          (pop_splits <- (stringr::str_split(popj, ';')[[1]]))
          (matchj <- which(pop_splits == popi))
          (probj <- sitj$pop_prob)
          (prob_splits <- (stringr::str_split(probj, ';')[[1]]) %>% as.numeric)
          if(length(matchj)>0){
            (probj <- prob_splits[matchj])
            (nj <- sitj$n * probj)
            ni <- c(ni, nj)
          }
        }
        ni
        n <- ni %>% sum
      }     # end of if nrow density_sightings > 0
      L <- density_segments$dist %>% sum
      eri <- data.frame(population = popi, n, L)
      er <- rbind(er, eri)
    }
    er
    er$er <- er$n / er$L
    return(er)
  }
  # end of convenience function ================================================

  # Point estimate of encounter rate ===========================================
  (er_estimate <- er_subgroup(density_sightings,
                              density_segments,
                              populations,
                              stochastic = FALSE))

  # Bootstrapped estimates =====================================================
  # include stochastic reassignment

  er_boots <- data.frame()
  i = 1
  for(i in 1:iterations){
    boot_data <- prep_bootstrap_datasets(segments = density_segments,
                                         sightings = density_sightings)
    eri <- er_subgroup(boot_data$sightings,
                       boot_data$segments,
                       populations,
                       stochastic = TRUE)
    #(Li <- boot_data$segments$dist %>% sum)
    #(ni <- boot_data$sightings %>% nrow)
    #(eri <- ni / Li)
    message('--- --- ',
            stringr::str_pad(i, width=4, pad=' ', side='left'),
            ' :: re-sampled encounter rate estimate = ',
            stringr::str_pad(round(mean(eri$er)*100,3), width=5, pad='0', side='right'),
            ' subgroups / 100 km2')
    er_boots <- rbind(er_boots, data.frame(i, eri))
    if(!is.null(output_dir)){saveRDS(er_boots, file=paste0(output_dir, 'er_boots.RData'))}
  }
  er_boots

  ##############################################################################
  # Weighted g(0) and weighted CV

  if(verbose){message('\n--- calculating bft-weighted Rg0 ...')}

  (Rg0 <- g0_result$summary$Rg0)
  (Rg0_cv <- g0_result$summary$Rg0_CV)

  # Automatically find the survey-specific weighted mean and CV
  if(verbose){message('--- --- calculating CV ...')}
  cruz <- list(cohorts = list(all = list(das = density_das)))
  g0w <- g0_weighted(Rg0, Rg0_cv, cruz = cruz)
  g0w

  (g0_wt_mn <- g0w$g0$wt.mean)
  (g0_wt_cv <- g0w$g0$wt.cv)

  # Model g0 as a logit-transformed deviate
  if(verbose){message('\n--- creating bootstrap distribution of weighted g(0) values ...')}
  (g0_param <- g0_optimize(g0_wt_mn, g0_wt_cv, try_count = 20, verbose = FALSE)$bestFit)
  g0_boots <- plogis(rnorm(iterations,g0_param[1],g0_param[2]))
  if(toplot){hist(g0_boots)}

  ##############################################################################
  # Density estimation

  # Get estimate
  if(verbose){message('\n--- estimating density & abundance...')}

  populations
  er_estimate$D <- NA
  er_estimate$N <- NA
  its <- data.frame()
  i=1
  for(i in 1:length(populations)){
    (popi <- populations[i])
    message('--- --- population = ', popi)

    message('--- --- --- point estimate...')
    (eri <- er_estimate %>% filter(population == popi))
    (D <- eri$er * (as.numeric(ss_estimate) / (2 * esw * g0_wt_mn)))
    D * 100
    er_estimate$D[i] <- D
    if(!is.null(abundance_area)){
      N <- round(D * abundance_area) %>% as.numeric
      er_estimate$N[i] <- N
    }

    message('--- --- --- bootstraps...\n')
    (booti <- er_boots %>% filter(population == popi))
    (itsi <- data.frame(booti,
                        ss = ss_boots,
                        esw = esw_boots,
                        g0 = g0_boots))
    itsi <- itsi[sample(1:nrow(itsi), size= iterations, replace=FALSE),]
    itsi %>% head
    itsi$D <- itsi$er * (itsi$ss / (2 * itsi$esw * itsi$g0))
    itsi$N <- NA
    if(!is.null(abundance_area)){
      itsi$N <- round(itsi$D * abundance_area) %>% as.numeric
    }
    its <- rbind(its, itsi)
  }
  er_estimate
  its %>% head
  its %>% nrow


  if(toplot){
    ggplot(its, aes(x=D)) +
      geom_histogram() +
      facet_wrap(~population, scales='free_x')
  }

  ##############################################################################
  # Outputs

  if(verbose){message('\n--- compiling results ...')}

  er_estimate
  its %>% head

  bootsumm <-
    its %>%
    group_by(population) %>%
    summarize(D_sd = sd(D),
              D_L95 = coxed::bca(D)[1],
              D_U95 = coxed::bca(D)[2],
              N_L95 = coxed::bca(N)[1],
              N_U95 = coxed::bca(N)[2],
              ESW_sd = sd(esw),
              ss_sd = sd(ss),
              er_sd = sd(er))

  pops <- left_join(er_estimate, bootsumm, by='population')
  pops <-
    pops %>%
    mutate(ESW = esw %>% as.numeric,
           group = ss_estimate,
           n_segments = nrow(density_segments),
           g0 = g0_wt_mn,
           g0_cv = g0_wt_cv,
           Area = ifelse(is.null(abundance_area), NA, abundance_area)) %>%
    mutate(CV = D_sd / D,
           ESW_CV = ESW_sd / ESW,
           ER_CV = er_sd / er,
           group_sd = ss_sd,
           group_CV = ss_sd / group) %>%
    select(population, L, n_segments,
           g0, g0_cv,
           ESW, ESW_CV,
           group, group_sd, group_CV,
           n, ER=er, ER_CV,
           D, CV, D_L95, D_U95,
           Area, N, N_L95, N_U95)

  pops

  results <- list(estimate = pops,
                  bft = g0w$bft,
                  g0_details = g0_result,
                  df = df,
                  bootstraps = its,
                  iterations = iterations)

  results
  if(!is.null(output_dir)){saveRDS(results, file=paste0(output_dir,'lta_subgroup_results.RData'))}

  if(verbose){message('\nFinished!\n')}

  return(results)
}
