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
#' @param df_sits A `data.frame` of sightings you want to use to fit the detection function model.
#' For false killer whales in Bradford et al. (2020), this is a combination of systematic sightings
#' prior to 2010 and Phase 1 sightings from 2010 onwards (using the PC protocol).
#' This `dataframe` must have a column named `PerpDistKM` with detection distances in km.
#' No filtering will be applied to these sightings within this function,
#' so make sure you provide the data pre-filtered. Bradford et al. (2020) used a
#' single detection function for all populations of false killer whale.
#'
#' @param truncation_distance The truncation distance, in km, to apply during detection function model fitting.
#'
#' @param ss A numeric vector of subgroup school sizes to use to find its mean and bootstrapped CV.
#' In Bradford et al. (2020), data come from all Phase 1 and Phase 2 estimates of subgroup sizes from 2010 onwards.
#' These estimates are the geometric mean of repeat estimates from separate observers.
#'
#' @param cruz10 A processed `cruz` object with short segment lengths, ideally 10 km or less (hence the 10 in the input name).
#' This `cruz` object will be used to estimate `Rg(0)`, i.e., the relative trackline detection probability.
#' Consider using the built-in dataset `"noaa_10km_1986_2020"`.
#'
#' @param g0_spp A character vector of species codes to use to estimate `Rg(0)`.
#' In most cases this will be a single species, e.g., '033' for false killer whales.
#'
#' @param g0_truncation The truncation distance to use when estimating `Rg(0)`.
#' In Bradford et al. (2020) this is 5.5 km.
#'
#' @param g0_pool_bft A way to specify that low Beaufort sea states, which are typically rare in open-ocean
#' surveys, should be pooled. This step may be needed in order to achieve a monotonic decline
#' in the `g(0) ~ Bft` relationship, but the default is `NULL`, i.e., no pooling.
#' If `g0_pool_bft` is the character string `"01"`, Beaufort states 1 will be pooled into state 0.
#' If `g0_pool_bft` is the character string `"012"`, Beaufort states 1 and 2 will be pooled into state 0.
#'
#' @param g0_jackknife_fraction The proportion of data to leave out within each jackknife permutation for
#' estimating the CV of *g(0)*. The default is 0.1 (i.e., 10% of the data, yielding 10 jackknife loops), after Barlow (2015).
#'
#' @param density_segments The survey segments to be used in density/abundance estimation.
#' For example, Bradford et al. (2020) used 150-km segments to estimate false killer whale
#' density in the Hawaiian EEZ in 2017 (these data are available in the built-in dataset
#' `"cnp_150km_1986_2020"`). No filtering will be applied to these segments,
#' so make sure only the segments you wish to use are included and nothing more.
#' For example, in the case above, make sure you are only providing systematic segments for the
#' Hawaiian EEZ in 2017.
#'
#' @param density_das The complete survey data corresponding to the above segments.
#' These data will be used to determine the proportion of survey effort occurring in each Beaufort
#' sea state during Relative `g(0)` estimation.
#'
#' @param density_sightings The encounters to use in density/abundance estimation.
#' In Bradford et al. (2020), these were the Phase 1 detections of false killer whales
#' within the population-region-year of interest, e.g., Northwest Hawaiian Island population sightings
#' within the Hawaiian EEZ in 2017. No filtering will be applied to these sightings,
#' so make sure only the sightings you wish to use are included and nothing more.
#'
#' @param abundance_area The area in square km of the region of interest. The density
#' estimate will be scaled by this area.
#'
#' @param iterations Number of iterations to use in the various CV bootstrapping procedures
#' occurring throughout this function, specifically: Effective Strip Half-Width CV estimation,
#' school size CV estimation, weighted `g(0)` CV estimation, and encounter rate estimation.
#'
#' @param density_bootstraps Number of bootstrap iterations to use for
#' the CV estimate of density and abundance specifically. This input allows this final step
#' to use a different (typically larger) iteration size than the `iterations` input above.
#'
#' @param output_dir The path in which results `RData` files should be stored. If left `""`,
#' the current working directory will be used.
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
#' \item Estimates the geometric mean of subgroup school size based on the `ss` input.
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
#' iteratively drawing values from the resampled distributions
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
#' \item `n`: Number of sightings used in density estimation.
#' \item `L`: Survey effort, in km, used in density estimation.
#' \item `n_segments`: Number of effort segments used in density estimation.
#' \item `g0`: The weighted mean of Rg(0), based on sightings conditions in `density_segments`.
#' \item `g0_cv`: The CV of this estimate.
#' \item `g0_details`: A `list` with detailed results from `Rg(0)` estimation (see output details in `?g0_model`).
#' \item `df`: A `list` with detailed results from detection function estimation (see output details in `?df_fit`).
#' \item `bootstraps`: A named `list` with the bootstrapped values for `esw` (effective strip half-width),
#' `ss` (school size), `g0` (relative g(0)), `er` (encounter rate), `D` (density), and `N` (abundance).
#' \item `iterations`: number of bootstrap iterations used for CV estimation of effective strip half-width, school size, g(0), and encounter rate.
#' \item `density_bootstraps`: number of bootstrap iterations used for CV estimation of density and abundance.
#' }
#' See the [online vignette](https://emk-noaa.github.io/LTAvignette/subgroup-based-analysis.html) for more details
#'
#' @export
#'
lta_subgroup <- function(df_sits, # DateTime, Lat, Lon, Cruise, PerpDistKm
                         truncation_distance,
                         ss, # numeric vector of sightings
                         cruz10, # if NULL, load the built-in dataset
                         g0_spp,
                         g0_truncation,
                         g0_pool_bft = NULL,
                         g0_jackknife_fraction = 0.1,
                         density_segments, # already filtered to population
                         density_das,
                         density_sightings,
                         abundance_area = NULL,
                         iterations = 5000,
                         density_bootstraps = 10000,
                         output_dir = '', # end with forward slash
                         toplot = FALSE,
                         verbose = TRUE){

  ##############################################################################
  # Debugging (not run)

  if(FALSE){ # to develop/debug, use Pelagic population in 2017

    #document()

    # prep cruz  ===============================================================
    data("cnp_1986_2020_150km")
    cruz <- cnp_1986_2020_150km
    cruz$cohorts$most$sightings$stratum %>% table

    # df_sits ==================================================================

    # For 1986 - 2010, assume all detections are Phase 1
    sits1  <-
      cruz$cohorts$most$sightings %>%
      filter(OnEffort == TRUE,
             year < 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             species == '033',
             mixed == FALSE) %>%
      select(DateTime, Lat, Lon, Cruise, PerpDistKm)

    # For 2011 on, use subgroups data and filter to Phase 1 only
    sits2  <-
      cruz$cohorts$most$subgroups$subgroups %>%
      filter(OnEffort == TRUE,
             lubridate::year(DateTime) >= 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             Species == '033',
             Phase == 1) %>%
      select(DateTime, Lat, Lon, Cruise, PerpDistKm = PerpDist)

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
      cruz$cohort$most$subgroups$subgroups %>%
      filter(lubridate::year(DateTime) >= 2011,
             Lat >= 5, Lat <= 40, Lon >= -185, Lon <= -120,
             Species == '033') %>%
      select(DateTime, Lat, Lon, Cruise, PerpDistKm = PerpDist, ss=GSBest_geom)
    ss %>% nrow
    (ss <- ss$ss %>% as.numeric)

    # g0 params ================================================================

    g0_spp <- '033'
    g0_truncation <- 5.5
    g0_pool_bft = NULL
    g0_jackknife_fraction = 0.1

    data("cnp_1986_2020_10km")
    data("ccs_1986_2020_10km")

    cruzes <- list(cnp_1986_2020_10km,
                   ccs_1986_2020_10km)

    cruz10 <- cruz_combine(cruzes)

    # density_segments =========================================================

    cruz$strata
    cruzi <- filter_cruz2(cruz = cruz,
                          analysis_only = TRUE,
                          years = 2017,
                          cruises = c(1705, 1706),
                          regions = 'HI_EEZ',
                          bft_range = 0:6,
                          eff_types = 'S',
                          on_off = TRUE)
    # simplify_strata
    cruzi$cohorts$most$segments$stratum <- 'HI-EEZ'
    density_segments <- cruzi$cohorts$most$segments
    density_das <- cruz$cohorts$most$das

    # density_sightings ========================================================

    density_sightings  <-
      cruz$cohorts$most$subgroups$subgroups %>%
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
    toplot = TRUE
    verbose = TRUE
    density_bootstraps <- 10000

    cruz <- cruzi

    # try it ===================================================================
    lta_subgroup(cruz,
                 df_sits,
                 truncation_distance,
                 ss,
                 cruz10,
                 g0_spp,
                 g0_truncation,
                 g0_pool_bft,
                 g0_jackknife_fraction,
                 density_segments,
                 density_das,
                 density_sightings,
                 abundance_area,
                 iterations,
                 density_bootstraps,
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
      message('--- --- ',i,' :: re-sampled ESW estimate = ',round(eswi,3), ' km')
      esw_boots <- c(esw_boots, eswi)
      saveRDS(esw_boots, file=paste0(output_dir,'esw_boots.RData'))
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

  if(verbose){message('\n--- estimating Rg0 ...')}
  g0_result <-
    g0_model(
      spp = g0_spp,
      truncation_distance = g0_truncation,
      cruz = cruz10,
      pool_bft = g0_pool_bft,
      jackknife_fraction = g0_jackknife_fraction,
      toplot = toplot,
      verbose = verbose)

  saveRDS(g0_result, file=paste0(output_dir,'g0_result.RData'))

  ##############################################################################
  # Encounter rate

  if(verbose){message('\n--- estimating the encounter rate ...')}
  (n <- density_sightings %>% nrow)
  (L <- density_segments$dist %>% sum)
  er_estimate <- n/L

  er_boots <- c()
  i = 1
  for(i in 1:iterations){
    boot_data <- prep_bootstrap_datasets(segments = density_segments,
                                         sightings = density_sightings)
    (Li <- boot_data$segments$dist %>% sum)
    (ni <- boot_data$sightings %>% nrow)
    (eri <- ni / Li)
    message('--- --- ',i,' :: re-sampled encounter rate estimate = ',round(eri*100,3), ' subgroups / 100 km2')
    er_boots <- c(er_boots, eri)
    saveRDS(er_boots, file=paste0('er_boots.RData'))
  }

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
  (g0_param <- g0_optimize(g0_wt_mn, g0_wt_cv, try_count = 20, verbose = TRUE)$bestFit)
  g0_boots <- plogis(rnorm(5000,g0_param[1],g0_param[2]))
  if(toplot){hist(g0_boots)}

  ##############################################################################
  # Density estimation

  # Get estimate
  if(verbose){message('\n--- estimating density ...')}

  (D <- er_estimate * (ss_estimate / (2 * esw * g0_wt_mn)))
  D * 100

  # Prep boot strap data
  if(verbose){message('--- --- bootstrapping ...')}
  its <- data.frame(er = er_boots, ss = ss_boots, esw = esw_boots, g0 = g0_boots)
  head(its)

  # Bootstrap
  its <- its[sample(1:nrow(its), size= density_bootstraps, replace=TRUE),]

  its$D <- its$er * (its$ss / (2 * its$esw * its$g0))
  if(toplot){hist(its$D)}

  ##############################################################################
  # Abundance estimation

  its$N <- NA
  if(!is.null(abundance_area)){
    if(verbose){message('\n--- estimating abundance ...')}
    N <- round(D * abundance_area) %>% as.numeric
    its$N <- round(its$D * abundance_area) %>% as.numeric
  }

  ##############################################################################
  # Outputs

  if(verbose){message('\n--- compiling results ...')}

  results <- list(D = D,
                  D_CV = sd(its$D) / D,
                  D_L95 = coxed::bca(its$D)[1],
                  D_U95 = coxed::bca(its$D)[2],
                  N = N,
                  N_CV = sd(its$N) / N,
                  N_L95 = coxed::bca(its$N)[1],
                  N_U95 = coxed::bca(its$N)[2],
                  ER = er_estimate,
                  ss = ss_estimate,
                  n = nrow(density_sightings), L = L,
                  n_segments = nrow(density_segments),
                  g0 = g0_wt_mn, g0_cv = g0_wt_cv,
                  g0_details = g0_result,
                  df = df,
                  bootstraps = list(esw = esw_boots,
                                    ss = ss_boots,
                                    g0 = g0_boots,
                                    er = er_boots,
                                    D = its$D,
                                    N = its$N),
                  iterations = iterations,
                  density_bootstraps = density_bootstraps)
  saveRDS(results, file=paste0(output_dir,'lta_subgroup_results.RData'))

  if(verbose){message('\nFinished!\n')}

  return(results)
}
