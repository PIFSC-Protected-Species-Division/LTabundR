#' Compile table of Relative g(0) estimates by Beaufort state
#'
#' This function estimates relative trackline probabilities, Rg(0),
#' for a set of species and returns a single table (a `data.frame`) with
#' the results for each species. This function is a wrapper for `LTabundR::g0_model()`.
#' See its documentation for details on the procedure.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' Ensure that segment lengths are short; Barlow (2015) used 10km segments.
#' @param species A list of sublists, in which each sublist details the settings for
#' a single species (or related group of species). Each sublist needs to have these slots:
#' \itemize{
#' \item `spp`: A character vector of species code(s) whose relative trackline detection probability (g(0))
#' you want to estimate.
#' \item `title`: A unique title, such as a common name, scientific name, or genus,
#' to easily summarize this species group.
#' \item `cohort`: The cohort whose data pertains to the species of interest,
#' provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' If not provided, assumed to be 1.
#' \item `truncation`: The truncation distance, in km, to apply to sightings.
#' \item `constrain_shape` Some *Rg(0)* curves will not decline monotonically due to sample size
#' issues at low Bft (0-2) or high Bft (5-6) states. To coerce monotonic decline, set this to
#' `TRUE`, and the function will use a shape-constrained GAM (`scam()` from package `scam`) instead of a
#' classic `mgcv::gam()`.
#' \item k Smoothing term for the Bft spline in the GAM. Default (and the value used in Barlow 2015) is 4.
#' \item `regions`: A way to specify that `cruz` data should be filtered to a certain
#' geostratum region before conducting the analysis. If not `NULL`, this input must match
#' one of the geostratum names in `cruz$strata`.
#' }
#' @param eff_types Effort types to filter segments and sightings to before conducting analysis. The
#' default is systematic effort only (`"S"`). Can be `NULL`.
#' @param jackknife_fraction The proportion of data to leave out within each jackknife permutation,
#' which is used for estimating the CV of *Rg(0)* estimates.
#' The default is 0.1 (i.e., 10% of the data, yielding 10 jackknife loops), after Barlow (2015).
#' @param seed Set a seed (any integer) to ensure that the result is reproducible.
#' If left `NULL`, the results are liable to differ for each run of this function.
#' @param toplot Boolean, with default `TRUE`, indicating whether segment length histograms and detection function plots (`Distance::plot.ds()`)
#' should be displayed (during estimation of effective strip width).
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A `data.frame`, in which every row details the Rg0 estimate for a single species group
#' in a single Beaufort sea state, with the following columns:
#' \enumerate{
#' \item `title`: The title given to the species group.
#' \item `spp`: Species codes, concatenated by a hyphen if there are multiple in the species group.
#' \item `bft`: Beaufort sea state.
#' \item `Rg0`: Estimate of the relative g(0) for this sea state.
#' \item `ESW`: Estimate of the effective strip half-width (ESW) for this sea state.
#' \item `Rg0_SE`: The standard error of the Rg(0) estimate, non-zero only if `jackknife_fraction` was used.
#' \item `Rg0_CV`: The CV of the Rg(0) estimate, non-zero only if `jackknife_fraction` was used.
#' \item `ESW_SE`: The standard error of the ESW estimate, non-zero only if `jackknife_fraction` was used.
#' \item `sits`: The number of sightings within this sea state used to model Rg(0).
#' \item `sits_p`: The proportion of sightings within this sea state.
#' \item `segs`: The number of segments within this sea state used to model Rg(0).
#' \item `segs_p`: The proportion of segments within this sea state.
#' }
#' See the built-in dataset, `data(g0_results)`, for an example of the output.
#'
#' @import dplyr
#'
#' @export
#'
g0_table <- function(cruz,
                     species,
                     eff_types = 'S',
                     jackknife_fraction = NULL,
                     seed=NULL,
                     toplot = TRUE,
                     verbose = TRUE){

  if(FALSE){ # debugging only - not run ========================================
    data("noaa_10km_1986_2020")
    cruz <- noaa_10km_1986_2020
    eff_types = 'S'
    jackknife_fraction = NULL

    species <- list(
      list(spp = '046',
           title = 'Sperm whale',
           truncation = 5,
           pool_bft = NULL,
           regions = NULL))

  } #===========================================================================

  # Stage results dataframe
  result <- data.frame()

  # Loop through each estimate
  i=1
  for(i in 1:length(species)){
    (df <- species[[i]])
    (spp_title <- df$title)
    message('\n**************************************************************
**************************************************************\n',
            spp_title,
            '\n**************************************************************
**************************************************************\n')
    (sppi <- df$spp)
    (tdi <- df$truncation %>% as.numeric)
    cohorti <- 1
    if(!is.null(df$cohort)){cohorti <- df$cohort}

    # Subsetting as needed
    regioni <- NULL
    (regioni <- df$regions)
    cruzii <- cruz
    if(!is.null(regioni)){
      cruzii <- filter_cruz(cruz, regions = regioni)
    }

    # Set up constraint shape parameters
    constrain_shapi <- df$constrain_shape
    if(is.null(constrain_shapi)){constrain_shapi <- FALSE}
    ki <- df$k
    if(is.null(ki)){ki <- 4}

    # Carry out Rg0 estimation
    seediter <- NULL
    if(!is.null(seed)){seediter <- seed + i}
    rg0_sp  <- g0_model(spp = sppi,
                        truncation_distance = tdi,
                        cruz = cruzii,
                        eff_types = eff_types,
                        cohort = cohorti,
                        constrain_shape = constrain_shapi,
                        k=ki,
                        jackknife_fraction = jackknife_fraction,
                        seed=seediter)

    # Look at GAM object (but do nothing with it for now)
    (gami <- rg0_sp$gam %>% summary)

    # Summarize sample sizes
    (summi <- rg0_sp$summary)

    summi$sits <-
      sapply(0:6, function(x){
        rg0_sp$sightings %>%
          summarize(sits = length(which(bft == x))) %>%
          as.numeric
      })
    summi$sits_p <- summi$sits / sum(summi$sits)

    summi$segs <-
      sapply(0:6, function(x){
        rg0_sp$segments %>%
          summarize(segs = length(which(round(avgBft) == x))) %>%
          as.numeric
      })
    summi$segs_p <- summi$segs / sum(summi$segs)

    # Review
    resulti <- data.frame(title = spp_title, spp = paste(sppi, collapse='-'), summi)
    resulti

    # Update master results with this set of results
    result <- rbind(result, resulti)

  } # end of species estimate loop

  return(result)
}
