#' Encounter Rate simulator test
#'
#' Test for the probability that year-to-year changes observed in a species' encounter rate
#' are due to random sampling variation instead of an actual change in the encounter rate.
#' This function uses bootstrap sampling of survey segments to see if random variation in
#' sampling could possibly produce an *apparent* but immaterial change in encounter rate across years.

#' @param spp Species code
#' @param cohort The cohort whose data you would like to analyze, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param iterations Number of iterations
#'
#' @details See the Appendix to Bradford et al. (2020) for analytical details, but briefly:
#' in each bootstrap iteration, survey segments are resampled in a way that preserves
#' the proportion of effort occurring within each geostratum in the data.
#' The resampled data are used to calculate the overall encounter rate across all years,
#' since the null hypothesis is that the encounter rate does not change across years.
#' This overall encounter rate is used to predict the number of sightings in each year,
#' based on the distance covered by the resampled segments in each year.
#' This process is repeated (`iterations` times) to produce a distribution of predicted sighting counts
#' in each year. This distribution is compared to the actual number of sightings observed in each year.
#' The number of simulated sightings counts that exceed the observed count reflects
#' the probability that the observed count is due to random sample variation alone.
#'
#' @return A dataframe with a row for each year. Columns provide the number of observations of
#' the species of interest during systematic effort, and the p-value of the test.
#' The p-value represents the fraction of simulated encounter rates that exceed the observed encounter rate.
#'
#' @export
#'
er_simulator <- function(spp,
                         cohort = 1,
                         cruz,
                         iterations = 1000){

  if(FALSE){ #==================================================================

    spp = '072' # brydes
    cohort = 1
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cruz <- filter_cruz(cruz, years= c(2002, 2010, 2017))
    iterations = 10

    # try it
    er_simulator(spp = '072',
                 years = c(2002, 2010, 2017),
                 cruz = cruz,
                 iterations = 10)

  } #===========================================================================

  # Get data from the cruz object
  segments <- cruz$cohorts[[cohort]]$segments
  sightings <-
    cruz$cohorts[[cohort]]$sightings %>%
    filter(species %in% spp)
  das <- cruz$cohorts[[cohort]]$das
  years <- unique(das$year)

  # Review overall ER (debugging)
  nrow(sightings) / sum(segments$dist)

  # Get observed sightings per year ============================================

  obsi <- c()
  yi <- 1
  for(yi in 1:length(years)){
    (yeari <- years[yi])
    obsi[yi] <-
      sightings %>%
      dplyr::filter(year == yeari) %>%
      nrow()
  }
  obsi

  # Bootstrap iterations =======================================================

  bsits <- matrix()
  bi = 1
  for(bi in 1:iterations){
    message('Iteration ', bi, ' ...')

    # Simulate the overall encounter rate w bootstrap resampling
    kms <- c()
    sits <- c()
    yi <- 1
    for(yi in 1:length(years)){
      (yeari <- years[yi])
      message('--- --- ',yeari)
      # Resample the data
      segi <- segments %>% dplyr::filter(year == yeari)
      siti <- sightings %>% dplyr::filter(year == yeari)
      bs <- prep_bootstrap_datasets(segi, siti)
      # Save km surveyed and sightings
      kms[yi] <- bs$segments$dist %>% sum
      sits[yi] <- bs$sightings %>% nrow
    }

    # Determing the overall encounter rate of the bootstrapped sample (across all years)
    (er_sim_overall <- sum(sits) / sum(kms))

    # Predict the sightings in each year based on the null assumption of
    # no overall change in encounter rate
    (sim_sits <- kms * er_sim_overall)

    # Add to growing list of simulated predictions
    if(bi==1){
      bsits <- sim_sits
    }else{
      (bsits <- cbind(bsits, sim_sits))
    }
  }

  # Plot results ===============================================================

  par(mfrow=c(length(years),1))
  (xmax <- 1.1*max(c(bsits, obsi)))
  i=1
  pvals <- c()
  for(i in 1:length(years)){

    # Plot
    main_title <- paste0(paste('Species ',spp,collapse='-'),' :: ',years[i])
    hist(bsits[i,],
       xlim=c(0, xmax),
       breaks= seq(0, xmax, length=20),
       col = 'grey60',
       border = 'grey85',
       main = years[i], #main_title,
       xlab = 'Simulated sightings')
    abline(v=obsi[i], col='red', lwd=2)

    pvals[i] <- (which(bsits[i,] > obsi[i]) %>% length) / iterations
  }
  par(mfrow=c(1,1))

  # Compile results into a list
  resulti <- list(summary = data.frame(years, observed = obsi, p = pvals),
                  details = bsits)

  return(resulti)
}
