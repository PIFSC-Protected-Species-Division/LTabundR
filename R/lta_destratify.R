#' Pool stratified LTA results
#'
#' Loop through a list of stratified line-transect-analysis results
#' and "destratify" -- i.e., pool -- them into a single estimate.
#'
#' @param lta_list A `list` of line-transect-analysis results produced from
#' `LTabundR::lta()` and compiled into a `list` with `LTabundR::lta_enlist()`.
#'
#' @param years A numeric vector of year(s) in which you want to pool all region-stratified
#' results into a single annual estimate.
#'
#' @param combine_method A character string indicating the means by which estimates
#' of the CV and 95% confidence interval of abundance will be destratified.
#' The two recognized options are `"arithmetic"` (the default) and `"bootstrap"`.
#' See details below.
#'
#' @param new_region A name for the pooled geostratum represented by the pooled estimate.
#' If this is not provided, a name will be generated from the geostrata that are pooled.
#'
#' @param verbose A Boolean, with default `TRUE`, indicating whether or not status
#' updates should be printed to the Console.
#'
#' @return A modified `lta_list`, in which stratified estimates have been pooled into
#' single annual estimates for relevant years.
#'
#' @details Stratified estimates of parameter means are pooled using a weighted mean
#' in which the area of each geostratum is used as the weight. Stratified estimates of
#' abundance variance terms (SD, CV, and 95% confidence interval) are pooled either
#' arithmetically -- using standard equations to pool estimates one at a time --
#' or iteratively -- using the bootstrap results contained within the `lta_list`.
#' The iterative technique resamples the abundance bootstrap estimates from each
#' geostratum 10,000 times then calculates SD, CV, and 95% confidence interval from the
#' resulting distribution. Some variance terms, e.g., the SD of school size and the CV of g(0),
#' are not destratified in the current version and are instead replaced with `NA`.
#'
#' @export
#' @import dplyr
#'
lta_destratify <- function(lta_list,
                           years,
                           combine_method = 'arithmetic',
                           new_region = NULL,
                           verbose = TRUE){

  if(FALSE){ # not run -- debugging only =========================================
    # To debug this code, you need lta results with stratified effort, e.g., CNP 2002.
    # See the vignette chapter for example code:
    # https://emk-noaa.github.io/LTAvignette/destratify.html

    #lta_list <- results
    years <- 2002
    combine_method = 'arithmetic'
    new_region <- '(HI-EEZ)'
    verbose = TRUE
  } #=============================================================================

  # Review input for debugging
  lapply(lta_list,'[[','pool')

  # Loop through each lta result, destratify each individually
  if(verbose){message('Destratifying each LTA list ...')}
  li <- 11 # for debugging
  for(li in 1:length(lta_list)){
    lti <- lta_list[[li]]
    if(verbose){message('--- LTA object ',li,' :: species pool ',lti$pool,' ...')}

    # review
    lti %>% names
    # these are the 2 slots that will need updating
    lti$estimate
    lti$bootstrap$summary

    # Stage replacement objects
    lti_new <- lti
    esti <- bsummi <- data.frame()

    # Loop through each title in this lti object
    (titles <- lti$estimate$title %>% unique)
    ti=2
    for(ti in 1:length(titles)){
      (titli <- titles[ti])
      if(verbose){message('--- --- estimate title: ',titli)}

      # Filter datasets to this title
      (estt <- lti$estimate %>% dplyr::filter(title == titli))
      (bsummt <- lti$bootstrap$summary %>% dplyr::filter(title == titli))
      (bsdett <- lti$bootstrap$details %>% dplyr::filter(title == titli))

      # Set aside years not relevant to requested destratification
      (esty_other <- estt %>% dplyr::filter(! year %in% years))
      (bsummy_other <- bsummt %>% dplyr::filter(! year %in% years))
      (bsdeti_other <- bsdett %>% dplyr::filter(! year %in% years))

      # Now filter to the years you want to destratify
      (esty <- estt %>% dplyr::filter(year %in% years))
      (bsummy <- bsummt %>% dplyr::filter(year %in% years))
      (bsdety <- bsdett %>% dplyr::filter(year %in% years))

      # If there are years to process, loop through them
      (yearsi <- esty$year %>% unique %>% as.numeric)
      if(length(yearsi)>0){
        yi <- 1
        for(yi in 1:length(yearsi)){
          (yeari <- yearsi[yi])
          if(verbose){message('--- --- --- estimate year: ',yeari)}

          (estyi <- esty %>% dplyr::filter(year %in% yeari))
          (bsummyi <- bsummy %>% dplyr::filter(year %in% yeari))
          (bsdetyi <- bsdety %>% dplyr::filter(year %in% yeari))

          # Check if destratification is actually necessary
          if(nrow(estyi)>1){
            if(verbose){message('--- --- --- --- multiple regions found. Destratifying ...')}

            # Join area to bootstraps
            (estyi_join <- estyi %>% dplyr::select(Region, Area))
            bsummyi <- left_join(bsummyi, estyi_join, by='Region')
            bsdetyi <- left_join(bsdetyi, estyi_join, by='Region')

            # Get average/sum of estimates for the regions, weighting by area when appropriate
            estyi
            this_region <- new_region
            if(is.null(this_region)){this_region <- paste(estyi$Region, collapse=' + ')}

            estii <- estyi[1,]
            estii$Region <- this_region
            estii$Area = sum(estyi$Area)
            estii$segments <- sum(estyi$segments)
            estii$km <- sum(estyi$km)
            estii$Area_covered = sum(estyi$Area_covered)
            estii$g0_est <- stats::weighted.mean(estyi$g0_est, estyi$Area)
            estii$ER_clusters <- stats::weighted.mean(estyi$ER_clusters, estyi$Area)
            estii$D_clusters <- stats::weighted.mean(estyi$D_clusters, estyi$Area)
            estii$N_clusters <- sum(estyi$N_clusters)
            estii$size_mean <- stats::weighted.mean(estyi$size_mean, estyi$Area)
            estii$ER <- stats::weighted.mean(estyi$ER, estyi$Area)
            estii$D <- stats::weighted.mean(estyi$D, estyi$Area)
            estii$N <- sum(estyi$N)
            estii

            # Destratify bootstrap summary
            bsummyi %>% as.data.frame
            bsummii <- bsummyi[1,]
            bsummii$Region <- this_region
            bsummii$ESW_mean <- stats::weighted.mean(bsummyi$ESW_mean, bsummyi$Area)
            bsummii$g0_mean <- stats::weighted.mean(bsummyi$g0_mean, bsummyi$Area)
            bsummii$km <- sum(bsummyi$km)
            bsummii$ER <- stats::weighted.mean(bsummyi$ER, bsummyi$Area)
            bsummii$D <- stats::weighted.mean(bsummyi$D, bsummyi$Area)
            bsummii$size <- stats::weighted.mean(bsummyi$size, bsummyi$Area)
            bsummii$Nmean <- sum(bsummyi$Nmean)
            bsummii$Nmedian <- sum(bsummyi$Nmedian)

            # Parameters that can't be combined
            estii$size_sd <- NA
            bsummii$g0_cv <- NA

            # Parameters that need to be combined
            bsummii$Nsd <- NA
            bsummii$CV <- NA
            bsummii$L95 <- NA
            bsummii$U95 <- NA

            estii %>% as.data.frame
            bsummii %>% as.data.frame

            if(combine_method == 'arithmetic'){
              #=================================================================
              # Method 1: simple arithmentic (adapted from Bradford et al 2021 Excel)

              if(verbose){message('--- --- --- --- calculating N dispersion arithmetically ...')}

              # Destratify N CV
              (mn1  <- estyi$N[1])
              (cv1 <- bsummyi$CV[1])
              # Combine estimates one region at a time
              ci <- 2
              for(ci in 2:nrow(estyi)){
                (mn2  <- estyi$N[ci])
                (cv2 <- bsummyi$CV[ci])

                (mni <- mn1 + mn2)
                (cvi <- sqrt((cv1*mn1)^2 + (cv2*mn2)^2) / mni)

                mn1 <- mni
                cv1 <- cvi
              }
              mn1
              cv1

              # Fill in destratified fields
              bsummii$Nsd <- cv1*mn1
              bsummii$CV <- cv1
              (bsummii$L95 <- round(mn1 / exp(qnorm(1-(1-0.95)/2)*sqrt(log(1+(cv1*cv1))))))
              (bsummii$U95 <- round(mn1 * exp(qnorm(1-(1-0.95)/2)*sqrt(log(1+(cv1*cv1))))))

            }else{

              #=================================================================
              # Method 2: Bootstrap-based destratification

              if(combine_method == 'bootstrap'){
                if(verbose){message('--- --- --- --- calculating N dispersion iteratively ...')}
                # Destratify N
                ns <-
                  bsdetyi %>%
                  dplyr::group_by(Region) %>%
                  dplyr::slice_sample(n=10000, replace=TRUE) %>%
                  dplyr::group_split() %>%
                  lapply(function(x){x %>% dplyr::pull(N)}) %>%
                  Reduce(f = '+')
                (N_sd <- ns %>% sd)
                (N_lci <- coxed::bca(ns)[1])
                (N_uci <- coxed::bca(ns)[2])

                # Complete destratification
                bsummii$Nsd <- N_sd
                bsummii$CV <- N_sd / bsummii$Nmean
                bsummii$L95 <- N_lci
                bsummii$U95 <- N_uci

              }else{
                stop('Wait! The combine_method you provided was not recognized.')
              }
            }

            estii %>% as.data.frame
            bsummii %>% as.data.frame

          }else{
            if(verbose){message('--- --- --- --- only one region found. Destratification not needed.')}
            # There is only one region, so no destratification necessary.

            estii <- estyi
            bsummii <- bsummyi
          } # end of ifelse only 1 region

          # Add to growing datasets
          esti <- rbind(esti, estii)
          bsummi <- rbind(bsummi, bsummii)

        } # end of year loop
      }else{ # end of if there are years to process
        if(verbose){message('--- --- --- no estimate in the year(s) of interest. Skipping.')}
      }

      # Add back any years that did not need processing
      esti <- rbind(esti, esty_other)
      bsummi <- rbind(bsummi, bsummy_other)
    } # end of titles loop

    esti
    bsummi

    # Replace lti contents with new destratified version
    lti$estimate <- esti
    lti$bootstrap$summary <- bsummi
    lti$estimate

    # Replace lti object
    lta_list[[li]] <- lti
    if(verbose){message('\n')}

  } # end of lta list loop

  return(lta_list)
}
