#' Estimate relative g(0) in different survey conditions
#'
#' This function is an implementation of Barlow (2015), "Inferring trackline detection
#' probabilities, g(0), for cetaceans from apparent densities in different survey
#' conditions" (*Marine Mammal Science*), for processed `Wincruz` survey data.
#' This function predicts the relative g(0) (compared to Beaufort sea state 0)
#' for all Beaufort sea states 0 - 6.
#'
#' @param spp A character vector of species code(s) whose relative trackline detection probability (g(0))
#' you want to estimate.
#'
#' @param truncation_distance The truncation distance, in km, to apply to sightings.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' Ensure that segment lengths are short; Barlow (2015) used 10km segments.
#' See using the built-in `LTabundR` dataset, `data(noaa_10km_1986_2020)`, if it applies to your study.
#'
#' @param eff_types Effort types to filter segments and sightings to before conducting analysis. The
#' default is systematic effort only (`"S"`). Can be `NULL`.
#'
#' @param cohort The cohort whose data pertains to the species of interest, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @param jackknife_fraction The proportion of data to leave out within each jackknife permutation,
#' which is used to calculate the CV of the *Rg(0)* estimates.
#' The default is 0.1 (i.e., 10% of the data, yielding 10 jackknife loops), after Barlow (2015).
#'
#' @param constrain_shape Some *Rg(0)* curves will not decline monotonically due to sample size
#' issues at low Bft (0-2) or high Bft (5-6) states. To coerce monotonic decline, set this to
#' `TRUE`, and the function will use a shape-constrained GAM (`scam()` from package `scam`) instead of a
#' classic `mgcv::gam()`.
#'
#' @param k Smoothing term for the Bft spline in the GAM. Default (and the value used in Barlow 2015) is 4.
#'
#' @param toplot Boolean, with default `TRUE`, indicating whether segment length histograms and detection function plots (`Distance::plot.ds()`)
#' should be displayed (during estimation of effective strip width).
#'
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.

#' @details After Barlow (2015), this function implements the following procedure:
#' \enumerate{
#' \item Filter sightings to the specified species code(s),
#' and associate those sightings with their respective 10-km segment ID.
#' \item Determine the presence or absence of the species of interest for each segment.
#' \item Estimate the Effective Search Area (ESA) for each Beaufort sea state represented in the data.
#' This is done by fitting a detection function to the sightings data (half-normal key; using function `Distance::ds()`),
#' using Beaufort as the only covariate. With this detection function model,
#' the probability of detection (`p(det)`) for sightings in each Beaufort state is determined (`Distance::predict.ds()$fitted`).
#' The effective strip width (ESW) for each Beaufort state is determined by multiplying `p(det)` by the supplied `truncation_distance`.
#' The ESW is used to calculate the ESA in each segment (ESA = 2 x L x ESW).
#' Note that Beaufort states 0 and 1 are combined (after Barlow 2015),
#' since they are typically under-represented in open-ocean fieldwork.
#' \item A binomial Generalized Additive Model (GAM) (`"logit"` link function)
#' is fit to the segments (using `mgcv::gam`), based upon Equation 3 from Barlow (2015),
#' which includes Beaufort sea state, Latitude x Longitude, and Year as predictors
#' (using the `mgcv::s()` spline function) and the log of ESA as an offset (`stats::offset()`)
#' to predict the probability of apparent presence in each segment.
#' To prevent over-fitting, the spline functions for Beaufort and Year are constrained with `k=4`,
#' and model complexity penalty is inflated (`gamma = 1.4` in `mgcv::gam()`).
#' \item The GAM model is used to predict the probability of apparent species presence (`mgcv::predict.gam()`) in
#' fake segements of equal length, year, and Latitude/Longitude but with different Beaufort sea states, 0 - 6.
#' \item By comparing the probability for Beaufort 0 (in which rates of apparent presence would be highest) to
#' that for other Beaufort states, the Relative g(0) (*Rg(0)*)is estimated (Equation 4 in Barlow 2015).
#' \item The Coefficient of Variation (CV) for *Rg(0)* is then estimated using a jackknife procedure,
#' in which a fraction of the data are sequentially removed (input `jackknife_fraction`, with default 10%),
#' *Rg(0)* is re-estimated by implementing steps 4 - 6 above, and this process is repeated for all sequential fractions of the data.
#' The CV is derived from these pseudo-estimates (let's call them `jacks`) with the equation `n*CV(jacks) - (n-1)*CV(jacks))`,
#' in which `n` is the number of jackknife estimates (`1 / jackknife_fraction`) and
#' `CV` is the standard deviation of jacks divided by their mean.
#' }
#'
#' @return A list:
#' \itemize{
#' \item `Rg0` a `data.frame` of estimates of Relative g(0) at each Beaufort.
#' \item `gam` the `mgcv::gam()` model output.
#' \item `jackknife` A list of jackknife results: `$g0` contains the g(0) estimates from
#' each jacknife iteration; `$ESW` contains the effective half-strip-width estimates; `$gams` contains the gam model objects.
#' \item `summary`: A dataframe summarizing results; **this is the primary output you are likely to use.**.
#' \item `cruz10`: A modified nascent `cruz` object that has been re-segmentized to have segments of 10km-length.
#' This usually takes a while to create, so this output gives you the option of passing this object
#' on to your next call of `g0_bft_model()` (see `cruz10` argument above) to save time.
#' }
#' @export
#'
g0_model <- function(spp,
                     truncation_distance = 5.5,
                     cruz,
                     cohort=1,
                     eff_types = 'S',
                     jackknife_fraction=0.1,
                     constrain_shape = FALSE,
                     k = 4,
                     toplot = TRUE,
                     verbose = TRUE){

  if(FALSE){ # not run -- for debugging only ===================================
    data('noaa_10km_1986_2020')
    cruz <- noaa_10km_1986_2020
    cruz <- filter_cruz(cruz, regions = 'CCS')
    spp <- '051' # mesoplodon
    spp <- '075' # blue
    spp <- '076' # humpback
    spp <- '071' # minke
    spp <- '046' # sperm
    spp <- '036' # pilot whale # usually needs pooling
    spp <- c('047', '048','080') # kogia
    cohort = 1
    eff_types = 'S'
    truncation_distance <- 4.0
    jackknife_fraction <- .1
    constrain_shape = TRUE
    k = 4
    verbose = TRUE
    toplot = TRUE
    loopi = 1

    # Try it
    mr <- g0_model(spp = spp,
                   cruz = cruz,
                   constrain_shape = constrain_shape,
                   k = k,
                   jackknife_fraction = 0)

    mr$summary
    mr$sightings$bft %>% table

  } # end debugging data =======================================================

  # Prep data ==================================================================

  # Filter down to the cohort specified
  survey <-  cruz$cohorts[[cohort]]
  names(survey)
  cohort_settings <- cruz$settings$cohorts[[cohort]]

  # Create segments df, filter to use == TRUE,
  if(verbose){message('--- Preparing segments ...')}
  segments <- survey$segments %>% dplyr::filter(use==TRUE)
  if(!is.null(eff_types)){
    segments <- segments %>% dplyr::filter(EffType %in% eff_types)
  }

  # Make sure only sightings for this species are kept
  # and format sightings for detection function fitting
  if(verbose){message('--- Preparing sightings ...')}
  sightings <<- NULL
  sightings <-
    survey$sightings %>%
    dplyr::mutate(bft = Bft) %>%
    dplyr::filter(species %in% spp,
                  !is.na(bft)) %>%
    dplyr::mutate(distance = PerpDistKm,
                  object = 1:dplyr::n(),
                  observer = 1,
                  detected = 1)
  if(!is.null(eff_types)){
    sightings <- sightings %>% dplyr::filter(EffType %in% eff_types)
  }
  sightings <<- sightings

  # Check it out (debugging)
  sightings$species %>% table
  sightings$bft %>% table(useNA='ifany')

  # Add to segments a column, ngrp, with number of groups seen on this segment
  if(verbose){message('--- Finding groups detected on each segment ...')}
  segments$ngrp <- sapply(segments$seg_id, function(x){
    matchi <- length(which(sightings$seg_id == x))
    return(matchi)
  })
  segments$ngrp %>% table
  segments %>% nrow

  # Add to segments a column, p, with presence/absence (1 / 0)
  if(verbose){message('--- Converting to presence/absence on each segment ...')}
  segments$p <- sapply(segments$seg_id, function(x){
    matchi <- length(which(sightings$seg_id == x))
    returni <- ifelse(matchi>0, 1, 0)
    return(returni)
  })
  segments$p %>% table
  head(segments)

  # Prepare jackknife version of the data, by randomizing order of rows
  new_order <- sample(1:nrow(segments),size=nrow(segments), replace=FALSE)
  jk_segments <- segments[new_order,]

  ############################################################################
  # ESTIMATE / JACKNIFE LOOPs
  ############################################################################

  RESULT <- list() # stage results

  # Loop through code to create estimate first, then jackknife (if user wants)
  # If jackknife_fraction is non-zero, the user wants to do jackknife estimation too
  loops <- c('estimate')
  if(is.null(jackknife_fraction)){jackknife_fraction <- 0}
  if(jackknife_fraction > 0){loops <- c(loops, 'jacknife') }
  loopi <- loops[1] # for debugging
  for(loopi in loops){
    # if this is just the estimate loop, niter is 1
    if(loopi == 'estimate'){
      niter <- 1
    }else{
      niter <- ceiling(1/jackknife_fraction)
    }

    # Stage data subsetting plan
    gams <- Rg0  <- list() # stage results
    if(loopi == 'estimate'){
      starts <- 1
    }else{
      # Prep jackknife plan
      starts <- seq(1, nrow(segments),length = ((1 / jackknife_fraction)+1)) %>% round
      (starts <- starts[1:(length(starts)-1)])
      nrow(segments)
    }

    # Implement jackknife loop (if 'estimate', starts is 1 and no data is removed)
    ji=1
    for(ji in 1:length(starts)){
      if(loopi != 'jacknife'){
        if(verbose){message('\n==============================================================\nSpecies ',paste(spp, collapse='-'),' :: Estimation round', '\n')} #\n==============================================================')}
      }else{
        if(verbose){message('\n==============================================================\nSpecies ',paste(spp, collapse='-'),' :: Jackknife round ',ji,'\n')} #,'\n==============================================================')}
      }

      # Determine segments to keep in this jacknife round
      starti <- starts[ji]
      if(ji != length(starts)){
        endi <- starts[ji+1] - 1
      }else{
        endi <- nrow(segments)
      }
      starti ; endi
      if(verbose){message('--- Start row: ',starti,' | End row: ',endi)}

      # Subset data accordingly (first segments, then sightings)
      segmenti <- segments
      if(loopi == 'jacknife'){
        segmenti <- jk_segments[-(starti:endi),] ; nrow(segmenti)
      }
      segmenti$seg_id
      sightings$seg_id

      # Now filter sightings
      sightingi <- sightings %>% dplyr::filter(seg_id %in% segmenti$seg_id)
      sightingi %>% nrow #%>% print
      if(verbose){message('--- Data used in iteration: ',nrow(segmenti),' segments  |  ',nrow(sightingi),' sightings ...')}

      # Calculate Effective Search Area by fitting detection function ==========
      if(verbose){message('--- Fitting detection function to estimate\n    Effective Search Area (ESA) for each segment ...')}

      suppressWarnings({ suppressMessages({
        dso <- mrds::ddf(dsmodel= ~mcds(key='hn', formula='~1 + bft'),
                         data=sightingi,
                         method="ds",
                         meta.data=list(width=truncation_distance))
      }) })
      if(toplot){plot(dso)}

      # Prepare GAM dataset  =====================================================

      if(verbose){message('--- Preparing datasets for GAM model fitting ...')}
      # Prepare segment both for GAM modeling and predicting ESW with Distance::ds object
      df <- segmenti %>%
        dplyr::mutate(distance = 1, # fake values for ds prediction
                      object = 1:n()) %>% # ditto
        dplyr::mutate(bft = (avgBft)) %>%
        dplyr::select(seg_id, ngrp, p, dist, mlat, mlon, year, bft, distance, object)
      df <- df[complete.cases(df),] # remove na's
      df %>% head
      df$dist %>% range

      # Predict effective strip half-width for each segment based on bft value
      df$esw <- predict(object = dso, newdata = df, esw = TRUE)$fitted %>% as.numeric
      #hist(df$esw, breaks=20) # debugging
      #ggplot(df, aes(x=bft, y=esw)) + geom_point()

      # Stage esw estimates for results
      eswi <- df %>%
        #dplyr::mutate(bftr = floor(bft)) %>%
        dplyr::mutate(bftr = round(bft)) %>%
        dplyr::group_by(bftr) %>%
        dplyr::summarize(esw = mean(esw)) %>%
        dplyr::rename(bft = bftr)
      eswi
      all_bft <- data.frame(bft = 0:6)
      eswi <- dplyr::left_join(all_bft, eswi, by='bft')
      eswi
      (ESWi <- data.frame(bft = 0:6, esw = eswi$esw))

      # Use esw to calculate effective area searched (ESA)
      df %>% head
      df$esa <- 2 * df$dist * df$esw
      df %>% head
      #ggplot(df, aes(x=bft, y=esw)) + geom_point()
      #ggplot(df, aes(x=bft, y=esa)) + geom_point()

      # Assess sample sizes
      nrow(df)
      df$ngrp %>% table
      df$p %>% table
      length(which(df$p > 0)) / nrow(df)

      # Fit GAM  =================================================================
      # Fit Binomial GAM model (logit link), with ESA as an offset
      # after Barlow 2015 equation 3
      if(verbose){ message('--- Fitting the GAM ...')}

      if(!constrain_shape){
        bm <- mgcv::gam(formula = as.logical(ngrp) ~ s(bft, k=k) +
                        s(mlat, mlon, bs='tp') + s(year, k=4) + offset(log(esa)),
                        family=binomial,
                        data=df,
                        gamma=1.4)
      }else{
        bm <- scam::scam(formula = as.logical(ngrp) ~ s(bft, bs='mpd', k=k) +
                         s(mlat, mlon, bs='tp') + s(year, k=4) +
                         offset(log(esa)),
                       family=binomial,
                       data=df,
                       not.exp=TRUE,
                       gamma=1.4)
      }

      # Calculate Relative G0  ===================================================

      if(verbose){ message('--- Predicting relative g(0) ...')}
      # Predict p at each bft using a fake dataset
      (df_new <- data.frame(bft = 0:6,
                            mlat = mean(segments$mlat, na.rm=TRUE),
                            mlon = mean(segments$mlon, na.rm=TRUE),
                            #mlat = 33,
                            #mlon = -120,
                            year = round(mean(segments$year, na.rm=TRUE)),
                            #year = 2000,
                            #esa = 1))
                            esa = mean(df$esa, na.rm=TRUE)))
      if(!constrain_shape){
        (ps <- mgcv::predict.gam(bm, newdata=df_new))
      }else{
        (ps <- scam::predict.scam(bm, newdata=df_new))
      }

      # Make sure these numbers aren't too small to exponentiate
      # If so, make them larger but keep relative values the same
      (min_diff <- max(ps) + 200)
      if(min_diff < -100){ps <- ps + abs(min_diff)}
      ps

      # Calculate relative g(0)
      #(pslogis <- plogis(ps) / plogis(ps[1]))
      #(ps <- plogis(ps) / plogis(ps[1]))
      (ps <- exp(ps) / exp(ps[1])) # OG code 2022
      #plot(ps, type='o', pch=16, lwd=2, ylim=c(0,1))

      # If relative g(0) does not decline at bft 7, force all g(0) to 1.
      if(ps[6] > ps[1]){ps[1:7] <- 1}

      # Setup results
      df_new <-
        df_new %>%
        dplyr::mutate(Rg0 = ps, ESW = ESWi$esw) %>%
        dplyr::select(bft, Rg0, ESW)
      df_new
      message('--- ',paste(round(df_new$Rg0,3),collapse=', '))

      if(loopi == 'estimate'){
        RESULT$Rg0 <- df_new

        RESULT$gam <- bm
      }
      if(loopi == 'jacknife'){
        gams[[length(gams)+1]] <- bm
        Rg0[[length(Rg0)+1]] <- df_new
        message('')
      }
    } # end of jackknife loop

    if(loopi == 'jacknife'){
      gams
      Rg0
      Rg0_ests <- data.frame(lapply(Rg0,'[[',2))
      names(Rg0_ests) <- paste0('jk',1:length(Rg0))
      row.names(Rg0_ests) <- paste0('bft_',0:6)

      ESW_ests <- data.frame(lapply(Rg0,'[[',3))
      names(ESW_ests) <- paste0('jk',1:length(Rg0))
      row.names(ESW_ests) <- paste0('bft_',0:6)
      ESW_ests
      RESULT$jackknife <- list(g0 = Rg0_ests,
                               ESW = ESW_ests,
                               gams = gams)
    } # end of jacknife summary
  } # end of estimate / jacknife toggle loop

  # Jackknife summaries =========================================================

  (theta <- RESULT$Rg0) # estimate
  (jkg0 <- RESULT$jackknife$g0) # jacknife estimates of g0
  (jkesw <- RESULT$jackknife$ESW) # jacknife estimates of ESW

  # Calculate jackknife varianceand CV
  jk_g0_var <- rep(0, 7)
  jk_esw_var <- rep(0, 7)
  i=2
  for(i in 1:7){
    (g0i <- jkg0[i,] %>% as.numeric)
    (eswi <- jkesw[i,] %>% as.numeric)

    (g0mean <- g0i %>% mean(na.rm=TRUE))
    (eswmean <- eswi %>% mean(na.rm=TRUE))

    jk_g0_var[i] <- sum( (g0i - g0mean)^2 * (length(g0i)-1)/(length(g0i)) )
    jk_esw_var[i] <- sum( (eswi - eswmean)^2 * (length(eswi)-1)/(length(eswi)) )
  }

  # Review (debugging)
  jk_g0_var
  jk_esw_var

  # Calculate standard error
  (jk_g0_se <- sqrt(jk_g0_var))
  (jk_esw_se <- sqrt(jk_esw_var))

  # Calculate CV
  (g0_cv <- jk_g0_se / theta$Rg0)

  # Prepare summary values
  theta$n <- ncol(jkg0)
  theta$Rg0_SE <- jk_g0_se
  theta$Rg0_CV <- g0_cv
  theta$ESW_SE <- jk_esw_se
  theta

  # Plot result
  if(toplot){
    par(mar=c(4.2,4.2,2,.5))
    plot(Rg0 ~ bft, data = theta, type='o', pch=16, lwd=2, ylim=c(0,1), col='white',
         main = paste(spp, collapse=', '),
         xlab='Beaufort Sea State',
         ylab='Relative g(0)')
    abline(h=seq(0,1,by=.1), col='grey90')
    lines(theta$Rg0 ~ theta$bft, type='o', pch=16, lwd=2)
  }

  RESULT$summary <- theta
  RESULT$sightings <- sightings
  RESULT$segments <- segments

  return(RESULT)
} # end of function

