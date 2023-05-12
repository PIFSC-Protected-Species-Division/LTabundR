#' Build LTA tables for standard reports
#'
#' This function formats the results from an LTA analysis (`LTabundR::lta()`) into
#' that expected for standard tables in NOAA stock assessment reports (Tables 1, 2, 3, and 4, as well as appendix tables).
#'
#' @param lta_result An object holding the result of `LTabundR::lta()` or `LTabundR::lta_enlist()`.
#' @param cruz The cruz object (produced from LTabundR::process_surveys())
#' that was passed to `lta()` to produce `lta_result`.
#' This is optional; if not supplied, only part of Table 1 will be able to be filled in
#' and `tableA2` will not be provided (see `Value` below).
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A list with five slots:
#' \enumerate{
#' \item `table1a`: Sighting counts for all species in the `cruz` dataset for the years in which estimates were prepared.
#' If the `cruz` argument was `NULL`, this slot is also `NULL`. If not, a count of all sightings and systematic-only sightings
#' (i.e., `EffType = "S"` and `Bft <= 6`) are given for each species-year-region in the `cruz` data.
#' These counts are provided separately from the `$table1b` slot below, since those counts are based on the
#' `lta_result` object, and will not include sightings for species that did not have a specific LTA estimate
#' specified when it was made. We also include this separately so as to give the user full flexibility in how they summarize
#' sighting counts by region/population/stock.
#' \item `table1b`: Sighting counts used in estimates of density/abundance. Columns are prepared
#' for total sightings (Ntot) and systematic sightings (Nsys), but they are left blank, since it
#' is not clear how sightings from multiple regions in `$table1a` would be concatenated for this table,
#' since that involves stock-level designations.
#' The user can fill in those gaps accordingly.
#' \item `table2`: Sample sizes and truncation distances for detection functions
#' modeled by using pooled species/sightings
#' \item `table3`: Line-transect parameter estimates (mean Effective Strip half-Width, ESW;
#' mean school size; trackline detection probability, g(0); and its CV).
#' \item `table4`: Density and abundance estimates.
#' \item `tableA1` : Appendix table with study areas for each geostratum/cohort/year.
#' \item `tableA2`: A `list` in which each slot is a `data.frame` with Effort & Beaufort allocation,
#' parsed by geostratum, for each year. If the `cruz` argument was `NULL`, this slot is also `NULL`.
#' }
#' @import dplyr
#' @export
#'
lta_report <- function(lta_result,
                       cruz = NULL,
                       verbose=TRUE){

  if(FALSE){# debugging materials -- not run!  ================================
    data(lta_result)
    lta_result %>% names
    data(cnp_150km_1986_2020)
    cruz <- cnp_150km_1986_2020
    #cruz <- NULL
    verbose = TRUE

    # Try it
    x <- lta_result %>% lta_report(cruz)
    x <- lta_report(ltas, cruz)
    x$table1a %>% View
  } # end debugging staging area  ==============================================

  lta_result %>% names
  #lta_result$df %>% names

  ##############################################################################
  # Check to see if this is multi-stock results
  # if so, modify format

  # debugging
  is.null(names(lta_result))

  if(is.null(names(lta_result))){
    # if this list is un-named, it is a multi-stock results object
    # so reformat it:
    best_models <- data.frame()
    sample_size <- data.frame()
    estimates <- data.frame()
    bootstraps <- data.frame()
    for(i in 1:length(lta_result)){
      mri <- lta_result[[i]]
      best_models <- rbind(best_models, mri$df$best_models)
      sample_size <- rbind(sample_size, mri$df$sample_size)
      estimates <- rbind(estimates, mri$estimate)
      bootstraps <- rbind(bootstraps, mri$bootstrap$summary)
    }
    best_models
    sample_size
    estimates
    bootstraps

    lta_result <- list(df=list(best_models=best_models,
                               sample_size = sample_size),
                       estimate = estimates,
                       bootstrap=list(summary=bootstraps))
  }

  ##############################################################################
  # Table 1a in standard reports
  # Sample sizes -- total and systematic -- for all species in data
  # Only possible if cruz is provided

  table1a <- NULL
  if(!is.null(cruz)){
    if(verbose){message('--- Preparing Table 1a (total & systematic sighting totals) ...')}

    suppressMessages({
      (estimates <- lta_result$estimate)
      (estimate_years <- estimates$year %>% unique %>% as.numeric %>% sort)

      # Loop through each cohort in cruz data
      # Get sample sizes of each species, Ntot and Nsys
      ci = 1
      sits <- data.frame() # stage results object
      for(ci in 1:length(cruz$cohorts)){
        siti <- cruz$cohorts[[ci]]$sightings
        siti <- siti %>%
          dplyr::filter(year %in% estimate_years) %>%
          dplyr::group_by(species, stratum, year) %>%
          dplyr::summarize(Ntot = dplyr::n(),
                           Nsys = length(which(EffType == 'S' & Bft <= 6))) %>%
          dplyr::mutate(cohort = ci)
        siti
        sits <- rbind(sits, siti)
      }

      sits <-
        sits %>%
        select(species, cohort, stratum, year, Ntot, Nsys) %>%
        arrange(species, cohort, stratum, year)

      sits

      # Only keep data from latest cohort for each species
      #sits <- sits %>%
      #  dplyr::group_by(species) %>%
        #mutate(nmax = max(Ntot)) %>%
        #mutate(cohortmax = cohort[which(Ntot == nmax)[1]]) %>%
        #dplyr::filter(cohort == cohortmax) %>%
        #dplyr::ungroup() %>%
        #dplyr::select(- cohort, - nmax, - cohortmax)
      #sits

      # Pivot & arrange
      suppressWarnings({
        sits <-
          sits %>%
          tidyr::pivot_wider(id_cols = species:stratum,
                             names_from = year,
                             values_from = Ntot:Nsys,
                             names_vary = 'slowest',
                             values_fill = 0) %>%
          dplyr::arrange(as.numeric(substr(species, 1, 3)))
      })

      # Final formatting
      table1a <- dplyr::tibble(sits)
      table1a
    })
  }

  ##############################################################################
  # Table 2 in standard reports
  # Detection functions modeled by using pooled sightings

  if(verbose){message('--- Preparing Table 2 (detection functions for species pools) ....')}

  # Get relevant datasets
  (best_models <- lta_result$df$best_models)
  (sample_size <- lta_result$df$sample_size)

  # Stage result
  table2 <- tibble()

  # Loop through each pool
  (pools <- best_models$pool %>% unique)
  poi <- 1
  for(poi in 1:length(pools)){
    (pooli <- pools[poi])
    (best_modeli <- best_models %>% dplyr::filter(pool == pooli))
    (sampi <- sample_size %>% dplyr::filter(pool == pooli))

    # Stage best-fit formula
    (bestform <- best_modeli$Formula %>% unique)
    bfi <- 2 ; alt_terms <- c()
    if(length(bestform)>1){
      for(bfi in 2:length(bestform)){
        (formi <- bestform[bfi])

        # Handle equally best-fitting covariates
        (splits <- strsplit(formi,' ')[[1]])
        (alt_term <- paste0('(+',splits[length(splits)],')'))
        alt_terms <- c(alt_terms, alt_term)
      }
      (alt_terms <- paste(alt_terms, collapse=''))
    }
    (bestform <- paste0(bestform[1],alt_terms))

    # Initiate table for pool
    (table2i <- tibble(`Detection function` = best_modeli$pool[1],
                       Ntot = sampi$Ntot %>% sum,
                       Ndet = sampi$Ndet %>% sum,
                       TD = sampi$TD[1],
                       `Covariates tested` = best_modeli$`Covariates tested`,
                       `Best-fit model` = bestform))
    table2i <- table2i[1,]

    # Add row for each species in the pool
    sppi <- 1
    for(sppi in 1:nrow(sampi)){
      (sizi <- sampi[sppi,])
      # Format species name, if not Other
      if(sizi$species == 'Other'){
        sppnami <- 'Other'
      }else{
        # Attempt species translator function, using built-in data(species_codes)
        sppnami <- species_translator(sizi$species)$common_name1[1]
        if(length(sppnami)==0){
          sppnami <- sizi$species
        }
      }
      # Create row:
      (tabspp <- tibble(`Detection function` = paste0('       ',sppnami),
                        Ntot = sizi$Ntot,
                        Ndet = sizi$Ndet,
                        TD = '', `Covariates tested` = '', `Best-fit model` = ''))
      # Add to table 2:
      table2i <- rbind(table2i, tabspp)
    }

    table2i

    # Add to growing table
    table2 <- rbind(table2, table2i)

  }

  table2

  ##############################################################################
  # Tables 1b, 3, and 4 in standard reports
  # Table 1b -- Sample size used for density/abundance estimates
  # Table 3 - Line-transect parameter estimates for species/taxa (ESW, ss, g0)
  # Table 4: Density and abundance

  if(verbose){message('--- Preparing Tables 1b (estimate sample size),\n                     3 (ESW, s, and g(0)) and \n                     4 (density/abundance) ...')}

  # Save quick versions of key results
  (estimates <- lta_result$estimate)
  if(!is.null(lta_result$bootstrap)){
    (bootstraps <- lta_result$bootstrap$summary)
  }else{
    bootstraps <- data.frame()
  }
  suppressWarnings({
    (estimate_titles <-
       estimates %>%
       dplyr::arrange(as.numeric(substr(species, 1, 3))) %>%
       dplyr::pull(title) %>%
       unique)
  })
  (cols <- paste0(estimates$year,' ',estimates$Region) %>% unique %>% sort)

  # Stage result
  table1b <- table3 <- table4 <- tibble()

  # Loop through each species ==================================================
  title_i <- 1 # debugging
  for(title_i in 1:length(estimate_titles)){
    (titi <- estimate_titles[title_i]) # get the species
    (spp_estimates <- estimates %>% dplyr::filter(title == titi)) # filter to its estimates
    (speciesi <- spp_estimates$species %>% unique) # get species involved
    (titi <- strsplit(titi,'_')[[1]][1]) # format its Species/category title, removing any characters are after the underscore symbol

    # Stage this species' table
    if(title_i == 1){
      spp_table1 <- spp_table3 <- spp_table4 <- tibble(id=c('','')) # if first spp in estimates, format header
    }else{
      spp_table1 <- spp_table3 <- spp_table4 <- tibble(id=c(''))
    }

    # Loop through each year-region estimates (which are columns) ==============
    coli <- 4 # debugging
    for(coli in 1:length(cols)){
      (yr <- cols[coli])
      (yeari <- strsplit(yr,' ')[[1]][1])
      (regioni <- strsplit(yr,' ')[[1]][-1] %>% paste(collapse=' '))

      # Filter estimate and bootstrap summary down to this specific title-species-year-region
      (estimati <- estimates %>% dplyr::filter(title == titi,
                                               species == speciesi,
                                               year == yeari,
                                               Region == regioni))
      booti <- data.frame()
      if(nrow(bootstraps) > 0){
        (booti <- bootstraps %>% dplyr::filter(title == titi,
                                               species == speciesi,
                                               year == yeari,
                                               Region == regioni))
      }

      # Prepare table 1b =======================================================

      (tabi <- dplyr::tibble(`Species or category` = titi,
                             `Code` = '-',
                             `Ntot` = '-',
                             `Nsys` = '-',
                             `Nest` = '-'))
      if(nrow(estimati) > 0){
        tabi$Code <- estimati$species
        tabi$Nest <- estimati$n
      }
      tabi
      tabform <- tabi
      if(title_i == 1){
        (tabsub <- rbind(names(tabi)) %>% as.data.frame %>%  tibble)
        names(tabsub) <- names(tabi)
        tabsub
        tabform <- rbind(tabsub, tabi)
      }
      tabform
      (names(tabform) <- c(paste0('var',coli,'.2'), paste0('var',coli,'.3'), yeari, regioni, paste0('var',coli,'.6') ))
      tabform
      if(coli > 1){
        tabform <- tabform %>% dplyr::select(3:ncol(tabform))
      }
      tabform
      spp_table1 <- cbind(spp_table1, tabform)

      # Prepare table 3 ========================================================

      (tabi <- tibble(`Species or category` = titi,
                      `Mean ESW` = '-', `Mean s` = '-', `g(0)` = '-', `(CV)` = '-'))
      if(nrow(estimati) > 0 && any(estimati$D > 0)){
        (tabi <- tibble(`Species or category` = titi,
                        `Mean ESW` = ifelse(!is.na(estimati$ESW_mean),
                                            as.character(round(estimati$ESW_mean, 2)), '-'),
                        `Mean s` = ifelse(!is.na(estimati$size_mean),
                                          as.character(round(estimati$size_mean,1)), '-'),
                        `g(0)` = estimati$g0_est %>% round(2),
                        `(CV)` = '-'))
        if(nrow(booti)>0){if(!is.na(booti$g0_cv)){ tabi$`(CV)` <- booti$g0_cv %>% round(2) }}
      }
      tabi
      tabform <- tabi
      if(title_i == 1){
        (tabsub <- rbind(names(tabi)) %>% as.data.frame %>%  tibble)
        names(tabsub) <- names(tabi)
        tabsub
        tabform <- rbind(tabsub, tabi)
      }
      tabform
      names(tabform) <- c(paste0('var',coli,'.2'), yeari, regioni,
                          paste0('var',coli,'.5'), paste0('var',coli,'.6'))
      tabform
      if(coli > 1){
        tabform <- tabform %>% dplyr::select(2:ncol(tabform))
      }
      tabform
      spp_table3 <- cbind(spp_table3, tabform)

      # Prepare table 4 ========================================================

      (tabi <- tibble(`Species or category` = titi,
                      `Density` = '-', `Abundance` = '-', `CV` = '-', `95% CI` = '-'))
      if(nrow(estimati) > 0 && any(estimati$D > 0)){
        (tabi <- tibble(`Species or category` = titi,
                        `Density` = (estimati$D * 1000) %>% round(2) %>% stringr::str_pad(width=4, side='both',pad='0'),
                        `Abundance` = estimati$N %>% round(0) %>% prettyNum(big.mark=','),
                        `CV` = '-',
                        `95% CI` = '-'))
        if(nrow(booti)>0){
          tabi$`CV` <- booti$CV %>% round(2)
          tabi$`95% CI` <- paste0(prettyNum(round(booti$L95), big.mark=','),'-',prettyNum(round(booti$U95), big.mark=','))
        }
      }
      tabi
      tabform <- tabi
      if(title_i == 1){
        (tabsub <- rbind(names(tabi)) %>% as.data.frame %>%  tibble)
        names(tabsub) <- names(tabi)
        tabsub
        tabform <- rbind(tabsub, tabi)
      }
      tabform
      names(tabform) <- c(paste0('var',coli,'.2'), yeari, regioni,
                          paste0('var',coli,'.5'), paste0('var',coli,'.6'))
      tabform
      if(coli > 1){
        tabform <- tabform %>% dplyr::select(2:ncol(tabform))
      }
      tabform
      spp_table4 <- cbind(spp_table4, tabform)
    }

    # Build Table 1 ===============================
    spp_table1
    table1b <- rbind(table1b, spp_table1)
    table1b

    # Build Table 3 ===============================
    spp_table3
    table3 <- rbind(table3, spp_table3)
    table3

    # Build Table 4 ===============================
    spp_table4
    table4 <- rbind(table4, spp_table4)
    table4

  }

  # Finalize formatting ========================================================

  table1b$id <- NULL
  names(table1b)[grep('var',names(table1b))] <- ' '
  table1b

  table3
  table3$id <- NULL
  names(table3)[grep('var',names(table3))] <- ' '
  table3

  table4
  table4$id <- NULL
  names(table4)[grep('var',names(table4))] <- ' '
  table4

  ##############################################################################
  # Appendix Table A1 (Survey strata area by species and year)

  if(verbose){message('--- Preparing Appendix Table 1 (study areas) ...')}

  suppressMessages({
    (estimates <- lta_result$estimate)
    tablea1 <-
      estimates %>%
      rename(Year = year) %>%
      mutate(Area = format(round(Area), big.mark=',')) %>%
      group_by(Area, Region) %>%
      summarize(Year = paste(unique(Year), collapse=', '),
                Species = ifelse(length(unique(title)) > 1, 'All other species', unique(title))) %>%
      mutate(Region = gsub(r"{\s*\(}",'',Region)) %>%
      mutate(Region = gsub(r"{\s*\)}",'',Region)) %>%
      mutate(Region_Year = paste0(Region,' (',Year,')')) %>%
      select(Species, Region_Year, Area) %>%
      tidyr::pivot_wider(id_cols = Species, values_from = Area, names_from = Region_Year, values_fill = '-')
  })

  ##############################################################################
  # Appendix Table A2: Effort & Beaufort

  efflist <- NULL
  if(!is.null(cruz)){

    if(verbose){message('--- Preparing Appendix Table 2 (Effort & Beaufort proportions) ...')}

    suppressMessages({

      (estimates <- lta_result$estimate)
      (est_years <- estimates$year %>% unique)
      (est_regions <- estimates$Region %>% unique)
      (est_regions <- gsub(r"{\s*\(}",'',est_regions))
      (est_regions <- gsub(r"{\s*\)}",'',est_regions))

      # Stage result
      efflist <- list()

      # Loop through each estimate year
      i = 1
      for(i in 1:length(est_years)){
        (yeari <- est_years[i])
        cruzi <- filter_cruz(cruz,
                             years = as.numeric(yeari),
                             regions = est_regions,
                             bft_range = 0:6,
                             eff_types = 'S',
                             on_off = 'TRUE',
                             verbose = FALSE)
        (cohorts <- cruzi$cohorts) %>% length
        effy <- data.frame()
        ci <- 1
        for(ci in 1:length(cohorts)){
          (bft <- summarize_bft(cruzi, cohort=ci)$by_stratum)
          effi <-
            bft %>%
            group_by(stratum) %>%
            mutate(Effort = sum(round(km))) %>%
            mutate(Species = names(cruzi$cohorts)[ci]) %>%
            ungroup() %>%
            tidyr::pivot_wider(id_cols = c(Species, stratum, Effort),
                               names_from = bftr,
                               names_prefix = 'B',
                               values_from = prop,
                               values_fill = 0)
          effy <- rbind(effy, effi)
        }
        effy
        keeps <- sapply(effy$stratum, function(x){any(grepl(x, unique(estimates$Region)))})
        (effy <- effy[keeps,])
        effy <-
          effy %>%
          arrange(stratum) %>%
          group_by(Effort) %>%
          summarize_all(list(first)) %>%
          relocate(Species, Stratum = stratum)
        effy

        efflist[[length(efflist)+1]] <- effy
        names(efflist)[length(efflist)] <- as.character(yeari)
      }
      efflist
    })
  }

  ##############################################################################

  return_list <- list(table1a = table1a,
                      table1b = table1b,
                      table2 = table2,
                      table3 = table3,
                      table4 = table4,
                      tableA1 = tablea1,
                      tableA2 = efflist)

  return(return_list)
}

