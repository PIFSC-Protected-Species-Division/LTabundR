#' Estimate density & abundance
#'
#' This function is typically not called by the user (though it certainly can be);
#' instead, it is called as a subroutine within `lta()`.
#' This function estimates density/abundance for `Wincruz` data,
#' as processed by `LTabundR::process_surveys()`.
#'
#' @param segments Effort segments dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$segments`),
#' already filtered to contain the effort you wish to use to estimate density/abundance.
#' @param sightings Sightings dataframe, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$sightings`),
#' already filtered to contain the sightings you wish to use to estimate the density/abundance.
#' This sightings dataframe must have a column named `esw`, which can be provided
#' by the `LTabundR` function `fit_df()` (fit a detection function model).
#' If `NA`'s occur in the `esw` column, they will be replaced with the mean `esw` value
#' for the remainder of the dataset in that region-year.
#' Similarly, if `sightings` has a column named `ss_valid` (all standard `cruz` objects do)
#' and any of the rows in that column are `FALSE`, those rows will have their `best`
#' school size estimate (which will be `NA` or `1`, since they are invalid) replaced
#' by the mean best estimate for their respective species.
#' @param das Dataframe of DAS data, drawn from a `cruz` object (e.g., `cruz$cohorts[[1]]$das`)
#' @param strata A dataframe, drawn from a `cruz` object (e.g., `cruz$strata`),
#'  summarizing the geostrata provided (their name and area, in square km).
#' @param truncation_distance The truncation distance to apply to sightings.
#' @param use_g0 A Boolean, with default `TRUE`, indicating whether or not to use custom `g(0)` value(s).
#' If `FALSE`, the assumed `g(0)` value will be 1.
#' @param g0 A numeric vector of length 2: the `g(0)` for small and large groups.
#' @param g0_threshold The school size threshold between small and large groups.
#' @param region_pool Accepts a Boolean; if `TRUE`, the functions will produce a single estimate for all geostrata combined;
#' if `FALSE`, the function calculates density and abundance for each geostratum
#' contained within the data (according to `segments$stratum`) separately. If you set
#' `region_pool` to `TRUE`, the next two arguments ought to be specified.
#' @param region_pool_name A character string indicating a custom name to use in the `Region` column in the output.
#' Only used in the event that `region_pool == TRUE`.
#' @param region_pool_area A numeric indicating the area, in square km, of the pooled region.
#' Only used in the event that `region_pool == TRUE`.
#' If left `NULL` but `region_pool == TRUE`, abundance will not be calculated.
#' @param year_pool A Boolean indicating whether or not to pool years (set `year_pool` to `TRUE`)
#' or to report density/abundance for each year separately (set to `FALSE`).
#' @param forced_effort If this is a single numeric value instead of `NULL` (`NULL` is the default),
#' this value will be used as the survey effort, in km, in a brute-force method;
#' this same value will be used for every year and region. This is only helpful if you
#' are looking for a relatively easy way to compare results from your own analysis to another
#' (e.g., comparing `LTabundR` results to reports from NOAA reports prior to 2021, in which effort was
#' calculated slightly differently).
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @return A `data.frame` with a row for each density/abundance estimate.
#' You may expect multiple rows if multiple regions or years are contained within the datasets provided.
#' This `data.frame` contains the following fields:
#' \enumerate{
#' \item `Region`: Name(s) of geostrata represented in this estimate.
#' \item `Area`: Area of geostratum / region, in square km.
#' \item `year`: Years represented in this estimate.
#' \item `segments`: The number of effort segments used to estimate density/abundance.
#' \item `km`: The km of trackline effort contained in these segments.
#' \item `Area_covered`: The Area surveyed, according to `km` and `ESW_mean` (see next column).
#' \item `ESW_mean`: Mean effective strip width, in kw, calculated as the truncation distance multiplied by the mean probability of detection for all detections.
#' \item `n`: The number of detections in the data.
#' \item `g0_mean`: The mean `g(0)` estimate.
#' \item `ER_clusters`: The encounter rate for detections (schools) (`n / km`)
#' \item `D_clusters`: The density of detections (schools).
#' \item `N_clusters`: The abundance of schools.
#' \item `size_mean`: Average school size.
#' \item `size_sd`: Standard deviation of school size.
#' \item `ER`: Animal encounter rate.
#' \item `D`: Animal density.
#' \item `N`: Animal abundance.
#' }
#'
#' @export
#'
abundance <- function(segments,
                      sightings,
                      das,
                      strata,
                      truncation_distance,
                      use_g0 = TRUE,
                      g0 = c(1, 1),
                      g0_threshold = 20,
                      region_pool = FALSE,
                      region_pool_name = 'Regions pooled',
                      region_pool_area = NA,
                      year_pool = FALSE,
                      forced_effort = NULL,
                      verbose=TRUE){

  ############################################################################
  if(FALSE){
    truncation_distance <- 5.5
    use_g0 = TRUE
    g0 = c(1, 1)
    g0_threshold = 20
    region_pool = FALSE
    region_pool_name = 'Regions pooled'
    region_pool_area = NA
    year_pool = FALSE
    forced_effort = NULL
    verbose=TRUE

    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    strata <- cruz$strata
    sightings <- cruz$cohorts$all$sightings
    sightings <- df_fit(sightings, truncation_distance)$sightings
    segments <- cruz$cohorts$all$segments
    das <- cruz$cohorts$all$das
    cohorti <- filter_cohort(segments, sightings, das, analysis_only = TRUE, years = 2020, regions = 'HI_EEZ')
    segments <- cohorti$segments
    sightings <- cohorti$sightings

    #segments = region_segments
    #sightings = region_sightings
  }

  ############################################################################
  # Deal with invalid school size estimates

  if(FALSE){   # skipping this as of july 2024 -- no longer interpolating missing values for users

    if('ss_valid' %in% names(sightings)){
    if(any(sightings$ss_valid == FALSE)){
      (bads <- which(sightings$ss_valid == FALSE))
      i=1 # for debugging
      for(i in 1:length(bads)){
        (badi <- bads[i])
        (sppi <- sightings$species[badi])
        # Get mean best group size for this species in the dataset
        (besti <- sightings %>% filter(species == sppi) %>% pull(best) %>% mean(na.rm=TRUE))
        # Revise best estimate for this badi
        sightings$best[badi] <- besti
      }
      #sightings$best[bads]
    }
    }

  }

  (data_table <- sightings)

  ############################################################################
  # Stage the output object
  output_summ <- NULL

  # Handle zero effort / missing detection function
  if(nrow(segments) > 0){
    if(nrow(data_table) > 0){
      # Assign G0 based on school size
      if(verbose){message('--- --- Staging g0 values ...')}
      data_table$g0 <- 1
      if(use_g0){
        data_table$g0 <- g0[1]
        if(length(g0)>1){
          data_table$g0[data_table$best > g0_threshold] <- g0[2]
        }
      }
      data_table

      # Get rid of invalid group size estimates
      nrow(data_table)
      data_table <-
        data_table %>%
        dplyr::filter(best > 0) %>%
        dplyr::filter(ss_valid == TRUE) # adding this line in july 2024
      nrow(data_table)
    }

    ############################################################################
    # Abundance estimates
    ############################################################################
    # Core internal function for summarizing encounter rate, density, and abundance
    if(verbose){message('--- --- Calcluating density and abundance for various region-year combinations ...')}

    summarize_density <- function(summi, dati){
      summi$Area_covered <- ifelse(summi$km > 0, NA, 0) # if there is effort, AreaCovered will only be calculable if there are sightings (ESW).
      summi$ESW_mean <- NA
      summi$n <- 0
      summi$g0_est <- NA
      summi$N_clusters <- summi$D_clusters <- summi$ER_clusters <- 0
      summi$size_sd <- summi$size_mean <- NA
      summi$N <- summi$D <- summi$ER <- 0
      summi
      # only proceed if there was effort & sightings
      if(summi$km > 0 & nrow(dati)>0){       # width x   length
        dati$D_clusters <- 1 / (2 * dati$esw * dati$g0 * summi$km)
        (dati$D <- dati$best / (2 * dati$esw * dati$g0 * summi$km))
        summi$n <- dati %>% nrow
        summi$ESW_mean <- mean(dati$esw, na.rm=TRUE)
        summi$Area_covered <- summi$km * summi$ESW_mean
        (summi$g0_est <- mean(dati$g0, na.rm=TRUE))
        summi$ER_clusters <- summi$n / summi$km
        summi$D_clusters <- dati$D_clusters %>% sum(na.rm=TRUE)
        summi$N_clusters <- summi$D_clusters * summi$Area
        summi$size_mean <- mean(dati$best, na.rm=TRUE)
        summi$size_sd <- sd(dati$best, na.rm=TRUE)
        summi$ER <- sum(dati$best) / summi$km
        summi$D <- dati$D %>% sum(na.rm=TRUE)
        summi$N <- summi$D * summi$Area
      }
      summi
      return(summi)
    }

    suppressWarnings({ suppressMessages({

      ############################################################################
      # Abundance estimates for specific output regions and output years

      (output_regions <- segments$stratum %>% unique )
      (output_years <- segments$year %>% unique %>% sort)

      # Handling pooling
      if(region_pool){output_regions <- 'All regions'}
      if(year_pool){output_years <- paste(output_years, collapse=' | ')}
      output_regions
      output_years

      # Loop through each output region
      output_summ <- data.frame()
      i=1
      for(i in 1:length(output_regions)){
        (regioni <- output_regions[i])

        # Handle pooled regions
        filter_region <- regioni
        if(regioni == 'All regions'){filter_region <- segments$stratum %>% unique}
        filter_region

        # Get segment IDs that occurred in this region(s)
        (seg_ids <- region_segments(das, filter_region))

        # Loop through years
        annual_summ <- data.frame()
        j=1
        for(j in 1:length(output_years)){
          yearj <- output_years[j]
          yearj
          (filter_year <- strsplit(as.character(yearj),' | ')[[1]])

          # Filter to only the segments for this region-year
          summi <-
            segments %>%
            dplyr::filter(seg_id %in% seg_ids) %>%
            dplyr::filter(year %in% as.numeric(filter_year)) %>%
            dplyr::summarize(year = yearj,
                             segments = dplyr::n(),
                             km = sum(dist))

          # Add region name and area, depending on whether things are pooled
          summi
          summi$Area <- NA
          if(region_pool){
            summi$Region <- region_pool_name
            if(!is.na(region_pool_area)){summi$Area <- region_pool_area}
          }else{
            summi$Region <- regioni
            (matchi <- which(strata$stratum == regioni))
            if(length(matchi)>0){summi$Area <- strata$area[matchi]}
          }
          summi

          # Finish summi formatting
          summi <-
            summi %>% dplyr::select(Region, Area, year, segments, km)
          summi

          # Filter sightings to region and year
          if(nrow(data_table)>0){
            dati <-
              data_table %>%
              dplyr::filter(seg_id %in% seg_ids) %>%
              dplyr::filter(year %in% filter_year)
          }else{
            dati <- data_table
          }
          dati %>% head

          # Handle missing ESW values (replace NAs with the mean ESW for the region-year)
          if(nrow(dati)>0){
            if(any(is.na(dati$esw))){
              dati <-
                dati %>%
                mutate(mean_esw = mean(esw, na.rm=TRUE)) %>%
                mutate(esw = ifelse(is.na(esw), mean_esw, esw))
            }
          }

          # Handle forced effort
          if(!is.null(forced_effort)){
            summi$km <- forced_effort
            if(verbose){message('--- --- *** Forcing effort for every estimate ... ***')}
          }

          # Calculate abundance / density
          summi <- summarize_density(summi, dati)
          if(is.na(summi$g0_est)){summi$g0_est <- g0[1]}
          summi$g0_small <- g0[1]
          summi$g0_large <- g0[2]
          summi
          output_summ <- rbind(output_summ, summi)
        } # year loop
      } # region loop
    }) }) # end of suppressing warnings and messages

    output_summ

  } # end of if nrow segments > 0

  return(output_summ)
}
