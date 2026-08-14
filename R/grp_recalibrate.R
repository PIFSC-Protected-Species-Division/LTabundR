#' Recalibrate processed sightings
#'
#' Re-run group size calibration for a selection of sightings within a pre-existing `cruz` object.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#'
#' @param cohort The cohort whose sightings you would like to recalibrate,
#' provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @param spp Character vector of species codes to filter by.
#' Leave `NULL` if you want to recalibrate all sightings in the cohort.
#'
#' @param include_mixed If `TRUE`, mixed-species groups containing the `spp`
#' of interest will be recalibrated, including estimates for non-target species
#' present in those mixed groups.
#'
#' @param mixed_only If `TRUE`, *only* mixed-species groups will be recalibrated.
#' Ignored if `include_mixed` is `FALSE`.
#'
#' @param spp_max_only If `TRUE`, the only mixed-species groups recalibrated
#' will be those in which the `spp` of interest are the most abundant species
#' in the group (i.e., `spp_max` is found in `spp`).  Ignored if `include_mixed` is `FALSE`.
#'
#' @param ops_max The calibration options to apply to the predominant species
#' in the group. Default is the "Gerrodette" method, with default values provided
#' in the built-in dataset `data(grp_ops_gerrodette)`.
#'
#' @param ops_not The calibration options to apply to the non-predominant species
#' in any mixed groups contained in the set of sightings to be recalibrated.
#' Default is the "ABUND" method, with default values provided
#' in the built-in dataset `data(grp_ops_abund)`.

#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#' @param toplot Boolean, with default `TRUE`, indicating whether a diagnostics plot should be displayed at the end of the recalibration.
#'
#' @return A `list` with the following slots:
#' \enumerate{
#' \item `key`: A `data.frame` key of which sightings were re-calibrated,
#' with three columns: `i` = row index from `cruz$cohorts[[cohort]]$sightings`;
#' `SightNoDaily` = unique Sighting identifier from same;
#' `species` = the species code.
#' \item `recalibrated_sits`: Complete `sightings` rows but only for the sightings
#' that were re-calibrated.
#' \item `updated_cruz`: Updated `cruz` object in which `cruz$cohorts[[cohort]]$sightings`
#' includes the recalibrated sightings as well as all untouched sightings.
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
grp_recalibrate <- function(cruz,
                            cohort=1,
                            spp = NULL,
                            include_mixed = TRUE,
                            mixed_only = FALSE,
                            spp_max_only = FALSE,
                            ops_max = grp_ops_gerrodette,
                            ops_not = grp_ops_abund,
                            verbose = TRUE,
                            toplot = TRUE){

  if(FALSE){ # for development & debugging =====================================
    library(dplyr)
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cruz$cohorts$all$sightings$calibr_method %>% table
    data(grp_ops_gerrodette)
    data(grp_ops_abund)

    cohort=1
    spp <- '013'
    ops_max <- grp_ops_gerrodette
    ops_not <- grp_ops_abund
    mixed_only = FALSE
    spp_max_only = TRUE
    verbose = TRUE
    toplot = TRUE

    # Try it ====================================

    # Striped dolphin
    mr <- grp_recalibrate(cruz,
                          spp = '013',
                          ops_max = grp_ops_gerrodette,
                          ops_not = grp_ops_abund)

    # Flip calibration ops
    mr <- grp_recalibrate(cruz,
                          spp = '013',
                          ops_not = grp_ops_gerrodette,
                          ops_max = grp_ops_abund)

    # Control (should be no changes)
    mr <- grp_recalibrate(cruz,
                          spp = '013',
                          ops_not = grp_ops_abund,
                          ops_max = grp_ops_abund)

    # Mess with mixed inclusion
    mr <- grp_recalibrate(cruz, spp = '013',
                          include_mixed = FALSE,
                          mixed_only = TRUE,
                          spp_max_only = TRUE)
    mr <- grp_recalibrate(cruz, spp = '013',
                          include_mixed = TRUE,
                          mixed_only = TRUE,
                          spp_max_only = TRUE)
    mr <- grp_recalibrate(cruz, spp = '013',
                          include_mixed = TRUE,
                          mixed_only = TRUE,
                          spp_max_only = FALSE)

    # Try another species: spotted dolphins
    mr <- grp_recalibrate(cruz,
                          spp = c('002','006','090'))

  } #===========================================================================


  # Filtering ==================================================================

  if(verbose){message('--- filtering sightings according to inputs ...')}

  # Filter down to the cohort-analysis specified
  cohorti <- cruz$cohorts[[cohort]]
  names(cohorti) # review
  sits <- cohorti$sightings
  sits %>% head
  sits %>% nrow

  # get settings
  sets <- cruz$settings$cohorts[[cohort]]
  sets %>% names
  sets$geometric_mean_group
  sets$use_low_if_na

  # add unique identifier
  sits$id <- 1:nrow(sits)

  # Filter to species
  sitspp <- sits
  if(!is.null(spp)){
    sitspp <- sits %>% filter(species %in% spp)
  }

  # Get sighting numbers for these detections
  (sitnos <- sitspp %>% pull(SightNoDaily) %>% unique)
  sitnos %>% table %>% table

  # Get all of those sightings, including for rows of other species in the same group
  (siti <- sits %>% filter(SightNoDaily %in% sitnos))
  nrow(siti)

  # include_mixed?
  if(include_mixed){

    # mixed only?
    if(mixed_only){
      siti <- siti %>% filter(mixed == TRUE)
    }

    # spp_max_only
    if(spp_max_only){
      siti1 <- siti %>% filter(mixed == FALSE)
      siti2 <- siti %>% filter(mixed == TRUE & spp_max %in% spp)
      siti <- rbind(siti1, siti2) %>% arrange(id)
    }

  }else{
    siti <- siti %>% filter(mixed == FALSE)
  }

  if(nrow(siti)>0){
    if(verbose){message('--- left with ', nrow(siti), ' sightings ...')}
  }else{
    if(verbose){message('--- no sightings left! returning empty results')}
  }

  # stage result objects
  sitkey <- data.frame()
  cruz_new <- cruz
  sits_new <- sits
  siti_new <- siti

  # other objects that will be used in the following conditional
  sitnos_max <- sitnos_not <- data.frame()
  sitfull_max <- sitfull_not <- grps_max <- grps_not <- data.frame()
  grpcal <- grpcal_max <- grpcal_not <- data.frame()


  # proceed only if any sightings are left after filtering
  if(nrow(siti)>0){
    if(verbose){message('--- retrieving DAS data for those sightings ...')}

    # store key of line IDs
    (sitkey <- siti %>% select(id, SightNoDaily, species))
    sitkey$SightNoDaily %>% table %>% table
    siti %>% filter(mixed == TRUE) %>% nrow

    # Now get associated DAS data ================================================

    # re-render swfscDAS sightings table with all information
    suppressWarnings({
      sitfull <- swfscDAS::das_sight(cohorti$das, return.format = 'complete')
    })
    sitfull %>% names
    nrow(sitfull)

    # get remaining SightNo's -- first for spp_max cases...
    (sitnos_max <-
        rbind(siti %>% filter(mixed == FALSE),
              siti %>% filter(mixed == TRUE & spp_max %in% spp)) %>%
        select(SightNoDaily, id, species))
    sitnos_max %>% nrow

    # get full sighting record for these
    if(length(sitnos_max)>0){
      (sitfull_max <-
         sitfull %>%
         filter(Event == 'S',
                SightNoDaily %in% unique(sitnos_max$SightNoDaily)))
      sitfull_max %>% nrow
      sitfull_max$SightNoDaily %>% unique %>% length

      # get essential group size information
      (grps_max <-
          sitfull_max %>%
          select(SightNoDaily, year, Bft, Prob:GsSchoolLow)) %>% as.data.frame %>% head
      grps_max$SightNoDaily %>% unique %>% length
      nrow(grps_max)
    }

    # ... then for not spp_not cases ...
    (sitnos_not <-
        siti %>% filter(mixed == TRUE & !spp_max %in% spp) %>%
        select(SightNoDaily, id))
    sitnos_not
    sitfull_not <- grps_not <- data.frame()
    if(length(sitnos_not)>0){
      sitfull_not <-
        sitfull %>%
        filter(Event == 'S',
               SightNoDaily %in% unique(sitnos_not$SightNoDaily))
      (grps_not <- sitfull_not %>% select(SightNoDaily, year, Bft, Prob:GsSchoolLow))
    }

    # Calibration ================================================================
    if(verbose){message('--- re-calibrating sightings ...')}

    # first for spp_max cases
    if(verbose){message('--- --- first for spp_max cases (n=', nrow(grps_max), ') ...')}
    if(nrow(grps_max)>0){
      i=1
      (sitmax <- grps_max$SightNoDaily %>% unique) %>% length
      if(verbose){pb <- txtProgressBar(min = 0, max = length(sitmax), style = 3)}
      for(i in 1:length(sitmax)){
        if(verbose){setTxtProgressBar(pb, i)}
        (sitnoi <- sitmax[i])
        (grpi <- grps_max %>% filter(SightNoDaily == sitnoi))
        (grpcali <- LTabundR::grp_size(grpi,
                                       calibrate = ops_max,
                                       geometric_mean = sets$geometric_mean_group,
                                       use_low_if_na = sets$use_low_if_na))
        (grpcali <- data.frame(grpi %>% select(SightNoDaily) %>% head(1),
                               grpcali))
        grpcal_max <- rbind(grpcal_max, grpcali)
      }
      message('')
      grpcal_max
    }

    if(verbose){message('--- --- then for other cases (n=', nrow(grps_not), ') ...')}
    if(nrow(grps_not)>0){
      (sitnot <- grps_not$SightNoDaily %>% unique) %>% length
      if(verbose){pb <- txtProgressBar(min = 0, max = length(sitnot), style = 3)}
      for(i in 1:length(sitnot)){
        if(verbose){setTxtProgressBar(pb, i)}
        (sitnoi <- sitnot[i])
        (grpi <- grps_not %>% filter(SightNoDaily == sitnoi))
        (grpcali <- LTabundR::grp_size(grpi,
                                      calibrate = ops_not,
                                      geometric_mean = sets$geometric_mean_group,
                                      use_low_if_na = sets$use_low_if_na))
        (grpcali <- data.frame(grpi %>% select(SightNoDaily) %>% head(1),
                              grpcali))
        grpcal_not <- rbind(grpcal_not, grpcali)
      }
      message('')
      grpcal_not
    }

    # Combine spp_max cases with other cases
    grpcal_max %>% nrow
    grpcal_not %>% nrow
    (grpcal <- rbind(grpcal_max, grpcal_not))
    nrow(grpcal)
    # add id back
    grpcal <-
      left_join(grpcal, sitkey, by=c("SightNoDaily", "species")) %>%
      arrange(id)

    # Ensure 1:1 join without NAs
    nrow(grpcal)
    nrow(siti) # compare to original
    which(is.na(grpcal$id))
    table(grpcal$id) %>% table


    # Prepare final results ====================================================

    if(verbose){message('--- preparing outputs to return ...')}
    sitkey
    siti
    grpcal
    changed_sits <- data.frame()

    # make sure calibration parameter columns are not logical
    if(class(sits$calibr_beta)=='logical'){sits$calibr_beta <- as.numeric(sits$calibr_beta)}
    if(class(sits$calibr_intercept)=='logical'){sits$calibr_intercept <- as.numeric(sits$calibr_intercept)}
    if(class(siti$calibr_beta)=='logical'){siti$calibr_beta <- as.numeric(siti$calibr_beta)}
    if(class(siti$calibr_intercept)=='logical'){siti$calibr_intercept <- as.numeric(siti$calibr_intercept)}
    if(class(grpcal$calibr_beta)=='logical'){grpcal$calibr_beta <- as.numeric(grpcal$calibr_beta)}
    if(class(grpcal$calibr_intercept)=='logical'){grpcal$calibr_intercept <- as.numeric(grpcal$calibr_intercept)}

    # update rows in siti with new values
    siti_new <- dplyr::rows_update(siti, grpcal, by='id')
    siti %>% nrow
    siti_new %>% nrow
    sits_new <- dplyr::rows_update(sits, grpcal, by='id')
    sits %>% nrow
    sits_new %>% nrow

    # Rebuild the cruz
    cruz_new <- cruz$cohorts[[cohort]]$sightings <- sits_new

  } # end of if nrow(siti)>0

  # Validate ===================================================================

  if(toplot){
    par(mfrow=c(2, 2))
    plot(siti_new$ss_tot ~ siti$ss_tot,
         main='ss_tot', xlab='Original', ylab='Re-calibrated')
    abline(a=0, b=1, lty=2)
    plot(siti_new$best ~ siti$best,
         main='best', xlab='Original', ylab='Re-calibrated')
    abline(a=0, b=1, lty=2)
    plot(siti_new$ss_percent ~ siti$ss_percent,
         main='ss_percent', xlab='Original', ylab='Re-calibrated')
    abline(a=0, b=1, lty=2)
    (spsum <- siti %>% group_by(species) %>% summarize(n=sum(best)) %>% arrange(species))
    (spsum_new <- siti_new %>% group_by(species) %>% summarize(n=sum(best)) %>% arrange(species))
    plot(log(spsum_new$n) ~ log(spsum$n),
         main='total species counts', xlab='log Original', ylab='log Re-calibrated')
    abline(a=0, b=1, lty=2)
    par(mfrow=c(1, 1))
  }

  # Return results =============================================================
  # return indices AND grpcal results AND modified sits AND entire revised sightings dataframe

  message('\n--- finished!\n')
  return(list(key = sitkey %>% rename(i = id),
              recalibrated_sits = siti_new,
              updated_cruz = cruz_new))
}
