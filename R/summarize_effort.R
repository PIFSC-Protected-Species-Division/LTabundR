#' Summarize `Wincruz` survey effort
#'
#' Inventory and summarize survey effort within a `LTabundR` `cruz` object in various ways.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to summarize, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @return A list with various summary tables:
#' \enumerate{
#' \item `total`: Grand total distance and days surveyed. Two columns: `km`, `days`.
#' \item `total_by_year`: Total distance and days surveyed for each year Three columns:
#' `year`, `km`, `days`.
#' \item `total_by_cruise`: Total distance and days surveyed for each cruise. Four columns:
#' `Cruise`, `year`, `km`, `days`.
#' \item `total_by_effort`: Total distance and days surveyed, grouped by segments that will be included in the analysis and those that won't.
#' Five columns: `Cruise`, `year`, `use` (used for analysis or not), `km`, and `days`.
#' \item `total_by_stratum`: Total distances and days surveyed within each stratum, again grouped by segments
#' that will be included in the analysis and those that won't. Six columns:
#' `Cruise`, `year`, `stratum`, `use`, `km`, `days`.
#' }
#'
#' @export
#'
summarize_effort <- function(cruz,
                             cohort=1){

  #=============================================================================
  # For debugging -- only
  if(FALSE){
    data(cruz_proc)
    cruz <- cruz_proc
    cohort = 1

    # test
    summarize_effort(cruz) %>% names
    summarize_effort(cruz)$total
    summarize_effort(cruz)$total_by_effort
    summarize_effort(cruz)$total_by_stratum
  }
  #=============================================================================

  # Filter down to the cohort-analysis specified
  cohorti <- cruz$cohorts[[cohort]]
  names(cohorti)
  survey <- cohorti
  cohort_settings <- cruz$settings$cohorts[[cohort]]
  names(survey)
  eff <- survey$segments
  length(eff)
  nrow(eff)
  head(eff)
  eff %>% data.frame %>% head
  #cbind(eff$yday1, eff$yday2)

  suppressWarnings({
    suppressMessages({

      # Group effort by Cruise, year, stratum, and analysis use/not use
      eff_stratum <-
        eff %>%
        dplyr::group_by(Cruise, year, stratum, use) %>%
        dplyr::summarize(km = round(sum(dist, na.rm=TRUE)),
                         days = length(unique(c(yday1, yday2))))

      # Group by Cruise, year use/not use
      eff_eff <-
        eff %>%
        dplyr::group_by(Cruise, year, use) %>%
        dplyr::summarize(km = round(sum(dist, na.rm=TRUE)),
                         days = length(unique(c(yday1, yday2))))

      # Group by Cruise and year
      eff_cruise <-
        eff %>%
        dplyr::group_by(Cruise, year) %>%
        dplyr::summarize(km = round(sum(dist, na.rm=TRUE)),
                         days = length(unique(c(yday1, yday2))))

      # Group by year
      eff_year <-
        eff %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(km = round(sum(dist, na.rm=TRUE)),
                         days = length(unique(c(yday1, yday2))))

      # Total effort
      eff_tot <-
        eff %>%
        dplyr::summarize(km = round(sum(dist, na.rm=TRUE)),
                         days = length(unique(c(yday1, yday2))))

    })
  })

  # Prepare return
  return_list <- list(total = eff_tot,
                      total_by_cruise = eff_cruise,
                      total_by_year = eff_year,
                      total_by_effort = eff_eff,
                      total_by_stratum = eff_stratum)

  # Return
  return(return_list)
}

