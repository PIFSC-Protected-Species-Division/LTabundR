#' Summarize `Wincruz` by Beaufort Sea State
#'
#' Calulcate the distance and proportion of effort within each Beaufort Sea State
#' within your `Wincruz` survey data.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param use_only If `TRUE` (the default), sea states will only be summarized for effort where `use=TRUE`, which is
#' the effort that will be used in detection function model fitting. If `FALSE`, all effort will be summarized.
#' @param cohort The cohort whose data you would like to summarize, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @return A list with various summary tables:
#' \enumerate{
#' \item `overall`: Overall summary of effort, parsed by Beaufort sea state, for the entire dataset.
#' Three columns: `Bft`, `km`, `prop` (the fraction of effort for this sea state).
#' \item `by_year`: Same table as `overall`, this time parsed by `year`.
#' \item `by_cruise`: Same table as `overall`, this time parsed by geostratum.
#' \item `details`: Same table as `overall`, this time parsed by each unique `Cruise`-`year`-`stratum` combination.
#' }
#'
#' @export
#'
summarize_bft <- function(cruz,
                          use_only = TRUE,
                          cohort=1){

  #=============================================================================
  # For debugging -- only
  if(FALSE){
    data("cnp_150km_1986_2020")
    cruz <- filter_cruz(cnp_150km_1986_2020,
                        years = 2020,
                        regions = 'HI_EEZ')
    cohort = 1
    use_only = TRUE

    cruz$cohorts$all$das %>% filter(Bft > 6) %>% group_by(use, Bft) %>% tally()

    # test
    summarize_bft(cruz)$overall
    summarize_bft(cruz)
    summarize_bft(cruz, use_only = FALSE)$overall
  }
  #=============================================================================

  # Filter down to the cohort-analysis specified
  survey <- cruz$cohorts[[cohort]]
  names(survey)

  # Get DAS data
  eff <- survey$das

  # Review
  length(eff)
  nrow(eff)
  head(eff)

  if(use_only){
    uses <- TRUE
  }else{
    uses <- c(TRUE, FALSE)
  }
  uses

  suppressWarnings({
    suppressMessages({

      # Dataframe grouping effort by BFT
      bft <-
        eff %>%
        dplyr::filter(use %in% uses) %>%
        #dplyr::mutate(bftr = round(Bft)) %>%
        #dplyr::group_by(bftr) %>%
        dplyr::group_by(Bft) %>%
        dplyr::summarize(km = sum(km_int)) %>%
        dplyr::mutate(prop = km / sum(km))
      bft

      # Grouping by Cruise, year, stratum and bft
      bft_details <-
        eff %>%
        dplyr::filter(use %in% uses) %>%
        #dplyr::mutate(bftr = round(Bft)) %>%
        #dplyr::group_by(Cruise, year, stratum, bftr) %>%
        dplyr::group_by(Cruise, year, stratum, Bft) %>%
        dplyr::summarize(km = sum(km_int)) %>%
        dplyr::mutate(prop = km / sum(km))
      bft_details

      # Grouping by year and bft
      bft_year <-
        eff %>%
        dplyr::filter(use %in% uses) %>%
        #dplyr::mutate(bftr = round(Bft)) %>%
        #dplyr::group_by(year, bftr) %>%
        dplyr::group_by(year, Bft) %>%
        dplyr::summarize(km = sum(km_int)) %>%
        dplyr::mutate(prop = km / sum(km))
      bft_year

      # Grouping by stratum and bft
      bft_stratum <-
        eff %>%
        dplyr::filter(use %in% uses) %>%
        #dplyr::mutate(bftr = round(Bft)) %>%
        #dplyr::group_by(stratum, bftr) %>%
        dplyr::group_by(stratum, Bft) %>%
        dplyr::summarize(km = sum(km_int)) %>%
        dplyr::mutate(prop = km / sum(km))
      bft_stratum

    })
  })

  # Prepare return
  return_list <- list(overall = bft,
                      by_year = bft_year,
                      by_stratum = bft_stratum,
                      details = bft_details)

  # Return
  return(return_list)
}

