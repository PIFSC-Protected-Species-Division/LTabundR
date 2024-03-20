#' Pool bootstraps from multiple `lta()` outputs
#'
#' This function allows you to pool, or concatenate, the results of two separate `lta()` runs
#' in order to achieve the desired number of bootstrap iterations. An example use case:
#' you are running a 1,000-iteration `lta()` call overnight, but the power goes out at iteration 900.
#' In the morning you can run a 100-iteration version of the same `lta()` call, then use
#' this function to pool the two results together.
#'
#' @param ltas A `list` of outputs from the function `lta()`.

#' @param bootstraps Desired number of bootstraps to keep. Example use case: You have two `lta()` outputs,
#' one with 600 iterations and another with 500. You want to pool the two outputs but only keep 1,000 iterations total.
#' Specify `bootstraps = 1000` and this function will randomly select 1,000 of the 1,100 bootstraps available.
#'
#' @return An `lta()` output object.
#' The `$estimate` slot will be the exact same as the first slot in the `ltas` input;
#' The `$details` slot will have the pooled bootstrap samples.
#' The `$df` slot will have the pooled detection function curves.
#' The `$summary` slot will have updated summary statistics based on the pooled set of bootstraps.
#'
#' @export
#' @import dplyr
#' @import ggplot2
#'
lta_pool <- function(ltas, bootstraps=NULL){

  if(FALSE){ #==================================================================
    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020

    fit_filters <-
      list(spp = c('013', '026', '031'),
           pool = 'Multi-species pool 1',
           cohort = 'all',
           truncation_distance = 5,
           other_species = 'remove')

    scenarios <- list(list(years = 2017,
                           regions = 'MHI'),
                      list(years = 2020,
                           regions = 'MHI'))
    estimator <- lta_estimates(scenarios)
    estimates <-
      c(estimator(spp = '013', title = "Striped dolphin",
                  g0=.3, g0_cv=.2),
        estimator(spp = '026', title = "Fraser's dolphin",
                  g0=.3, g0_cv=.2),
        estimator(spp = '031', title = "Melon-headed whale",
                  g0=.3, g0_cv=.2))

    lta1 <- lta(cruz, Rg0, fit_filters, df_settings, estimates, bootstraps=5)
    lta2 <- lta(cruz, Rg0, fit_filters, df_settings, estimates, bootstraps=10)

    ltas <- list(lta1, lta2)
    bootstraps <- 12

    ltapooled <- lta_pool(ltas, bootstraps = 12)
    lta_report(ltapooled)
    lta_diagnostics(ltapooled)


  } #===========================================================================

  # Get basis for result object
  ltanew <- ltas[[1]]
  ltanew %>% names
  ltanew$bootstrap$df
  ltanew$bootstrap$details

  # Get the bootstraps
  boots <- list()
  dfs <- list()
  i=1
  for(i in 1:length(ltas)){
    lti <- ltas[[i]]
    lti %>% names
    boots[[i]] <- lti$bootstrap$details
    dfs[[i]] <- lti$bootstrap$df
  }
  length(boots)
  length(dfs)

  # Pool the bootstraps
  boot_df <- data.frame()
  i=2
  for(i in 1:length(boots)){
    booti <- boots[[i]]
    booti %>% head
    if(i > 1){
      (booti$i <- booti$i + (i-1) + (length(unique(boot_df$i)) - 1))
    }else{
      (booti$i <- booti$i + (i-1))
    }
    is <- unique(booti$i)
    message('LTA object ', i,' :: bootstraps ', is[1], ' through ',is[length(is)])
    boot_df <- rbind(boot_df, booti)
  }

  # Unique bootstrap interations
  (is <- boot_df$i %>% unique)
  message('Total bootstraps across supplied LTA objects = ', length(is))

  # Pool the DF
  df_df <- dfs %>% bind_rows
  nrow(df_df)

  # Limit to desired number of bootstraps by random sampling
  if(!is.null(bootstraps)){
    if(bootstraps <= length(is)){
      if(bootstraps == length(is)){
        message('Desired number of bootstraps is equal to number of bootstraps available. No subsampling will be done.')
      }else{
        message('Subsampling bootstraps to desired number (n = ',bootstraps,')...')
        (samp_is <- sample(is, size=bootstraps, replace=FALSE) %>% sort)
        boot_df <- boot_df %>% dplyr::filter(i %in% samp_is)
        df_df <- df_df[samp_is, ]
      }
    }else{
      stop('Desired number of boostraps is greater than available number of bootstraps. Stopping here.')
    }
  }

  # Re-calculate summary
  suppressMessages({
    bs_summary <-
    boot_df %>%
    dplyr::filter(is.finite(N) == TRUE) %>%
    dplyr::group_by(title, Region, year) %>%
    dplyr::summarize(species = paste(unique(species), collapse=', '),
                     iterations = dplyr::n(),
                     ESW_mean = mean(ESW_mean, na.rm=TRUE),
                     g0_mean = mean(g0_est, na.rm=TRUE),
                     g0_cv = sd(g0_est, na.rm=TRUE) / mean(g0_est, na.rm=TRUE),
                     km = mean(km, na.rm=TRUE),
                     ER = mean(ER, na.rm=TRUE),
                     D = mean(D, na.rm=TRUE),
                     size = mean(size_mean, na.rm=TRUE),
                     Nmean = mean(N, na.rm=TRUE),
                     Nmedian = median(N, na.rm=TRUE),
                     Nsd = sd(N, na.rm=TRUE),
                     CV = Nsd / Nmean,
                     L95 = lta_ci(Nmean, N)$bca_lognormal[1],
                     U95 = lta_ci(Nmean, N)$bca_lognormal[2])
  })

  # Check it out
  bs_summary

  # Update result object
  ltanew %>% names
  ltanew$bootstrap %>% names
  ltanew$bootstrap$summary <- bs_summary
  ltanew$bootstrap$details <- boot_df
  ltanew$bootstrap$df <- df_df

  return(ltanew)
}
