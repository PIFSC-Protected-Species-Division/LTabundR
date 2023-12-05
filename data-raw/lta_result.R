# Example of LTA bootstrap result

# Other dependencies
library(dplyr)
library(stringr)
library(devtools)
library(ggplot2)
library(usethis)

# Re-load LTabundR package locally
#document() ; load_all()

# Bring in cruz object
data("cnp_150km_1986_2020")
cruz <- cnp_150km_1986_2020
cruz$strata

data('g0_results')
Rg0 <- g0_results

################################################################################
# Striped dolphin example
################################################################################

# Setup inputs

fit_filters = list(spp = c('013', '026', '031'), # striped, frasers, melon-headed
                   pool = 'Multi-species pool 1',
                   cohort = 'all',
                   truncation_distance = 5,
                   other_species = 'remove',
                   years = 1986:2017,
                   regions = NULL,
                   not_regions = NULL)

df_settings = list(covariates = c('bft','lnsstot','cruise','year','ship','species'),
                   covariates_factor = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                   covariates_levels = 2,
                   covariates_n_per_level = 10,
                   detection_function_base = 'hn',
                   base_model = '~1',
                   delta_aic = 2)

estimates <-
  list(
    list(spp = '013',
         title = 'Striped dolphin',
         years = 2010,
         regions = 'HI_EEZ',
         g0 = 0.33, g0_cv = 0.20),
    list(spp = '013',
         title = 'Striped dolphin',
         years = 2017,
         regions = 'HI_EEZ',
         g0 = 0.32, g0_cv = 0.21),
    list(spp = '026',
         title = "Fraser's dolphin",
         years = 2010,
         regions = 'HI_EEZ',
         g0 = 0.33, g0_cv = 0.20),
    list(spp = '026',
         title = "Fraser's dolphin",
         years = 2017,
         regions = 'HI_EEZ',
         g0 = 0.32, g0_cv = 0.21),
    list(spp = '031',
         title = 'Melon-headed whale',
         years = 2010,
         regions = 'HI_EEZ',
         g0 = 0.33, g0_cv = 0.20),
    list(spp = '031',
         title = 'Melon-headed whale',
         years = 2017,
         regions = 'HI_EEZ',
         g0 = 0.32, g0_cv = 0.21))

################################################################################
# Run

result <- lta(cruz,
              Rg0,
              fit_filters,
              df_settings,
              estimates,
              use_g0 = TRUE,
              bootstraps = 100,
              toplot=TRUE,
              verbose=TRUE)

################################################################################
# Review result

result$estimate
result$df %>% names
result$df$best_models
result$df$sample_size

result$bootstrap$summary %>% as.data.frame
result$bootstrap$details
nrow(result$bootstrap$details)

#lta_report(result)$table4

#lta_result <- result
getwd()
setwd("/Users/ekezell/repos/LTabundR")
usethis::use_data(lta_result, overwrite = TRUE)

