## code to prepare `settings` dataset goes here

#library(devtools) ; document() #; load_all()

# Load strata
data(strata_cnp)

# Survey settings
survey <- load_survey_settings(seed=123)

# Load group size coefficients
data(grp_coeff_abund)

# Cohort 1 (default)
cohort1 <- load_cohort_settings(strata = c('OtherCNP', 'HI_EEZ', 'WHICEAS'))

# Load settings
example_settings <- load_settings(strata = strata_cnp,
                          survey = survey,
                          cohorts=list(cohort1))

example_settings

usethis::use_data(example_settings, overwrite = TRUE)
