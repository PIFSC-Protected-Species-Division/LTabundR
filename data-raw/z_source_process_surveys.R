library(devtools)
document()

# example_settings
source('data-raw/example_settings.R')

# example_cruz
source('data-raw/example_cruz.R')

# cnp_150km_1986-2020
source('data-raw/cruz_cnp_150km_1986-2020.R')

#cnp_150km_1986_2020$cohorts$all$sightings %>%
#  filter(ss_valid == FALSE) %>%
#  filter(OnEffort == TRUE) %>%
#  filter(species !='033') %>%
#  as.data.frame()

# noaa_daily_1986_2020
source('data-raw/cruz_all_day_1986-2020.R')

# noaa_10km_1986_2020
source('data-raw/cruz_all_10km_1986-2020.R')

