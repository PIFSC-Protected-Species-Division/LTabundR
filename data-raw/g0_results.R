################################################################################
################################################################################
# Relative g0 estimation
################################################################################
################################################################################

# Other dependencies
library(dplyr)
library(stringr)
library(devtools)
library(gsheet)
library(ggplot2)
library(ggpubr)

# Load LTabundR locally (works if you are using RStudio)
#library(rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#document('../../LTabundR/')
#load_all('../../LTabundR/')

################################################################################
# Prep data

document()

# Bring in 10km-segment data from all NOAA/NMFS surveys, 1986 - 2020
data("noaa_10km_1986_2020")

# Filter to analysis only
cruzi <- filter_cruz(noaa_10km_1986_2020,
                     analysis_only = TRUE,
                     eff_types = 'S',
                     bft_range = 0:6,
                     on_off = TRUE)

################################################################################
# Prepare settings list for each species

species <- list(
  list(spp = c('005', '016', '017'),
       title = 'Delphinus spp',
       truncation = 5.5,
       constrain_shape=TRUE),
  list(spp = c('002','006','089','090'),
       title = 'Stenella attenuata spp',
       truncation = 5.5),
  list(spp = c('003','010','011','088','100','101','102','103','107'),
       title = 'Stenella longirostris spp',
       truncation = 5.5),
  list(spp = '013',
       title = 'Striped dolphin',
       truncation = 5.5,
       constrain_shape=TRUE),
  list(spp = '015',
       title = 'Rough-toothed dolphin',
       truncation = 5.5),
  list(spp = '018',
       title = 'Bottlenose dolphin',
       truncation = 5.5,
       constrain_shape=TRUE),
       #pool_bft = '12'),
  list(spp = '021',
       title = "Risso's dolphin",
       truncation = 5.5,
       constrain_shape=TRUE),
       #pool_bft = '12'),
  list(spp = '026',
       title = "Fraser's dolphin",
       truncation = 5.5),
  list(spp = '031',
       title = 'Melon-headed whale',
       truncation = 5.5),
  list(spp = '032',
       title = 'Pygmy killer whale',
       truncation = 5.5),
  list(spp = '036',
       title = 'Short-finned pilot whale',
       truncation = 5.5,
       constrain_shape=TRUE),
       #pool_bft = '12'),
  list(spp = '037',
       title = 'Killer whale',
       truncation = 5.5,
       constrain_shape=TRUE),
       #pool_bft = '12'),
  list(spp = '046',
       title = 'Sperm whale',
       truncation = 5.5),
  list(spp = c('047', '048', '080'),
       title = 'Kogia spp',
       truncation = 4.0),
  list(spp = '061',
       title = "Cuvier's beaked whale",
       truncation = 4.0),
       #pool_bft = '01'),
  list(spp = '049',
       title = 'Unid. beaked whale',
       truncation = 5.5,
       constrain_shape=TRUE),
  list(spp = c('001','051','052','053','054','055',
               '056','057','058','059','060','081',
               '082','083','106','109'),
       title = 'Mesoplodon spp',
       truncation = 4.0,
       constrain_shape=TRUE),
  list(spp = '044',
       title = "Dall's porpoise",
       truncation = 5.5,
       regions = 'CCS'),
  list(spp = '071',
       title = 'Minke whale',
       truncation = 4.0,
       regions='CCS',
       constrain_shape = TRUE),
  list(spp = c('072','073','099'),
       title = "Sei/Bryde's",
       truncation = 5.5),
  list(spp = '074',
       title = 'Fin whale',
       truncation = 5.5,
       regions = 'CCS'),
  list(spp = '075',
       title = 'Blue whale',
       truncation = 5.5,
       regions = 'CCS'),
  list(spp = '076',
       title = 'Humpback whale',
       truncation = 5.5,
       regions = 'CCS'),
  list(spp = c('177','277','377','077'),
       title = 'Unid. dolphin',
       truncation = 5.5,
       constrain_shape=TRUE),
  list(spp = c('078','079','098','096'),
       title = 'Unid. cetacean',
       truncation = 5.5,
       constrain_shape=TRUE))

################################################################################
# Run Rg0 estimation analysis

Rg0 <-
  g0_table(cruzi,
           species,
           eff_types = 'S',
           jackknife_fraction = 0.1)

Rg0 %>% head

#Rg0 %>% View
################################################################################
# Plot result

g0_plot(Rg0, panes = 3)


#### Explore / QA-QC

cruzi <- filter_cruz(noaa_10km_1986_2020,
                     analysis_only = TRUE,
                     years = 1986:2020,
                     regions = 'CCS',
                     eff_types = 'S',
                     bft_range = 0:6,
                     on_off = TRUE)

bw <- list(list(spp = '071',
           title = 'Blue whale',
           truncation = 4.0,
           constrain_shape = TRUE,
           regions = 'CCS'))

g0_table(cruzi, species = bw)


cruz_explorer(cruzi)

################################################################################
# Save result

#save(Rg0, file='../test_code/eric/whiceas/Rg0.RData')
#head(result)

Rg0
g0_results <- Rg0
usethis::use_data(g0_results, overwrite = TRUE)

