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
  #1
  list(spp = c('005', '016', '017'),
       title = 'Delphinus spp',
       truncation = 5.5,
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #2
  list(spp = c('002','006','089','090'),
       title = 'Stenella attenuata spp',
       truncation = 5.5),
  #3
  list(spp = c('003','010','011','088','100','101','102','103','107'),
       title = 'Stenella longirostris spp',
       truncation = 5.5),
  #4
  list(spp = '013',
       title = 'Striped dolphin',
       truncation = 5.5,
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #5
  list(spp = '015',
       title = 'Rough-toothed dolphin',
       truncation = 5.5),
  #6
  list(spp = '018',
       title = 'Bottlenose dolphin',
       truncation = 5.5,
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #pool_bft = '12'),
  #7
  list(spp = '021',
       title = "Risso's dolphin",
       truncation = 5.5,
       constrain_shape=TRUE), #################### use to be TRUE before plogis update
  #pool_bft = '12'),
  #8
  list(spp = '026',
       title = "Fraser's dolphin",
       truncation = 5.5),
  #9
  list(spp = '031',
       title = 'Melon-headed whale',
       truncation = 5.5),
  #10
  list(spp = '032',
       title = 'Pygmy killer whale',
       truncation = 5.5),
  #11
  list(spp = '036',
       title = 'Short-finned pilot whale',
       truncation = 5.5,
       constrain_shape=TRUE), #################### use to be TRUE before plogis update
  #pool_bft = '12'),
  #12
  list(spp = '037',
       title = 'Killer whale',
       truncation = 5.5,
       constrain_shape=TRUE), #################### use to be TRUE before plogis update
  #pool_bft = '12'),
  #13
  list(spp = '046',
       title = 'Sperm whale',
       truncation = 5.5),
  #14
  list(spp = c('047', '048', '080'),
       title = 'Kogia spp',
       truncation = 4.0,
       constrain_shape = FALSE), #################### use to be TRUE before plogis update
  #15
  list(spp = '061',
       title = "Cuvier's beaked whale",
       truncation = 4.0,
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #16
  list(spp = '049',
       title = 'Unid. beaked whale',
       truncation = 5.5,
       constrain_shape=TRUE), #################### use to be TRUE before plogis update
  #17
  list(spp = c('001','051','052','053','054','055',
               '056','057','058','059','060','081',
               '082','083','106','109'),
       title = 'Mesoplodon spp',
       truncation = 4.0,
       constrain_shape=TRUE), #################### use to be TRUE before plogis update
  #18
  list(spp = '044',
       title = "Dall's porpoise",
       truncation = 5.5,
       regions = 'CCS',
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #19
  list(spp = '071',
       title = 'Minke whale',
       truncation = 4.0,
       regions='CCS',
       constrain_shape = FALSE), #################### use to be TRUE before plogis update
  #20
  list(spp = c('072','073','099'),
       title = "Sei/Bryde's",
       truncation = 5.5),
  #21
  list(spp = '074',
       title = 'Fin whale',
       truncation = 5.5,
       regions = 'CCS'),
  #22
  list(spp = '075',
       title = ' Blue whale',
       truncation = 5.5,
       regions = 'CCS'),
  #23
  list(spp = '076',
       title = 'Humpback whale',
       truncation = 5.5,
       regions = 'CCS'),
  #24
  list(spp = c('177','277','377','077'),
       title = 'Unid. dolphin',
       truncation = 5.5,
       constrain_shape=FALSE), #################### use to be TRUE before plogis update
  #25
  list(spp = c('078','079','098','096'),
       title = 'Unid. cetacean',
       truncation = 5.5,
       constrain_shape=FALSE))  #################### use to be TRUE before plogis update

################################################################################
# Run Rg0 estimation analysis

jkf <- 0.1
jkf <- 0

spp1_5 <- g0_table(cruzi,
                   species[1:5],
                   eff_types = 'S',
                   jackknife_fraction = jkf,
                   seed = 123)
spp6_10 <- g0_table(cruzi,
                    species[6:10],
                    eff_types = 'S',
                    jackknife_fraction = jkf,
                    seed = 123)
spp11_15 <- g0_table(cruzi,
                     species[11:15],
                     eff_types = 'S',
                     jackknife_fraction = jkf,
                     seed = 123)
spp16_20 <- g0_table(cruzi,
                     species[16:20],
                     eff_types = 'S',
                     jackknife_fraction = jkf,
                     seed = 123)
spp21_25 <- g0_table(cruzi,
                     species[21:25],
                     eff_types = 'S',
                     jackknife_fraction = jkf,
                     seed = 123)

(Rg0 <- rbind(spp1_5, spp6_10, spp11_15, spp16_20, spp21_25))

# Rg0 <-
#   g0_table(cruzi,
#            species,
#            eff_types = 'S',
#            jackknife_fraction = 0.1,
#            seed = 123)
g0_plot(Rg0, panes = 3)


################################################################################
#### Explore / QA-QC

table3 <-
  Rg0 %>%
  mutate(`Trunc.` = 5.5) %>%
  group_by(title) %>%
  mutate(n = format(sum(sits), big.mark=',')) %>%
  mutate(Rg0 = stringr::str_pad(round(Rg0, 3), side='left',width=4, pad='0')) %>%
  mutate(Rg0_CV = stringr::str_pad(round(Rg0_CV, 3), side='left',width=4, pad='0')) %>%
  ungroup() %>%
  select(Species = title, n, `Trunc.`, bft, Rg0, Rg0_CV) %>%
  mutate(bft = paste0('Bft ',bft)) %>%
  mutate(g0 = paste0(Rg0,' (',Rg0_CV, ')')) %>%
  pivot_wider(id_cols = Species:`Trunc.`,
              names_from = bft,
              values_from = g0) %>%
  mutate(`Bft 0` = '1.000 (0)')

table3$`Trunc.`[c(14, 15, 17, 19)] <- '4.0'

table3 <-
  table3 %>%
  filter(Species != 'Delphinus spp',
         Species != 'Stenella longirostris spp',
         Species != "Fraser's dolphin",
         Species != 'Melon-headed whale',
         Species != 'Killer whale',
         Species != "Cuvier's beaked whale",
         Species != "Dall's porpoise",
         Species != "Unid. beaked whale",
         Species != "Unid. dolphin",
         Species != "Unid. cetacean",
         Species != " Blue whale",
         Species != "Minke whale") %>%
  mutate(Species = ifelse(Species == 'Bottlenose dolphin',
                          'Common bottlenose dolphin',
                          Species)) %>%
  mutate(Species = ifelse(Species == 'Stenella attenuata spp',
                          'Pantropical spotted dolphin',
                          Species)) %>%
  mutate(Species = ifelse(Species == 'Kogia spp',
                          '\\emph{Kogia} spp',
                          Species)) %>%
  mutate(Species = ifelse(Species == 'Mesoplodon spp',
                          '\\emph{Mesoplodon} spp',
                          Species))

library(tinytable)
tt(table3,
   width=c(.25, .05, .05, .06, .08, .08, .08, .08, .08, .08)) %>%
  style_tt(fontsize = .7) %>%
  theme_latex(inner = "rowsep=3pt, colsep=3pt")



if(FALSE){
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
}

################################################################################
# Save result

#save(Rg0, file='../test_code/eric/whiceas/Rg0.RData')
#head(result)

Rg0
g0_results <- rbind(Rg0, sp036)

if(FALSE){
  data(g0_results)
  # fix one-time problem - 036 duplicated
  which(g0_results$spp == '036' & g0_results$bft == 0)
  which(g0_results$spp == '036' & g0_results$bft == 6)
  g0_results <- g0_results[-c(176:182),]
  g0_results %>% filter(spp == '036')
}

usethis::use_data(g0_results, overwrite = TRUE)

