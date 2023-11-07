library(dplyr)
library(LTabundR)
library(ggplot2)

data(species_codes)
data(ships)
data(group_size_coefficients)

das_file = '../LTAvignette/data/surveys/CenPac1986-2020_Final_alb.das'

# Edits ========================================================================

edit_1607_55 <-
  list(das_file = das_file,
       type = 'copy',
       rows = 128111,
       chars = NULL,
       edit = 128118)

edit_1607_68 <-
  list(das_file = das_file,
       type = 'copy',
       rows = c(129982, 129983 , 129985),
       chars = NULL,
       edit = 129987)

edit_1621_245 <-
  list(das_file = das_file,
       type = 'copy',
       rows = 271932:271933,
       chars = NULL,
       edit = 271937)

edit_1004_gmt10 <-
  list(das_file = das_file,
       type = 'function',
       rows = 433327:437665,
       chars = 6:39,
       edit = 'function(x){das_time(x, tz_adjust = 10)$dt}')

# Test it
# edits <- list(edit_1004_gmt10)
# dase <- das_editor(edits)
#
# # Before edit
# das <- das_readtext(das_file)
# das$das[433326:433330] # start of cruise
# das$das[437664:437667] # end of cruise
#
# # After edit
# dase$das[[1]]$das$das[433326:433330]
# dase$das[[1]]$das$das[437664:437667]
#
# das_inspector(das_file)
#

edits <- list(edit_1607_55,
              edit_1607_68,
              edit_1621_245,
              edit_1004_gmt10)

# Process objects ==============================================================

survey <- load_survey_settings(
  out_handling = 'remove',
  max_row_interval = Inf,
  segment_method = "equallength",
  segment_target_km = 150,
  segment_max_interval = 24,
  segment_remainder_handling = c("segment"),
  ship_list = ships,
  species_codes = species_codes,
  group_size_coefficients = group_size_coefficients,
  smear_angles = FALSE
)

data(strata_cnp)

all_species <- load_cohort_settings(
  id = "all", # *
  species = NULL,
  strata = c('WHICEAS', 'HI_EEZ', 'OtherCNP'), # *
  probable_species = FALSE,
  sighting_method = 0,
  cue_range = 0:7,
  school_size_range = c(0, 10000),
  school_size_calibrate = TRUE,
  calibration_floor = 0,
  use_low_if_na = TRUE,
  io_sightings = 0,
  geometric_mean_group = TRUE,
  truncation_km = 7.5, # *
  beaufort_range = 0:6,
  abeam_sightings = TRUE,
  strata_overlap_handling = c("smallest"),
  distance_types = c('S','F','N'),
  distance_modes = c('P','C'),
  distance_on_off = TRUE
)

settings <- load_settings(strata = strata_cnp,
                          survey = survey,
                          cohorts = list(all_species))

save_local=FALSE
process_sightings = TRUE
process_subgroups = TRUE
#edits <- NULL

# In process_surveys, run up to das_load

das %>%
  filter(Cruise == 1004) %>%
  filter(line_num %in% 435621:435624)
  #head(20)

# Run up to process_sightings

cruz$cohorts$all$sightings %>%
  filter(Cruise == 1004)




