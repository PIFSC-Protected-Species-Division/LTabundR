################################################################################
################################################################################
# Example processed dataset:
# CNP 1986 - 2020
################################################################################
################################################################################
# Dependencies

library(dplyr)
library(stringr)
library(devtools)
library(swfscDAS)
library(stringr)

# Load package locally
#document()
#load_all()

################################################################################
# Settings

# Survey-wide settings =========================================================

# Load built-in group size coefficiens
data(group_size_coefficients)

survey <- load_survey_settings(
  out_handling = 'remove',
  max_row_interval = Inf,
  segment_method = "equallength",
  segment_target_km = 150,
  segment_max_interval = 24,
  segment_remainder_handling = c("segment"),
  ship_list = NULL, # use package list
  species_codes = NULL, # use package codes
  group_size_coefficients = group_size_coefficients, # use package coefficients
  smear_angles = FALSE)

# Prep strata ==================================================================

# Load built-in strata
data(strata_cnp)
strata <- strata_cnp
names(strata)


# Cohort 1: Most species =======================================================

# Prep cohort settings
all_species <- load_cohort_settings(
  id = "all",
  species = NULL,
  strata = c('MHI', 'HI_EEZ', 'OtherCNP'),
  probable_species = FALSE,
  sighting_method = 0,
  cue_range = 0:7,
  school_size_range = c(0, 10000),
  school_size_calibrate = TRUE,
  calibration_floor = 0,
  use_low_if_na = TRUE,
  io_sightings = 0,
  geometric_mean_group = TRUE,
  truncation_km = 7.5,
  beaufort_range = 0:6,
  abeam_sightings = FALSE,
  strata_overlap_handling = c("smallest"),
  distance_types = c('S','F','N'),
  distance_modes = c('P','C'),
  distance_on_off = TRUE
)

# Cohort 2 Bottlenose dolphin  =================================================

bottlenose <- load_cohort_settings(
  id = "bottlenose",
  species = '018',
  strata = c('MHI', 'HI_EEZ', 'OtherCNP', 'Bottlenose_BI', 'Bottlenose_OUFI', 'Bottlenose_KaNi'),
  probable_species = FALSE,
  sighting_method = 0,
  cue_range = 0:7,
  school_size_range = c(0, 10000),
  school_size_calibrate = TRUE,
  calibration_floor = 0,
  use_low_if_na = TRUE,
  io_sightings = 0,
  geometric_mean_group = TRUE,
  truncation_km = 7.5,
  beaufort_range = 0:6,
  abeam_sightings = FALSE,
  strata_overlap_handling = c("smallest"),
  distance_types = c('S','F','N'),
  distance_modes = c('P','C'),
  distance_on_off = TRUE
)

# Cohort 3 Pantropical spotted dolphin  ========================================

spotted <- load_cohort_settings(
  id = "spotted",
  species = '002',
  strata = c('MHI', 'HI_EEZ', 'OtherCNP','Spotted_OU','Spotted_FI','Spotted_BI'),
  probable_species = FALSE,
  sighting_method = 0,
  cue_range = 0:7,
  school_size_range = c(0, 10000),
  school_size_calibrate = TRUE,
  calibration_floor = 0,
  use_low_if_na = TRUE,
  io_sightings = 0,
  geometric_mean_group = TRUE,
  truncation_km = 7.5,
  beaufort_range = 0:6,
  abeam_sightings = FALSE,
  strata_overlap_handling = c("smallest"),
  distance_types = c('S','F','N'),
  distance_modes = c('P','C'),
  distance_on_off = TRUE
)

# Cohort 4 False killer whale ==================================================

fkw <- load_cohort_settings(
  id = "pseudorca",
  species = '033',
  strata = c('MHI', 'HI_EEZ', 'OtherCNP','NHWI'),
  probable_species = FALSE,
  sighting_method = 0,
  cue_range = 0:7,
  school_size_range = c(0, 10000),
  school_size_calibrate = TRUE,
  calibration_floor = 0,
  use_low_if_na = TRUE,
  io_sightings = 0,
  geometric_mean_group = TRUE,
  truncation_km = 7.5,
  beaufort_range = 0:6,
  abeam_sightings = FALSE,
  strata_overlap_handling = c("smallest"),
  distance_types = c('S','F','N'),
  distance_modes = c('P','C'),
  distance_on_off = TRUE
)


# Compile `settings`  object ===================================================

settings <- load_settings(strata = strata,
                          survey = survey,
                          cohorts = list(all_species,
                                         bottlenose,
                                         spotted,
                                         fkw))

################################################################################
# Process

data(cnp_1986_2020_edits)

#das_file = '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das'
das_file = "/Users/ekezell/Desktop/projects/noaa ltabundr/CenPac1986-2020_Final_alb.das"

if(exists('cruz')){rm(cruz)}

cruz <- process_surveys(das_file = das_file,
                        settings = settings,
                        edits = cnp_1986_2020_edits,
                        process_sightings = TRUE,
                        process_subgroups = TRUE,
                        save_local = FALSE)

cruz$cohorts$all$das$stratum %>% table
cruz$cohorts$bottlenose$das$stratum %>% table
cruz$cohorts$spotted$das$stratum %>% table
cruz$cohorts$pseudorca$das$stratum %>% table

# Verify
cruz_structure(cruz)
#cruz_explorer(cruz)

summarize_bft(cruz)$overall
summarize_bft(cruz, use_only = FALSE)$overall

cruz$cohorts$all$das %>% filter(Bft > 6) %>% group_by(use, Bft) %>% tally()

# Rename
cnp_150km_1986_2020 <- cruz

# Save as data
usethis::use_data(cnp_150km_1986_2020, overwrite = TRUE)

