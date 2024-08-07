################################################################################
################################################################################
# Example processed dataset:
# All Pacific WinCruz data (CNP, ETP, CCS) 1986 - 2020
# 10km

################################################################################
################################################################################
# Dependencies

library(dplyr)
library(stringr)
library(devtools)
library(swfscDAS)

# Load package locally
#document()

################################################################################
# Settings

# Survey-wide settings =========================================================

# Load built-in group size coefficiens
data(group_size_coefficients)

survey <- load_survey_settings(
  out_handling = 'stratum',
  min_row_interval = 2,
  max_row_interval = 3600,
  max_row_km = 100,
  km_filler = 1,
  speed_filler = 10 * 1.852,
  segment_method = "equallength",
  segment_target_km = 10,
  segment_max_interval = 6,
  segment_remainder_handling = c("segment"),
  group_size_coefficients = group_size_coefficients)


# Load built-in strata  =======================================================

data(strata_ccs)
data(strata_cnp)

(strata <- c(strata_ccs, strata_cnp)) %>% names
strata$NEPAC <- data.frame(Lat = c(-20, -20, 65, 65), Lon = c(-230, -70, -70, -200))
strata %>% names
strata


# Cohort 1: All species =======================================================

# Prep cohort settings
all_species <- load_cohort_settings(
  id = "all",
  species = NULL,
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
                          cohorts = list(all_species))

################################################################################
# Process the data

if(exists('cruz')){rm(cruz)}
if(exists('swfsc')){rm(swfsc)}
if(exists('pifsc')){rm(pifsc)}

# First with SWFSC data
das_file = c("/Users/ekezell/Desktop/projects/noaa ltabundr/swfsc_1986_2020.das")
swfsc <- process_surveys(das_file,
                         settings = settings,
                         process_sightings = TRUE,
                         process_subgroups = TRUE,
                         save_local = FALSE) # change to TRUE if you want to save result as RData file
#cruz_explorer(swfsc)

# Then with PIFSC data
das_file = "/Users/ekezell/Desktop/projects/noaa ltabundr/CenPac1986-2020_Final_alb.das"
data(cnp_1986_2020_edits)
pifsc <- process_surveys(das_file,
                         settings = settings,
                         edits = cnp_1986_2020_edits,
                         process_sightings = TRUE,
                         process_subgroups = TRUE,
                         save_local = FALSE) # change to TRUE if you want to save result as RData file
#cruz_explorer(pifsc)

# But there is some overlap/redundancy in these datasets.
# cruz_combine will check for redundancies and remove them.
cruzes <- list(swfsc, pifsc)
cruz <- cruz_combine(cruzes)

# Verify
#cruz_explorer(cruz)

cruz_structure(cruz)

cruz$cohorts$all$das %>%
  head

cruz$cohorts$all$das$datum_id %>% table %>% table

seg_p <-
  pifsc$cohorts$all$segments %>%
  group_by(Cruise) %>%
  tally() %>%
  rename(n_pifsc = n) %>%
  as.data.frame

seg_s <-
  swfsc$cohorts$all$segments %>%
  group_by(Cruise) %>%
  tally() %>%
  rename(n_swfsc = n) %>%
  as.data.frame

seg_c <-
  cruz$cohorts$all$segments %>%
  group_by(Cruise) %>%
  tally() %>%
  rename(n_combined = n) %>%
  as.data.frame

seg_ps <- full_join(seg_p, seg_s, by='Cruise')
seg_ps

segs <- full_join(seg_ps, seg_c, by='Cruise')
segs

# Rename
noaa_10km_1986_2020 <- cruz

# Save as data
usethis::use_data(noaa_10km_1986_2020, overwrite = TRUE)



