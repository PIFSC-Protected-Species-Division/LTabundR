#' Species codes used in WinCRUZ
#'
#' @details A `data.frame` containing species codes and associated identifiers.
#' @format A `data.frame` with 164 rows and 5 variables:
#' \describe{
#'   \item{code}{Code used in WinCRUZ}
#'   \item{short_name}{Abbreviated name, in all CAPS}
#'   \item{scientific_name}{Latin Genus species}
#'   \item{common}{Most common name}
#'   \item{description}{Alternative common names}
#' }
"species_codes"
#'
#' Ship codes
#' @details A `data.frame` matching the ship name initials to the NOAA-NFMS cruise number.
"ships"
#'
#' Coastline
#' @details World coastline, downloadeded from Natural Earth.
"coastline"
#'
#' Land
#' @details World dataset of land polygons, downloaded from Natural Earth.
"land"
#'
#' EEZ (all US EEZ boundaries)
#' @details World dataset of geopolitical Exclusive Economic Zones (EEZ), downloaded from Natural Earth.
"eez"
#'
#' EEZ - California Current System (formatted for sf / tmap mapping)
#' @details The EEZ relevant to the California Current System only, formatted for `sf`-compatibility.
"eez_ccs"
#'
#' EEZ - Hawaii (formatted for sf / tmap mapping)
#' @details The EEZ relevant to the Hawai'ian area only, formatted for `sf`-compatibility.
"eez_hawaii"
#'
#' Strata CCS
#' @details Typical geostrata used in the California Current System region.
"strata_ccs"
#'
#' Strata CNP
#' @details Typical geostrata used in the Central North Pacific region.
"strata_cnp"
#'
#' Strata ETP
#' @details Typical geostrata used in the Eastern Tropical Pacific region.
"strata_etp"
#'
#' Example settings used in `LTabundR` vignette (WHICEAS 2020)
#' @details This example `settings` object was prepared with the following code:
#' ```
#' data(strata_cnp)
#' data(study_cnp)
#' data(group_size_coefficients)
#' survey <- load_survey_settings()
#' cohort1 <- load_cohort_settings(strata = c('OtherCNP', 'HI_EEZ', 'WHICEAS'))
#' example_settings <- load_settings(strata = strata_cnp,
#'                                study_area = study_cnp,
#'                                survey = survey,
#'                                cohorts=list(cohort1))
#' ```
"example_settings"
#'
#' Example cruz object (WHICEAS 2020)
#' @details This example `cruz` object was prepared with the following code:
#' ```
#' data(example_settings)
#' example_cruz <- process_surveys('data-raw/data/HICEASwinter2020.das',
#'                           settings = example_settings)
#' ```
"example_cruz"
#'
#' Cruise data processed for the CNP, 1986 - 2020 (150 km segment lengths)
#' @details This dataset was processed with the following code:
#' ```
#' data(group_size_coefficients)
#'
#' survey <- load_survey_settings(
#' out_handling = 'remove',
#' max_row_interval = Inf,
#' segment_method = "equallength",
#' segment_target_km = 150,
#' segment_max_interval = 24,
#' segment_remainder_handling = c("segment"),
#' ship_list = NULL, # use package list
#' species_codes = NULL, # use package codes
#' group_size_coefficients = group_size_coefficients, # use package coefficients
#' smear_angles = FALSE)
#'
#' data(strata_cnp)
#' strata <- strata_cnp
#'
#' all_species <- load_cohort_settings(
#' id = "all",
#' species = NULL, #spp_codes,
#' strata = c('MHI', 'WHICEAS', 'HI_EEZ', 'OtherCNP'),
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' bottlenose <- load_cohort_settings(
#' id = "bottlenose",
#' species = '018',
#' strata = c('MHI', 'WHICEAS', 'HI_EEZ', 'OtherCNP', 'Bottlenose_BI', 'Bottlenose_OUFI', 'Bottlenose_KaNi'),
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' spotted <- load_cohort_settings(
#' id = "spotted",
#' species = '002',
#' strata = c('MHI', 'WHICEAS', 'HI_EEZ', 'OtherCNP','Spotted_OU','Spotted_FI','Spotted_BI'),
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' fkw <- load_cohort_settings(
#' id = "pseudorca",
#' species = '033',
#' strata = c('MHI', 'WHICEAS', 'HI_EEZ', 'OtherCNP','NHWI'),
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' settings <- load_settings(strata = strata,
#'                           survey = survey,
#'                           cohorts = list(all_species,
#'                                          bottlenose,
#'                                          spotted,
#'                                          fkw))
#'
#' das_file = '../test_code/CNP/CenPac1986-2020_Final_alb.das'
#'
#' cnp_150km_1986_2020 <- process_surveys(das_file = das_file,
#'                      settings = settings)
#' ```
"cnp_150km_1986_2020"
#'
#' Cruise data processed for all NOAA SWFSC and PIFSC surveys, 1986 - 2020 (daily segment lengths)
#' @details This dataset was processed using the following code:
#' ```
#' data(group_size_coefficients)
#'
#' survey <- load_survey_settings(
#' out_handling = 'stratum',
#' max_row_interval = Inf,
#' segment_method = "day",
#' group_size_coefficients = group_size_coefficients)
#'
#' data(strata_ccs)
#' data(strata_cnp)
#' strata <- c(strata_ccs, strata_cnp)
#' strata$NEPAC <- data.frame(Lat = c(-20, -20, 65, 65), Lon = c(-230, -70, -70, -200))
#'
#' all_species <- load_cohort_settings(
#' id = "all",
#' species = NULL,
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' settings <- load_settings(strata = strata,
#'                        study_area = NULL,
#'                        survey = survey,
#'                        cohorts = list(all_species))
#'
#' das_file = c('../test_code/data/swfsc_1986_2020.das')
#' swfsc <- process_surveys(das_file,
#'                       settings = settings)
#'
#' das_file = '../test_code/CNP/CenPac1986-2020_Final_alb.das'
#' pifsc <- process_surveys(das_file,
#'                       settings = settings)
#'
#' cruzes <- list(swfsc, pifsc)
#' noaa_daily_1986_2020 <- cruz_combine(cruzes)
#' ```
"noaa_daily_1986_2020"
#'
#' Cruise data processed for all NOAA SWFSC and PIFSC surveys, 1986 - 2020 (10 km segment lengths)
#' @details This dataset was processed using the following code:
#' ```
#' data(group_size_coefficients)
#'
#' survey <- load_survey_settings(
#' out_handling = 'stratum',
#' max_row_interval = Inf,
#' segment_method = "equallength",
#' segment_target_km = 10,
#' segment_max_interval = 6,
#' segment_remainder_handling = c("segment"),
#' group_size_coefficients = group_size_coefficients)
#'
#' data(strata_ccs)
#' data(strata_cnp)
#' strata <- c(strata_ccs, strata_cnp)
#' strata$NEPAC <- data.frame(Lat = c(-20, -20, 65, 65), Lon = c(-230, -70, -70, -200))
#'
#' all_species <- load_cohort_settings(
#' id = "all",
#' species = NULL,
#' probable_species = FALSE,
#' sighting_method = 0,
#' cue_range = 0:7,
#' school_size_range = c(0, 10000),
#' school_size_calibrate = TRUE,
#' calibration_floor = 0,
#' use_low_if_na = TRUE,
#' io_sightings = 0,
#' geometric_mean_group = TRUE,
#' truncation_km = 7.5,
#' beaufort_range = 0:6,
#' abeam_sightings = FALSE,
#' strata_overlap_handling = c("smallest"),
#' distance_types = c('S','F','N'),
#' distance_modes = c('P','C'),
#' distance_on_off = TRUE
#' )
#'
#' settings <- load_settings(strata = strata,
#'                        study_area = NULL,
#'                        survey = survey,
#'                        cohorts = list(all_species))
#'
#' das_file = c('../test_code/data/swfsc_1986_2020.das')
#' swfsc <- process_surveys(das_file,
#'                       settings = settings)
#'
#' das_file = '../test_code/CNP/CenPac1986-2020_Final_alb.das'
#' pifsc <- process_surveys(das_file,
#'                       settings = settings)
#'
#' cruzes <- list(swfsc, pifsc)
#' noaa_10km_1986_2020 <- cruz_combine(cruzes)
#' ```
"noaa_10km_1986_2020"
#'
#' Example of LTA results for Striped Dolphin, 2010 and 2017
#' @details Details to come.
"lta_result"
#'
#' Group size coefficients
#' @details A table of group size calibration coefficient values for a variety of species
#' for a variety of observer codes. This table was provided by Jay Barlow (NOAA NMFS Bioligist), was created
#' prior to 2016, and has been used for published analyses through 2021.
"group_size_coefficients"
#'
#' Relative trackline detection probabilities -- Rg(0)
#' @details Rg(0) estimates for 25 species groups, based on survey data from 1986-2020 (see
#' `noaa_10km_1986_2020` dataset).
"g0_results"
#'
#' Relative trackline detection probabilities from Barlow (2015).
#' @details This dataset holds the results from Barlow (2015), "Inferring trackline detection
#' probabilities, g(0), for cetaceans from apparent densities in different survey
#' conditions" (*Marine Mammal Science*), which used NOAA/NMFS cruise data from
#' 1986 to 2010.
"barlow_2015"
#'
