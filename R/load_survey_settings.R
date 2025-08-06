#' Load survey settings
#'
#' This function builds a list of survey-wide settings (applying to all segments
#' and all sightings of all species, regardless of their cohort designation),
#' which you will pass to `load_settings()`.
#'
#' @param out_handling This argument allows you to specify
#' how data occurring outside of geo-strata should be handled.
#' If this is set to `"remove"`, those rows will be filtered out of the data early in the process.
#' This reduces memory usage, speeds up processing,
#' and gives you geographic control of how effort and sightings will be summarized.
#' If this is set to `"stratum"`, those data will be assigned to a fake geo-stratum, named `"out"`.
#' Effort in the `"out"` stratum will not be segmentized,
#' but `"out"` sightings will be processed and retained in the final datasets.
#' This setting might be useful if you want to use `"out"` data
#' for survey summaries and/or detection function estimation.
#' The default is `"remove"`, since that saves the most time and memory.
#' If no geostratum is provided, this setting will be ignored and all rows of data
#' will be assigned to a stratum called `"none"`.
#'
#' @param interpolate This argument allows you to interpolate the `DAS` data
#' at the onset of processing if your position updates are separated
#' by large time intervals, which would make spatial effort and stratum assignments less exact.
#' If this argument is `NULL`, then no interpolation will occur. If it is a number, e.g., 30, `LTabundR` will
#' interpolate the data using simple-linear methods (i.e., no great-circle calculations) such that
#' position updates occur every 30 seconds or less. If adjacent `DAS` rows are from different dates or cruises,
#' the interpolation routine will skip to the next pair of related rows. Interpolation will only occur for On-Effort rows.
#'
#' @param min_row_interval The minimum time interval, in seconds, between rows in order for
#' the Great Circle distance between rows to be calculated. Intervals less than this number
#' will be assigned a distance of 0 km.
#'
#' @param max_row_interval The maximum allowable time interval, in seconds,
#' between rows before `LTabundR` assumes that there has been a break in survey data logging.
#' The default of 3600 seconds (6 hours) was chosen because there is usually at least 6 hours
#' of nighttime darkness between the end of effort on one day and the start of effort on the next.
#'
#' @param max_row_km The maximum allowable distance interval, in km,
#' between rows before the function assumes that there has been a break in survey data logging.
#' The default was chosen arbitrarily to find a value that replicates the
#' processing results from `ABUND`.
#'
#' @param speed_filler When speed is not available in the data, this value (in kph)
#' will be used as a filler in order to estimate the
#' distance between consecutive rows of data based on timestamp differences
#' (when lat/long coordinates are not available).
#' The default was chosen arbitrarily to find a value that replicates
#' the processing results from `ABUND`.
#'
#' @param km_filler When valid speed and position information is not available
#' (e.g., the given distance exceeds `max_km_gap`),
#' this value (in km) will be used as an estimate of the
#' distance in between consecutive rows of data.
#' The default was chosen arbitrarily to find a value that replicates the
#' processing results from `ABUND`.
#'
#' @param segment_method The two method options are `"day"` --
#' all effort within the same Cruise-StudyArea-Stratum-Year-Effort scenario (i.e., an effort bloc)
#' will be binned into segments by calendar date -- and `"equallength"` --
#' effort within each unique effort bloc
#' will be divided into segments of approximately equal length.
#'
#' @param segment_target_km If segmentizing by `"equallength"`,
#' this field allows you to specify what that target length is, in km.
#' If segmentizinng by `"day"`, this argument is ignored.
#' The default is 150 km, the distance generally surveyed in one day on NOAA Fisheries surveys.
#'
#' @param segment_max_interval If segmentizing by `"equallength"`,
#' this setting allows you to specify the time gaps in effort
#' that are allowed to be contained within a single segment.
#' For example, if your goal is a few large segments of equal length
#' (e.g., 150-km segments, for bootstrap estimation of density variance),
#' you are probably willing for discrete periods of effort to be concatenated into a single segment,
#' even if the gaps between effort are as large as 1 or 2 days,
#' in which case you would set `segment_max_interval` to 24 or 48 (hours), respectively.
#' However, if your goal is many smaller segments (e.g., 5-km segments, for habitat modeling),
#' you want to ensure that effort is contiguous so that segment locations
#' can be accurately related to environmental variables,
#' in which case you would set `segment_max_interval` to be very small (e.g., 0.2 hours, or 12 minutes).
#' Setting this interval to a small number, such as 0.2, also allows
#' the segmentizing function to overlook momentary breaks in effort,
#' such as when an unofficial observer logs a sighting.
#' If segmentizinng by `"day"`, this argument is ignored.
#'
#' @param segment_remainder_handling If segmentizing by `"equallength"`,
#' periods of effectively-contiguous effort (as specified by `segment_max_interval`)
#' are unlikely to be perfectly divisible by your `segment_target_km`;
#' there is going to be a remainder. You can handle this remainder in three ways:
#' (1) `"disperse"` allows the function to adjust `segment_target_km` so that
#' there is in fact no remainder, effectively dispersing the remainder evenly
#' across all segments within that period of contiguous effort;
#' (2) `"append"` asks the function to append the remainder to a randomly selected segment,
#' such that most segments are the target length with the exception of one longer one;
#' or (3) `"segment"` asks the function to simply place the remainder in its own segment,
#' placed randomly within the period of contiguous effort.
#' This setting also has a second layer of versatility,
#' because it can accept a one- or two-element character vector.
#' If a two-element vector is provided (e.g., `c("append","segment")`),
#' the first element will be used in the event that the remainder is less than or equal to
#' half your `segment_target_km`; if the remainder is more than half that target length,
#' the second element will be used. This feature allows for replication
#' of the segmentizing methods in Becker et al. (2010).
#' If segmentizinng by `"day"`, this argument is ignored.
#'
#' @param seed Set a seed (any integer) to ensure that your survey is processed reproducibly:
#' namely, segments will be chopped the exact same way every time.
#' Some of the segment remainder handling methods (namely `"segment"` and `"append"`) will place the remainder
#' to a randomly selected segment. Supplying a number here will ensure the remainder goes in the same place with each run.
#' If left `NULL`, the segment breaks are liable to differ each time this function is run, and the segments to which
#' sightings are assigned are liable to vary as well.
#'
#' @param ship_list  A `data.frame` containing a list of ship names.
#' If not provided the default version, which was current as of the release of `ABUND9` in 2020, will be used (`data(ships)`).
#' Supplied `data.frames` must match the column naming structure of `data(ships)`.
#'
#' @param species_codes A `data.frame` containing species codes.
#' This is an optional input, chiefly used to format species names
#' in the reporting stage of the workflow (`lta_report()` especially).
#' If the user supplies a `data.frame` it must match the column naming structure of `data(species_codes)`.
#'
#' @param group_size_coefficients A `data.frame` of calibration factors.
#' If not provided, group sizes will not be calibrated.
#' To use the same coefficients that have been in use at SWFSC and PIFSC up to 2021, see `data(group_size_coefficients)`.
#' Supplied `data.frame`'s must match the column naming structure of that built-in dataset.
#'
#' @param smear_angles If `TRUE` (the default is `FALSE`), bearing angles to a
#' group of animals will be "smeared" by adding a uniformly distributed random number between -5 and +5 degrees.
#' This has not been used in any recent analyses because observers have not been rounding angles as much as they used to,
#' according to the release notes for `ABUND9`.
#' It was suggested by Buckland as a method for dealing with rounding, which is
#' especially influential when rounding to zero places many sightings at zero perpendicular distance.
#'
#' @return A list with named slots, equivalent to your input arguments.
#' Save this output to an object, e.g., "`survey_settings`", and pass it to `load_settings()`.
#'
#' @export
#'
load_survey_settings <- function(out_handling = 'remove',
                                 interpolate = NULL,
                                 min_row_interval = 2,
                                 max_row_interval = 3600,
                                 max_row_km = 100,
                                 km_filler = 1,
                                 speed_filler = 10*1.852,
                                 segment_method = 'day',
                                 segment_target_km = 150,
                                 segment_max_interval = 48,
                                 segment_remainder_handling = 'segment',
                                 seed = NULL,
                                 ship_list = NULL,
                                 species_codes = NULL,
                                 group_size_coefficients = NULL,
                                 smear_angles = FALSE){

  # Simply save the input arguments as a named list and return it.
  survey_settings <- as.list(environment())

  return(survey_settings)
}
