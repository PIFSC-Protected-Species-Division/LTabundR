#' Load cohort-specific settings
#'
#' This function builds a list of cohort-specific settings -- applying only to the analyses
#' for a certain group of species -- which you will pass to `load_settings()`.
#'
#' @param id An informal identifier for this cohort,
#' to help you keep track of which cohort is which.
#' For example, settings for a cohort of large whales species could be named `"big_whales"`;
#' settings for small delphinids and phocoenids could be named `"small_odontocetes"`;
#' settings for beaked whales could be named `"beakers"`.
#'
#' @param species A character vector of species codes to include in this cohort.
#' If `NULL` (the default), all species will be included.
#'
#' @param strata A character vector of geostratum names; these must match the names
#' listed in the `strata` slot of your survey settings (see documentation for `load_settings()`).
#' If `NULL` (the default), *all* geostrata in your survey settings will each be used.
#' This argument is an opportunity to subset the geostrata used for a cohort;
#' for example, certain dolphin species in Hawaiian waters have unique geostrata that
#' should apply to them but not to other species.
#'
#' @param probable_species If `TRUE` (default is `FALSE`),
#' the “probable” species identifications will be used in place of the “unidentified” categories.
#'
#' @param sighting_method A coded integer which determines which sightings
#' will be included based on how they were first seen.
#' Allowable codes are
#' `0`=any method,
#' `1`=with 25X only,
#' `2`=neither with 25x binoculars nor from the helicopter (i.e., naked eyes and 7x binoculars only).
#' These codes match those used in `ABUND7/9`.
#'
#' @param cue_range Numeric vector of acceptable "observation cues"
#' for sightings used in estimates of abundance.
#' (`0`=this detail is missing in the data,
#' `1`=associated birds,
#' `2`=splashes,
#' `3`=body of the marine mammal,
#' `4`=associated vessel,
#' `5`=?,
#' `6`=blow / spout,
#' `7`=associated helicopter).
#' These codes match those used in `ABUND7/8/9`.
#'
#' @param group_size_range Minimum and maximum group sizes to be included in estimates of abundance.
#' This is the overall group size, not the number of the given species that are present in a mixed group.
#'
#' @param group_size_calibrate If `NULL`, group sizes will not be calibrated.
#' Otherwise supply a `list` to specify which calibration approach to use.
#' Currently two approaches are supported: that used by `ABUND 7/8` (the default shown),
#' which takes an observer-specific, species-agnostic approach,
#' and that developed in Gerrodette et al. (2019) and applied in Barlow et al. (2026),
#' which includes a species-specific, observer-agnostic approach. See Details below.
#'
#' @param use_low_if_na If this setting is `TRUE`,
#' and if an observer does not make a best estimate of group size,
#' mean group size will be calculated from "low" estimates.
#' This will be done only if no observer has a "best" estimate.
#'
#' @param io_sightings A coded integer which specifies how sightings by the independent observer will be handled.
#' Allowable codes, which are inherited from those used in `ABUND7/9`, are
#' `"-1"`=include independent observer sightings with all other sightings,
#' `"0"`=ignore sightings by independent observer,
#' `"1"`=use only sightings made by regular observer team WHEN an independent observer was present,
#' `"2"`=include only sightings made by the independent observer.
#' IO sightings are typically used only for making g(0) estimates,
#' otherwise IO sightings are usually ignored (code = `"0"`).
#'
#' @param geometric_mean_group This argument accepts a Boolean;
#' if `TRUE`, geometric mean school sizes will be calculated instead of arithmetic means.
#' Also, if school size calibration is carried out, the geometric mean will be weighted by calibration variance,
#' such that estimates from observers with low variance will receive more weight.
#' Barlow, Gerrodette, and Perryman (1998) found that using the geometric mean
#' yielded slightly better performance than a simple arithmetic mean group size.
#'
#' @param truncation_km Specifies the maximum perpendicular distance
#' for groups that are to be included for abundance estimation.
#' Also determines the bins used for grouped perpendicular distances.
#'
#' @param beaufort_range A numeric vector indicating the Beaufort sea states (0 - 7) to be accepted within usable segments.
#'
#' @param abeam_sightings If `TRUE`, sightings that occur aft of beam are included
#' in estimating the detection function and densities. Default is `FALSE`:
#' all abeam sightings will not be used in density estimation or detection function estimation.
#'
#' @param strata_overlap_handling This setting informs how effort is split into
#' segments when surveys cross stratum boundaries, and also which stratum name
#' is assigned to each row of data. See Details below.
#'
#' @param distance_types  A character vector of the full-range of effort types that meet the "analysis
#' inclusion criteria", i.e., will be included in detection function estimation,
#' and therefore considered in effort segmentizing.
#' Accepted values are `"S"` (systematic/standard effort), `"F"` (fine-scale effort),
#' and `"N"` (non-systematic/non-standard effort, in which systematic protocols
#' are being used but effort is not occurring along design-based transect routes).
#' Note that it is possible to further subset the effort types specifically for
#' density estimation in `LTabundR`'s function for line transect analysis, `lta()`.
#'
#' @param distance_modes  The effort modes that meet "analysis inclusion criteria", i.e.,
#' will be included in detection function estimation,
#' and therefore considered in effort segmentizing.
#' Accepted values are `"P"` (passing) and `"C"` (closing)
#'
#' @param distance_on_off  The value(s) of `OnEffort`
#' (On Effort is `TRUE`, Off Effort is `FALSE`) that will be included in
#' detection function estimation,
#' and therefore considered in effort segmentizing.
#'
#' @details More information on the `list` required for the **`group_size_calibrate`** input:
#' The `list` requires the following fields (examples are given in the built-in datasets
#' `grp_ops_abund` and `grp_ops_gerrodette`):
#' \itemize{
#' \item `method`: accepted values = 'ABUND' and 'Gerrodette';
#'
#' \item `coefficients`: a `data.frame`. For the "ABUND" method, this is a table of observer-specific calibration coefficients.
#' To use the same coefficients that have been in use at SWFSC and PIFSC up to 2021,
#' see the built-in dataset `data(grp_coeff_abund)`.
#' For the "Gerrodette" method, this is a table of species-specific coefficients.
#' To use the same coefficients from the method's originating papers, see the
#' built-in dataset `data(grp_coeff_gerrodette)`.
#' For both methods, supplied `data.frame`'s must match the column naming structure of the respective built-in dataset.
#'
#' \item `floor`: minimum group size to which calibration will be applied.
#' In the "ABUND" approach, this is referenced only for observers who do not
#' have an entry in the `coefficients` table, which has a calibration floor for each observer.
#' The default is 0, meaning that calibration will be attempted for *all*
#' school size estimates, regarding of the raw estimate.
#'
#' \item `intercept` (ignored if not using the "Gerrodette" method) the value to
#' use for the "intercept" coefficient if a species in this cohort is not present in the `coefficients` table.
#'
#' \item `beta` (ignored if not using the "Gerrodette" method): the value to
#' use for the "beta" coefficient if a species in this cohort is not present in the `coefficients` table.
#'
#' \item `beta_mixed` (ignored if not using the "Gerrodette" method): used for mixed-species groups.
#' If `NULL`, the "beta" coefficient for the most abundant species in the group will be used.
#' If a numeric value is supplied here, that value will be used instead.
#' }
#' Note that the high and low estimates are *never* calibrated; only the best estimates are.
#'
#' More information on the `list` required for the **`strata_overlap_handling`** input:
#' Note that the main impact of this setting is
#' on how effort is broken into segments; the assigned stratum name is for display
#' only and will not constrain options for including/excluding strata in analyses
#' farther along in the `LTabundR` workflow.
#'
#' \itemize{
#' \item The default option is `"smallest"`,
#' which means that effort will always be assigned to the smallest stratum when
#' multiple strata overlap spatially. This is a safe option for surveys with "nested"
#' strata (such as the Central North Pacific strata used by NOAA Fisheries.
#'
#' \item Another option is `"each"`in which each time a stratum boundary is crossed the
#' current segment will end and a new segment will begin.
#' Also, stratum assignments for each row of effort will be shown as a concatenation
#' of all the stratum layers overlapping at its position (e.g., "OtherCNP&HI_EEZ").
#' Note that the `"each"` option segmentizes effort in the exact same was as `"smallest"`
#' when strata are fully nested; its main advantage is in dealing with partially
#' overlapping strata.
#'
#' \item The third option is `"largest"`, in which the largest of
#' overlapping strata is used to assign a stratum name to each row.
#' (We are not sure what use case this would serve,
#' but we offer it as an option for niche analyses.)
#' }
#'
#' @return A list with named slots, equivalent to your input arguments.
#' Save this output to an object, e.g., using the same name you provided in the `id` argument,
#' and pass it to `load_settings()`.
#'
#' @export
#'
load_cohort_settings <- function(id='default',
                                 species=NULL,
                                 strata = NULL,
                                 probable_species = FALSE,
                                 sighting_method = 0,
                                 cue_range = 0:7,
                                 group_size_range = c(0,10000),
                                 group_size_calibrate = grp_ops_abund,
                                 use_low_if_na = FALSE,
                                 io_sightings = 0,
                                 geometric_mean_group = TRUE,
                                 truncation_km = 5.5,
                                 beaufort_range = 0:6,
                                 abeam_sightings = FALSE,
                                 strata_overlap_handling = 'smallest',
                                 distance_types = c('S','F','N'),
                                 distance_modes = c('P','C'),
                                 distance_on_off = TRUE
                                 ){

  # Simply save the input arguments as a named list and return it.
  cohort_settings <- as.list(environment())

  return(cohort_settings)
}
