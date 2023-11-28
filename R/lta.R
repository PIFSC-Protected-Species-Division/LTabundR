#' Line transect analysis
#'
#' For a single species or species pool, fit a detection function and estimate density / abundance,
#' with an option to conduct parametric / non-parametric bootstrap sampling for variance estimation.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' Ensure that this `cruz` object is filtered only to the years, regions, and sighting conditions
#' you would like to use for detection function fitting. Filter your `cruz` object with
#' full flexibility using `LTabundR::filter_cruz()`. Note that filtering for detection function fitting
#' is typically less stringent than filtering for downstream steps for abundance estimation,
#' since as many sightings are included as possible to combat low sample sizes, as
#' long as sightings were observed using standard methods in an unbiased search pattern,
#' and as long as you do not expect detectability to vary across years and regions.
#'
#' @param Rg0 The result of `LTabundR::g0_model()`, which is a `data.frame` with Relative
#' trackline detection probabilities, `g(0)`, for each species in each Beaufort sea state.
#' See `LTabundR` dataset `data(g0_results)` as an example.
#' If not provided, weighted `g(0)` will not be estimated in this function call,
#' and unless you manually provide `g(0)` values within your `estimates` input,
#' `g(0)` will be assumed to be 1 and `g(0)_cv` will be assumed to be 0.
#'
#' @param fit_filters A named list, with filters and settings pertaining to the data
#' used to fit a detection function model.
#' The slots below are recognized, but only `spp`, `cohort`, and `truncation_distance`
#' are required (i.e., do not have defaults).
#' \itemize{
#' \item `spp`: A character vector of species codes. Using multiple species codes may be useful
#' when you have low sample sizes for a cohort of similar species.
#' \item `pool`: A character string, providing a title for this species pool.
#' If not specified, the species codes used will be concatenated to produce a title automatically.
#' \item `cohort`: The cohort containing these species, provided as a number
#' indicating which slot in `cruz$cohorts` should be referenced.
#' \item `truncation_distance`: The truncation distance to apply during model fitting.
#' \item `other_species`: A character vector with four recognized values:
#' \itemize{
#' \item If `"apply"` (the default if not specified),
#' the species code will be changed to `"Other"`
#' for sightings in which the species was in a mixed-species school
#' but was not the species with the largest percentage of the total school size.
#' In those cases, the species was not as relevant to the detection
#' of the school as the other species were, which may bias the detection function.
#' This creates a factor level for the detection function to use (when `"species"` is a covariate)
#' to distinguish between cue-relevant species that are within the specified pool
#' and those that are not.
#' \item The second option for `other_species` is `"ignore"`, which
#' does **not** reassign species codes to `"Other"`, and ignores whether the species of
#' interest held the plurality for a mixed species detection.
#' \item The third option is `"remove"`: any species re-assigned to `"Other"` will be removed before the detection
#' function is fit; this can be useful if only a small number of species are re-assigned
#' to `"Other"`, which would then obviate `species` as a viable covariate (since the sample
#' size of all `species` levels would be unlikely to exceed
#' `df_settings$covariates_n_per_level` -- see below).
#' \item The fourth and final option is `coerce`, which forces *all* species codes to `"Other"`
#' for the purposes of detection function fitting and abundance estimation. This can be
#' useful if you want to toggle the use of `species` as a covariate for a specific species pool,
#' and/or produce abundance estimates for unidentified taxa (e.g., an 'Unidentified dolphins' species pool that includes multiple species codes).
#' }
#' }
#'
#' @param df_settings A named list, with parameters for fitting the detection function.
#' The following slots are recognized, but none is required (i.e., all have defaults):
#' \itemize{
#' \item `covariates`: Covariates you wish to include as candidates in detection function models,
#' provided as a character vector. The covariates must match columns existing
#' within `cruz$cohorts$<cohort_name>$sightings`.
#' Common covariates that you will find within `sightings`
#' include `c('Bft','LnSsTot','Cruise','Year','Ship','species')`.
#' Note that the function will ignore case, coercing all covariates to lowercase.
#' If `LnSsTot` is included as a covariate, the function will (1) check to see if
#' the `sightings` dataframe has a column named `ss_valid` (all `cruz` objects do),
#' then, if so, (2) filter `sightings` only to rows where `ss_valid` is `TRUE`, meaning
#' the school size estimate for that sighting is a valid estimate.
#' \item `covariates_factor`: A Boolean vector, which must be the same length as `covariates`,
#' indicating whether each covariate should be treated as a factor instead of a numeric.
#' \item `covariates_levels`: The minimum number of levels a factor covariate must have
#' in order to be included as an eligible covariate.
#' \item `covariates_n_per_level`: The minimum number of observations within each level of a factor covariate.
#' If this condition is not met, the covariate is excluded from the candidates.
#' \item `simplify_cue` A Boolean, with default `TRUE`, indicating whether or not
#' `cue` codes should be simplified before being included as a covariate in detection function fitting.
#' This can help to overcome the factor sample size limitations that may prevent
#' inclusion as a covariate.
#' If `TRUE`, cues 0, 1, 2, 4, and 7 are coerced to 5, representing 'other' cues.
#' \item `simplify_bino` A Boolean, with default `TRUE`, indicating whether or not
#' sighting `method` codes should be simplified before being included as a covariate in detection function fitting.
#' This can help to overcome the factor sample size limitations that may prevent
#' inclusion as a covariate.
#' If `TRUE`, methods other than 4 are all coerced to 5, representing 'other' methods.
#' \item `detection_function_base`: The base key for the detection function, provided as a character vector.
#' Accepted values are `"hn"` (half-normal key, the default, which exhibit greater stability
#' when fitting to cetacean survey data; Gerrogette and Forcada 2005), `"hr"` (hazard-rate),
#' or `c("hn", "hr)`, which will loop through both keys and attempt model fitting.
#' \item `base_model`: The initial model formula, upon which to build using candidate covariates.
#' if not provided by the user, the default is `"~ 1"`.
#' \item `delta_aic`: The AICc difference between the model yielding the lowest AICc and
#' other candidate models, used to define the best-fitting models. Typically,
#' AICc differences of less than 2 (the default) indicate effectively equal model performance.
#' If this value is not zero, then model averaging will be done: if multiple models
#' are within `delta_aic` of the model with the lowest AICc, all "best" models will be used
#' in subsequent steps and their results will be averaged. See `Details` below.
#' }
#'
#' @param estimates A nested list (i.e., a list of sublists), in which each constituent sublist
#' contains the settings for a single density/abundance estimate.
#' Check out the `LTabundR` function, `lta_estimates()`, which facilitates building this `estimates` argument.
#' The slots below are recognized in each constituent sublist,
#' but not all are required (i.e., some have defaults).
#' \itemize{
#' \item `spp`: (Required) A character vector of species codes.
#' If multiple codes are provided, a single density/abundance estimate will be provided
#' for this pooled group of species.
#' If  `NULL`, the codes in `fit_filters$spp` will be used.
#'
#' \item `title`: (Required) A title for this abundance estimate, given as a character vector,
#' e.g., `"Striped dolphin - pelagic"`.
#' If left blank, the species code(s) will be concatenated to use as a title.
#' Note that, if `spp_method` is `'each'`, then `title` must be the same length as `spp`.
#'
#' \item `years`: (Required) A required numeric vector of years, used to filter data
#' to include only effort/sightings from these years.
#'
#' \item `regions`: (Required) A character vector of geostratum names, used to filter the data
#' to a study area in which to estimate density/abundance.
#' Any segment or sighting occurring within *any* of the provided `regions` will be returned.
#' This holds true for nested regions: for example, in analyses from the
#' Central North Pacific, in which the Hawaii EEZ geostratum (`"HI_EEZ"`)
#' is nested within the larger geostratum representing the entire CNP study area (`"OtherCNP"`),
#' an input of `regions = "OtherCNP"` will return segments/sightings
#' *both* inside the Hawaii EEZ *and* outside of it.
#' These geostratum names must have been used to process this `cruz` object cohort during `LTabundR::process_surveys()`.
#
#' \item `cruises`: (An optional numeric vector of cruise numbers, used to filter data to include
#' effort/sighting from only certain cruises. Ignored if `NULL`.
#'
#' \item `regions_remove`: An optional character vector of geostratum names, similar to above.
#' These regions will be subtracted from `regions` to determine the final study area
#' in which density/abundance will be estimated.
#' Any segment or sighting occurring within any of these `regions_remove` will **not** be
#' used in density/abundance estimation.
#' Using the example above, if `regions = "OtherCNP"` and `regions_remove = "HI_EEZ"`,
#' only segments occuring within `OtherCNP` *and* outside of `HI-EEZ` will be used.
#' This can be particularly useful for abundance estimates for pelagic stock that exclude nested insular stocks.
#' These geostratum names must have been used to process this `cruz` object cohort during `LTabundR::process_surveys()`.
#'
#' \item `region_title`: An optional character vector indicating the title you would like to
#' give to the region pertaining to this estimate. This can be useful if you have a
#' complicated assemblage of regions you are combining and/or removing.  If not supplied,
#' the function will automatically generate a `region_title` based on `regions` and `regions_remove`.
#'
#' \item `g0`: (Optional) If left as the default `NULL`, this function will automatically
#' estimate the weighted trackline detection probability (`g0`) according to the
#' distribution of Beaufort sea states contained within the survey years/regions for which
#' density/abundance is being estimated (this is done using the `LTabundR` function `g0_weighted()`;
#' see its documentation for details). This will only be done if the `Rg0` input above is not `NULL`;
#' if it is and you do not provide `g(0)` values here, `g0` will be coerced to equal 1.
#' To coerce `g(0)` to a certain value of your own choosing,
#' you can provide a numeric vector of length 1 or 2.
#' If length 1, this value represents `g(0)` for all schools regardless of size.
#' If length 2, these values represent `g(0)` for small and large school sizes, as defined by
#' `g0_threshold` below.
#'
#' \item `g0_cv`: (Optional) Similar to `g0` above: if left `NULL`, the CV of the `g(0)` estimate
#' will be automatically estimated based on weighted survey conditions.
#' Alternatively, you can manually specify a CV here, using a numeric vector of length 1 or 2.
#' If you do not specify a value and `Rg0` input is `NULL`, `g0_cv` will be coerced to equal 0.
#'
#' \item `g0_threshold`: (Optional) The school size threshold between small and large groups.
#'
#' \item `alt_g0_spp`: (Optional) An alternate species code to use to draw Relative `g(0)` values from
#' the `Rg0` input. This is useful in the event that `Rg(0)` was not estimated for the
#' species whose density/abundance you are estimating, but there *is* a similarly detectable
#' species whose `Rg(0)` parameters have been estimated.
#'
#' \item `combine_g0`: (Optional) A Boolean, with default `FALSE`. If `TRUE`, weighted g0 estimates will be
#' produced *separately* for each species code provided (specifically, for each unique row in the
#' `Rg0` table that is found after filtering by the species codes you provide in
#' this estimate), *THEN* average those estimates together. This can be useful
#' when you do not have a `Rg(0)` estimates for a certain species, but you can approximate
#' Rg0 by averaging together estimtes from multiple species (e.g., averaging together
#' weighted g(0) from across rorqual species in order to get a weighted g(0) estimate
#' for 'Unidentified rorquals').
#'
#' \item `forced_effort` (Optional) If this is a single numeric value instead of `NULL` (`NULL` is the default),
#' this value will be used as the survey effort, in km, in a brute-force method.
#' If left `NULL`, the function will calculate survey effort itself.
#' This is only helpful if you are looking for a relatively easy way to compare results
#' from your own analysis to another
#' (e.g., comparing `LTabundR` results to reports from NOAA reports prior to 2021,
#' in which effort was calculated slightly differently).
#'
#' \item `area`: (Optional) If this is a single numeric value instead of `NULL` (`NULL` is the default),
#' this value will be used as the area of the region in which abundance is being estimated,
#' in square km, in a brute-force approach. If left `NULL`, the function will calculate
#' the final area of the survey area resulting from the `regions` and `regions_remove` filters above.
#'
#' \item `remove_land`: (Optional) A Boolean, with default `TRUE`, indicating whether or not land area should
#' be removed from the survey area before calculating its area for abundance estimation.
#' This term is only referenced if `area` is not specified manually.
#' }
#'
#' @param use_g0 A Boolean, with default `TRUE`, indicating whether or not to use custom `g(0)` value(s).
#' If `FALSE`, the assumed `g(0)` value will be 1. This can be a handy way of toggling weighted `g(0)`
#' estimation on and off across all sublists within `estimates`.
#'
#' @param ss_correction Should a correction be applied to school sizes?
#' School sizes will be scaled by this number. The default, `1`, means no changes will occur.
#' This is a vestige of pre-2021 analysis workflows, and typically will not be invoked.
#'
#' @param abund_eff_types A character vector of `EffType` accepted as systematic effort
#' (for density / abundance estimation). The default is just `"S"` (systematic effort),
#' but in some surveys/cases you may wish to use fine-scale effort (`"F"`) too.
#'
#' @param abund_bft_range A numeric vector of Beaufort Sea Sates accepted as systematic effort
#' (for density / abundance estimation). The default is `0:6`.

#' @param bootstraps The number of bootstrap iterations. If 0 or 1, no bootstrapping will be carried out.
#'
#' @param toplot Boolean, with default `TRUE`, indicating whether detection function plots (`Distance::plot.ds()`)
#' should be displayed as the candidate models are tested.
#'
#' @param results_file If not `NULL`, this input will be taken as the name of the file
#' in which to save the results as this function works. This can be a handy way of saving
#' results as you go, in the event of a major error or system crash.
#'
#' @param verbose Boolean, with default `TRUE`, indicating whether or not updates should be printed to the Console.
#'
#' @details
#' See the [vignette online](https://emk-noaa.github.io/LTAvignette/lta.html) for detailed examples & case studies.
#'
#' **Survey area calculations:**
#' The area for which abundance is to be estimated is calculated separately for each sublist within `estimates`
#' according to the inpunts `regions` and `regions_remove`. This calculation is performed by a call to the
#' `LTabundR` function `strata_area()`, which handles complex combinations and subtractions of geostrata,
#' accounting for overlapping strata and the (optional) removal of any land area
#' (see its documentation for details).
#' The polygon for each resulting study area is added to the respective `estimates` sublist.
#' Those polygons can be retrieved from the output's `$inputs` slot.
#'
#' **Weighted g(0) estimates:**
#' If `g(0)` values are not supplied manually for an `estimates` sublist, a weighted `g(0)`
#' will be estimated as part of this function's operations through a call to the
#' `LTabundR` function `g0_weighted()`, which automatically optimizes a model that
#' estimates the `g(0)` and its CV based on the distribution of effort in different
#' Beaufort sea states within the specific year, region, and cruise in question.
#' This is only done if the input `Rg0` is supplied.
#'
#' **Covariates in detection function estimation:** Before detection functions are modelled,
#' any covariates supplied by the user and specified as a factor are first
#' tested for eligibility. Only factors with
#' at least two levels (or whatever you specified with `df_settings$covariates_levels`)
#' and 10 observations in each level (or whatever you specified with `df_settings$covariates_n_per_level`)
#' are eligible for inclusion.
#'
#' **Fitting a detection function:** The detection function is estimated using functions
#' in the package `mrds`, primarily the main function `mrds::ddf()`, which uses a
#' Horvitz-Thompson-like estimator to predict the probability of detection for each sighting.
#' If multiple base key functions (e.g., half-normal or hazard-rate) are provided, and/or if
#' covariates are specified, model fitting is done in a forward stepwise procedure:
#' In the first round, the base model (no covariates, i.e., `"~1"`) is fit first.
#' In the second round, each covariate is added one at a time;
#' at the end of the round, the covariate, if any, that produces the lowest AICc
#' below the AICc from the previous round is added to the formula.
#' This process is repeated in subsequent rounds, adding a new covariate term in each round,
#' until the AICc no longer improves.
#' If a second base key is provided, the process is repeated for that second key.
#' All models within `delta_aic` of the model with the lowest AICc qualify as best-fitting models.
#'
#' The best-fitting model(s) is(are) then used to estimate the Effective Strip half-Width (ESW)
#' based on the covariates associated with each sighting.
#' If multiple best-fitting models occur, we will find the average ESW for each
#' sighting across all models, using a weighted mean approach in which we weight according to model AICc.
#' To turn off this model averaging step, set `delta_aic` to `0` to avoid passing
#' multiple models to the abundance estimation stage.
#'
#' This stage of the `lta()` command is executed within a backend function, `LTabundR::df_fit()`,
#' which has its own documentation for your reference.
#'
#' Note that if `LnSsTot` and/or `Bft` (or any other numeric covariate) are included as
#' candidate covariates, missing data (in the case of `LnSsTot`, rows where `ss_valid == FALSE`),
#' will be removed before detection function fitting, but those sightings will not be removed
#' from the sightings data used for abundance estimation (see below). (Note that the code to
#' handle these exceptions are contained within `lta()`, not `df_fit()`).
#'
#' **Estimating density & abundance:** Estimates are produced for various combinations
#' of species, regions, and years, according to the arguments specified in your `estimates` list(s).
#' Before these estimates are produced, we filter the data used to fit the detection function
#' to strictly systematic (design-based) effort
#' (as specified in the `abund_eff_types` and `abund_bft_range` inputs).
#' Note that if `NA`'s occur in the `esw` column
#' (due, for instance, to a covariate with missing data for a sighting),
#' they will be replaced with the mean `esw` value
#' for the remainder of the dataset in that region-year.
#' Similarly, if `sightings` has a column named `ss_valid` (all standard `cruz` objects do)
#' and any of the rows in that column are `FALSE`, those rows will have their `best`
#' school size estimate (which will be `NA` or `1`, since they are invalid) replaced
#' by the mean best estimate for their respective species.
#'
#' This stage of the `lta()` command is executed within a back-end function, `LTabundR::abundance()`,
#' which has its own documentation for your reference.
#'
#' **Bootstrap variance estimation:** If the `bootstraps` input value is greater than 1, bootstrap variance estimation will be attempted.
#' In each bootstrap iteration, survey segments are re-sampled with replacement
#' before fitting the detection function and estimating density/abundance.
#'
#' Note that the entire process is repeated in each bootstrap: step-wise fitting
#' of the detection function, averaging of the best-fitting models, and density/abundance
#' estimation for all species/region/year combinations specified in your `estimates` input.
#' At the end of the bootstrap process, results are summarized for each species/region/year combination.
#' 95% confidence intervals are calculated using the BCA method (package `coxed`, function `bca()`).
#'
#' **`g(0)` values during bootstrapping:** When conducting the non-parametric bootstrap routine
#' to estimate the CV of density and abundance, uncertainty is incorporated into the g(0) value
#' in each iteration using a parametric bootstrapping subroutine:
#' First, a logit-transformed distribution is modeled
#' based upon the mean and CV of g(0) provided by the user in the `estimates` input
#' (see documentation for `LTabundR::g0_optimize()` for details on this step).
#' This modeled distribution is used to randomly draw a g(0) value for each iteration
#' of the density/abundance bootstrap routine. In this way,
#' the uncertainty in g(0) is propagated into uncertainty in density/abundance.
#'
#' **Workflow recommendations:** This function was designed to optimize workflow where possible.
#' Some considerations:
#' \itemize{
#' \item Expect a single `lta()` call for each species pool. You can use a single call
#' to estimate the detection function once, then predict density/abundance separately for each species within the pool.

#' \item Some inputs will be common across all species pools (e.g., `df_settings` and `bootstraps`).
#' It may be most efficient -- and easiest to keep consistent and to update --
#' if you define these common inputs at the top of your script,
#' then call them with a simple variable in each of your `lta()` calls.

#' \item We recommend setting up each `lta()` call starting with `bootstraps = 0`,
#' so that you can first test that the simple estimation step is successful for all species pools.
#' Then test the bootstrapping functionality by setting `bootstraps` to `10` in each `lta()` call.
#' Track how long it takes for your code to run, which you can use to predict
#' processing time for a larger number of iterations, e.g., `bootstraps = 1000`.

#' \item The most complicated argument to prepare is `estimates`. To help with this,
#' `LTabundR` includes a function, `lta_estimates()`. See its documentation for details. }
#'
#' @return A named list:
#' \enumerate{
#' \item `pool`: The species pool this estimate pertains to.
#' \item `inputs:` A record of the inputs you provided, stored as a list.
#' \item `estimate`: A table of density/abundance estimates for each species/region/year
#' combination specified in the `estimates` input.
#' This `data.frame` contains the following fields:
#' \enumerate{
#' \item `Region`: Name(s) of geostrata represented in this estimate.
#' \item `Area`: Area of geostratum / region, in square km.
#' \item `year`: Years represented in this estimate.
#' \item `segments`: The number of effort segments used to estimate density/abundance.
#' \item `km`: The km of trackline effort contained in these segments.
#' \item `Area_covered`: The Area surveyed, according to `km` and `ESW_mean` (see next column).
#' \item `ESW_mean`: Mean effective strip width, in kw, calculated as the mean probability of detection for all detections.
#' \item `n`: The number of detections in the data.
#' \item `g0_est`: The mean `g(0)` estimate.
#' \item `ER_clusters`: The encounter rate for detections (schools) (`n / km`)
#' \item `D_clusters`: The density of detections (schools).
#' \item `N_clusters`: The abundance of schools.
#' \item `size_mean`: Average school size.
#' \item `size_sd`: Standard deviation of school size.
#' \item `ER`: Animal encounter rate.
#' \item `D`: Animal density.
#' \item `N`: Animal abundance.
#' }
#'
#' \item `df`: A named list with details for the detection function.
#' \enumerate{
#' \item `best_models`: A `data.frame` summary of the best-fitting models,
#' based upon the table produced by `Distance::summarize_ds_models()`.
#' See that function's documentation for details.
#' \item `all_models`:  Similar to the preceding slot, a tabular summary of *all* models tested.
#' \item `best_objects`:  A list containing the `ds` objects (produced by package `Distance`)
#' for each of the best-fitting models.
#' \item `sample_size`: A `data.frame` with the detections for each species within the species pool
#' used to fit the detection function (as well as `Other` species; see the `other_species` input).
#' `Ntot` is total detections for each species; `Ndet` is total detections within the truncation distance
#' and therefore used in the detection function fitting routine; `TD` is the truncation distance.
#' \item `curve`: A `data.frame` of the best-fitting detection function curve
#' (best-fitting models averaged together, weighted by AICc), for 100 distances between 0
#' and the `truncation_distance` (two columns: `km` and `p`, the probability of detection
#' at that distance).
#' }
#'
#' \item `bootstrap`: A named list with results from the bootstrap process, only
#' returned if the `bootstraps` input is greater than `1`.
#' \enumerate{
#' \item `summary`: a `data.frame` with a row for each species/region/year combination for which density/abundance was estimated.
#' Notable columns include `g0_mean` and `g0_cv` (the mean and CV of g(0) values across parametric bootstrap iterations);
#' `Nmean` (the mean abundance, based on bootstrap re-sampling);
#' `Nmedian` (median abundance); `Nsd` (standard-deviation of abundance);
#' `CV` (coefficient of variation, which applies to both density and abundance);
#' `L95` (the lower BCA 95% confidence interval), and `U95` (the upper BCA 95% confidence interval).
#' \item `details`: a `data.frame` with details for every iteration of the bootstrap routine.
#' \item `df`: a `data.frame` with the detection function curve for each bootstrap iteration.
#' }
#' }
#' @export
#'
#'
lta <- function(cruz,
                Rg0,
                fit_filters,
                df_settings,
                estimates,
                use_g0 = TRUE,
                ss_correction = 1,
                abund_eff_types = c('S'),
                abund_bft_range = 0:6,
                bootstraps = 0,
                results_file = NULL,
                toplot=TRUE,
                verbose=TRUE){

  ##############################################################################
  ##############################################################################
  if(FALSE){ # for debugging

    data('cnp_150km_1986_2020')
    cruz <- cnp_150km_1986_2020
    data(g0_results)
    g0_results %>% head
    Rg0 <- g0_results

    # Detection function specifications ============================================

    fit_filters <-
      list(spp = c('013', '026', '031'),
           pool = 'Multi-species pool 1',
           cohort = 'all',
           truncation_distance = 5,
           other_species = 'remove')

    df_settings <-
      list(covariates = c('bft','lnsstot','cruise','year','ship','species'),
           covariates_factor = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
           covariates_levels = 2,
           covariates_n_per_level = 10,
           simplify_cue = TRUE,
           simplify_bino = TRUE,
           detection_function_base = 'hn',
           base_model = '~1',
           delta_aic = 2)


    # Density / abundance estimation plan ==========================================

    scenarios <- list(list(years = 2017,
                           regions = 'MHI'),
                      list(years = 2020,
                           regions = 'MHI'))

    estimator <- lta_estimates(scenarios)
    estimates <-
      c(estimator(spp = '013', title = "Striped dolphin"),
        estimator(spp = '026', title = "Fraser's dolphin", alt_g0_spp = '013'),
        estimator(spp = '031', title = "Melon-headed whale", alt_g0_spp = '013'))

    use_g0 = TRUE
    ss_correction = 1
    bootstraps = 10000
    toplot=TRUE
    verbose=TRUE
    abund_eff_types = c('S')
    abund_bft_range = 0:6

    # Try it
    lta(cruz, Rg0, fit_filters, df_settings, estimates)

    # debugging
    ltas <- lta_enlist("/Users/ekezell/Desktop/projects/noaa ltabundr/marianas/lta_barlow/")
    ltas[[1]]$estimate
    ltas[[1]]$bootstrap$summary %>% as.data.frame
    hist(ltas[[2]]$bootstrap$details$g0_est, breaks=20, main='g(0) values in bootstraps')

    #n <-
    df <- ltas[[1]]$bootstrap$details %>% filter(title == 'Bottlenose dolphin')
    df %>% head
    length(which(df$n == 0))
    length(which(df$n > 1))
    plot(df$N ~ df$g0_est)
    hist(df$n)
    ggplot(df, aes(x=g0_est, y=N, color=factor(n))) + geom_point()
    n <- df %>% filter(n < 10) %>% pull(N)
    n <- n[n > 0]
    sd(n) / mean(n)
    hist(n)

    # Produce diagnostic plot
    results %>% head
    ggplot(results,
           aes(x=n)) +
      geom_histogram(alpha=.5, col='white', fill='darkblue') +
      facet_wrap(~title + Region + year)


    # To try function, use TLabundR-dev/test_code/CNP/lta_tests.R
  }

  ##############################################################################
  ##############################################################################
  # Harvest inputs & handle input errors & NULL inputs

  # Initial checks
  if(verbose){message('\n')}
  if(!is.list(cruz)){stop('Wait!!! `cruz` must be a list.')}
  if(!is.null(Rg0)){ if(!is.data.frame(Rg0)){stop('Wait!!! `Rg0` must be a data.frame.')}}

  # Fit filters
  if(!is.list(fit_filters)){stop('Wait!!! `fit_filters` is not provided as a list.')}
  if(is.null(fit_filters$spp)){stop('Wait!!! `fit_filters$spp` may not be NULL. Provide at least one species code.')}
  if(is.null(fit_filters$truncation_distance)){stop('Wait!!! `fit_filters$truncation_distance` may not be NULL. Provide a numeric truncation distance.')}
  cohort <- fit_filters$cohort
  if(!is.null(cohort) & ! cohort %in% names(cruz$cohorts)){
    stop('Wait!!! The cohort you specified does not exist. Enter an exsiting name for a cohort, or a number between 1 and length(cruz$cohorts).')
  }
  if(is.null(cohort)){cohort <- 1}
  (truncation_distance <- min(10, fit_filters$truncation_distance))
  pool <- fit_filters$pool ; if(is.null(pool)){pool <- paste(fit_filters$spp, collapse=' | ')}
  other_species <- fit_filters$other_species ; if(is.null(other_species)){other_species <- 'apply'}

  # Detection function defaults
  if(!is.list(df_settings)){stop('Wait!!! `df_settings` is not provided as a list.')}
  (covariates <- df_settings$covariates)
  if(!is.null(covariates)){covariates <- tolower(covariates)}
  (covariates_factor <- df_settings$covariates_factor)
  if(length(covariates) != length(covariates_factor)){stop('Wait!!! `covariates` is not the same length as `covariates_factor.')}
  covariates_levels <- df_settings$covariates_levels ; if(is.null(covariates_levels)){covariates_levels <- 2}
  covariates_n_per_level <- df_settings$covariates_n_per_level ; if(is.null(covariates_n_per_level)){covariates_n_per_level <- 10}
  detection_function_base <- df_settings$detection_function_base ; if(is.null(detection_function_base)){detection_function_base <- 'hn'}
  base_model <- df_settings$base_model ; if(is.null(base_model)){base_model <- '~1'}
  delta_aic <- df_settings$delta_aic ; if(is.null(delta_aic)){delta_aic <- 2}
  simplify_cue <- df_settings$simplify_cue ; if(is.null(simplify_cue)){simplify_cue <- FALSE}
  simplify_bino <- df_settings$simplify_bino ; if(is.null(simplify_bino)){simplify_bino <- FALSE}

  # g0 setting
  if(is.null(use_g0)){use_g0 <- TRUE}

  ##############################################################################
  ##############################################################################
  # Prep estimates

  if(verbose){message('Reviewing `estimates` sublists and filling in any unspecified parameters ...\n')}

  # Initial checks
  if(is.null(estimates)){stop('Wait!!! `estimates` input cannot be NULL.')}
  if(!is.list(estimates)){stop('Wait!!! `estimates` input is not provided as a list.')}
  if(!is.list(estimates[[1]])){stop('Wait!!! `estimates` input has not been provided as a list of lists.')}

  # Loop through each `estimates` sublist
  i=1
  for(i in 1:length(estimates)){
    (estimati <- estimates[[i]])

    # Handle essential inputs that are missing or mis-formatted
    if(is.null(estimati$spp)){stop('Wait!!! One of your `estimates` sub-lists (sub-list ',i,') is missing a `spp` slot.')}
    for(j in 1:length(estimati$spp)){
      if(any(! estimati$spp[j] %in% fit_filters$spp)){stop('Wait!!! One of your `estimates` spp  (in sub-list ',i,') does not belong to the spp you used to buid your detection function.')}
    }
    if(is.null(estimati$years)){stop('Wait!!! One of your `estimates` years inputs in (sub-list ',i,') is NULL.')}
    if(is.null(estimati$regions)){stop('Wait!!! One of your `estimates` regions inputs in (sub-list ',i,') is NULL.')}

    # Handle non-essential NULL objects
    if(is.null(estimati$title)){ estimati$title <- paste(estimati$spp, collapse=' | ') }
    if(is.null(estimati$regions_remove)){estimati$regions_remove <- NULL}
    if(is.null(estimati$cruises)){estimati$cruises <- NULL}
    if(is.null(estimati$forced_effort)){estimati$forced_effort <- NULL}
    if(is.null(estimati$remove_land)){estimati$remove_land <- TRUE}
    if(is.null(estimati$combine_g0)){estimati$combine_g0 <- FALSE}

    if(verbose){message('--- estimate sub-list ',i,' :: ', estimati$title)}

    # Handle survey area
    study_area <- NULL
    if(verbose){message('--- --- building survey area polygon given `regions` and `regions_remove` specs ...')}
    #strata_all = cruz$settings$strata
    #strata_keep = estimati$regions
    #strata_remove = estimati$regions_remove
    #remove_land = estimati$remove_land
    (this_area <- strata_area(strata_all = cruz$settings$strata,
                              strata_keep = estimati$regions,
                              strata_remove = estimati$regions_remove,
                              remove_land = estimati$remove_land,
                              toplot = toplot,
                              verbose = FALSE))
    estimati$study_area <- this_area$sf
    if(is.null(estimati$area)){
      if(verbose){message('--- --- --- getting area of this polygon to use as abundance estimation area ...')}
      estimati$area <- this_area$km2
    }

    # Handle g0
    if(verbose){message('--- --- handling g(0) parameters ....')}
    g0i <- 1 # debugging values
    g0cvi <- 0
    bft <- NULL
    if(use_g0){
      if(is.null(estimati$g0)){
        if(!is.null(Rg0)){
          # If Rg0 results *are* available, try to calculate weighted g0
          if(verbose){message('--- --- --- attempting to estimate weighted g(0) ....')}

          # First look for this species in the Rg0 table
          if(is.null(estimati$alt_g0_spp)){
            g0_spp <- estimati$spp
          }else{
            g0_spp <- estimati$alt_g0_spp
          }
          g0_spp
          if(estimati$combine_g0){
            # keep all Rg0 rows
            (titles <- unique(Rg0$title[sapply(g0_spp, function(x){grep(x, Rg0$spp)})]))
            (rg0i <- Rg0 %>% dplyr::filter(title %in% titles))
          }else{
            # only check for first species in the character vector
            (rg0i <- Rg0[grepl(g0_spp[1], Rg0$spp),])
          }

          # can only proceed if Rg0 results were found for this species of interest
          if(nrow(rg0i)>0){
            # Filter cruz object to estimate year-region-efftype-region
            if(verbose){message('--- --- --- filtering cruz data to this estimate sub-list year and region ...')}
            print(estimati)
            suppressMessages({
              cruzg0 <- filter_cruz(cruz,
                                    years = estimati$years,
                                    regions = estimati$regions,
                                    not_regions = estimati$regions_remove,
                                    cruises = estimati$cruises,
                                    eff_types = abund_eff_types,
                                    bft_range = abund_bft_range)
            })

            if(verbose){message('--- --- --- modeling weighted g(0) and weighted CV (this may take several minutes) ...')}
            cruzg0
            g0s <- g0cvs <- c() # stage results vectors
            (g0titles <- unique(rg0i$title))
            # Loop through each species title
            ri=1 # debugging
            for(ri in 1:length(g0titles)){
              (rg0ii <- rg0i %>% dplyr::filter(title == g0titles[ri]))
              g0w <- g0_weighted(Rg0 = rg0ii$Rg0,
                                 Rg0_cv = rg0ii$Rg0_CV,
                                 cruz = cruzg0,
                                 cohort = cohort,
                                 toplot = toplot,
                                 verbose = FALSE)
              bft <- g0w$bft # save the bft proportions
              g0i <- g0w$g0$weighted_g0
              #g0i <- g0w$g0$wt.mean # the old method (until 11-19-23) used the mean of the modeled weighted g0s
              g0cvi <- g0w$g0$wt.cv
              g0s <- c(g0s, g0i)
              g0cvs <- c(g0cvs, g0cvi)
              if(verbose){message('--- --- --- species = ', paste(unique(rg0ii$spp), collapse='-'),' :: weighted g0 = ', g0i,' (CV = ',g0cvi,')')}
            }
            g0s
            g0cvs

            # Combine these into a single estimate of g0 and g0cv (using an LTabundR function)
            g0i <- g0_combine(g0s, g0cvs)$g0
            g0cvi <- g0_combine(g0s, g0cvs)$CV

            # For debugging ================================
            #         #minke # s/b  # fw  # bw
            #g0s <- c(0.11,   .41,   .33,  .55)
            #g0cvs <- c(0.98, .20,   .27,  .35)
            #g0s <- g0s[1] # works for single-species and combine_g0 applications
            #g0cvs <- g0cvs[1]
            #(g0i <- mean(g0s)) # combined g0
            #(g0int <- g0s^2 * g0cvs^2) # intermediate step
            #(g0int2 <- sum(g0int / (length(g0s)^2))) # combined variance
            #(g0cvi <- round((sqrt(g0int2) / g0i), 3)) # combined CV
            if(verbose){message('--- --- --- final weighted g0 = ', g0i,' (CV = ',g0cvi,')')}

          }else{ # end if nrow(rgoi) > 0
            if(verbose){message('--- --- --- species could not be found in Rg0 table! Using default g0/cv values...\n')}
          }
        }else{ # end if !is.null(Rg0)
          if(verbose){message('--- --- --- Rg0 table not provided. Using default g0/cv values...\n')}
        }
      } # end if is.null(estimati$g0)
    } # end if use_g0

    # check out results of weighted g0 estimation
    g0i
    g0cvi
    bft
    if(is.null(estimati$g0)){ estimati$g0 <- g0i }
    if(is.null(estimati$g0_cv)){ estimati$g0_cv <- g0cvi }
    estimati$bft <- bft

    # Deal with small v large g0 formatting
    if(length(estimati$g0)==1){estimati$g0 <- c(estimati$g0, estimati$g0)}
    if(length(estimati$g0_cv)==1){estimati$g0_cv <- c(estimati$g0_cv, estimati$g0_cv)}
    if(is.null(estimati$g0_threshold)){estimati$g0_threshold <- Inf}

    # Update this sublist within `estimates` list
    estimates[[i]] <- estimati
    if(verbose){message('\n')}
  }

  # Review result (debugging)
  estimates

  ##############################################################################
  ##############################################################################
  # Make g0 table
  # For each estimate, find the parameters for a logit-normal distribution of g0 values
  # Store as a list for easy reference later

  if(use_g0){
    if(verbose){message('Preparing a table of g(0) parameters for this set of estimates ...')}

    # Stage results
    g0_tables <- list()

    # Loop through each estimate
    g0_loops <- length(estimates)
    g0_loops
    gi=1 # debugging
    for(gi in 1:g0_loops){
      (est_filters <- estimates[[gi]])
      (g0 <- est_filters$g0)
      (g0_cv <- est_filters$g0_cv)
      g0_threshold <- est_filters$g0_threshold
      # Use the LTabundR function g0_optimize
      g0_small <- g0_optimize(g0[1], g0_cv[1], try_count = 40, verbose = verbose) ; g0_small # Estimate g0[1] (small schools)
      g0_large <- if(g0[1] == g0[2]){g0_small}else{g0_optimize(g0[2], g0_cv[2], try_count = 40, verbose = verbose)} ; g0_large
      (g0_i <- c(g0_small$g0_mean, g0_large$g0_mean))
      (g0_cvi <- c(g0_small$g0_cv, g0_large$g0_cv))
      g0_param <- matrix(data=NA, nrow=2, ncol=2)
      g0_param[1,] <- g0_small$bestFit
      g0_param[2,] <- g0_small$bestFit
      g0i <- list(g0 = g0,
                  g0_cv = g0_cv,
                  g0_threshold = g0_threshold,
                  g0_param = g0_param)
      g0i
      g0_tables[[length(g0_tables)+1]] <- g0i
    }
  }

  ##############################################################################
  ##############################################################################
  # Setup datasets

  # Filter to correct cohort
  cohorti <- cruz$cohorts[[cohort]] ; names(cohorti)
  ani <- cohorti # rename for convenience
  strata <- cruz$strata
  dist_segments <- ani$segments
  dist_sightings <- ani$sightings
  dist_das <- ani$das

  # Filter only to data to be used in detection function fitting
  dist_segments <- dist_segments %>% dplyr::filter(use==TRUE)
  dist_sightings <- dist_sightings %>% dplyr::filter(use==TRUE, included == TRUE)
  dist_das <- dist_das %>% dplyr::filter(use==TRUE)

  # Double check
  dist_segments$use %>% table
  dist_sightings$use %>% table
  dist_das$use %>% table
  dist_sightings$included %>% table

  ##############################################################################
  ##############################################################################
  # Filter  datasets according to filters
  # use the LTabundR function `filter_cohort()`

  if(verbose){message('\nFiltering data according to `fit_filter` ...')}
  # distance - abundance data
  filtered_data <- filter_cohort(segments = dist_segments,
                                 sightings = dist_sightings,
                                 das = dist_das,
                                 spp = fit_filters$spp,
                                 verbose = verbose)
  dist_segments <- filtered_data$segments
  dist_sightings <- filtered_data$sightings

  ##############################################################################
  ##############################################################################
  # Handle 'Other' species designations for mixed-species school
  # (species that are not the plurality in a mixed sighting become Other)

  if(verbose){message('\nHandling "Other" species designations ...')}

  if(other_species == 'coerce'){
    # Coerce all species codes to Other
    dist_sightings$species <- 'Other'
  }

  # Save original species names and detection distances
  dist_sightings$species_og <- dist_sightings$species
  dist_sightings$distance_og <- dist_sightings$PerpDistKm

  # Prepare version of sightings data for detection-function fitting
  fit_sightings <- dist_sightings
  if(other_species %in% c('apply', 'remove')){
    # With 'Other' species, there is a single row for every sighting,
    # and in multi-spp sightings, the spp with the greatest abundance is used as the main species,
    # and any sighting in which spp_max is not within fit_filters$spp gets the 'Other' spp designation
    fit_sightings <-
      dist_sightings %>%
      dplyr::group_by(SightNoDaily) %>%
      dplyr::summarize_all(~head(.x,1)) %>%
      dplyr::mutate(species = spp_max) %>%
      dplyr::mutate(species = ifelse(species %in% fit_filters$spp, species, 'Other')) %>%
      as.data.frame
  }

  fit_sightings$species %>% table
  nrow(dist_sightings)
  nrow(fit_sightings)

  ##############################################################################
  ##############################################################################
  # Handle NAs in the numeric covariates & invalid school sizes

  if(!is.null(covariates)){
    if(length(covariates)>0){
      (numeric_covars <- covariates[!covariates_factor])
      # Get indices for which one of the numeric covariates is NA
      if(length(numeric_covars)>0){
        bads <-
          sapply(numeric_covars, function(x){
            coli <- which(tolower(names(fit_sightings)) == x)
            bads <- which(is.na(as.numeric(fit_sightings[,coli])))
            return(bads)
          }) %>%
          unlist %>% unique
        bads
        # If there are any, remove them and announce that you are doing so.
        if(length(bads)>0){
          if(verbose){
            message('\nRemoving rows with invalid numeric covariate data from the detection-function-fitting dataset: ***')
          }
          # Show which rows are being removed:
          covarcols <- which(tolower(names(fit_sightings)) %in% numeric_covars)
          baddf <- fit_sightings[bads,] %>% select(Cruise, DateTime, SightNoDaily, line_num, species, all_of(covarcols))
          if(verbose){
            print(baddf)
          }
          # Remove the rows
          fit_sightings <- fit_sightings[-bads, ]
        }
      }
    }
  }

  # Handle invalid school size estimates
  if(!is.null(covariates)){
    if('lnsstot' %in% tolower(covariates)){
      if('ss_valid' %in% names(fit_sightings)){
        (bads <- which(fit_sightings$ss_valid == FALSE))
        # If there are any, remove them and announce that you are doing so.
        if(length(bads)>0){
          if(verbose){
            message('\nRemoving rows with invalid school size estimates from the detection-function-fitting dataset: ***')
          }
          # Show which rows are being removed:
          baddf <- fit_sightings[bads,] %>% select(Cruise, DateTime, SightNoDaily, line_num, species, best, ss_valid)
          if(verbose){
            print(baddf)
          }
          # Remove the rows
          fit_sightings <- fit_sightings[-bads, ]
        }
      }
    }
  }

  ##############################################################################
  ##############################################################################
  # Get detection function sample size

  if(verbose){message('\nPreparing sample size table ...')}

  (sample_size <- fit_sightings %>%
      dplyr::group_by(species) %>%
      dplyr::summarize(Ntot = n(),
                       Ndet = length(which(PerpDistKm <= truncation_distance))) %>%
      dplyr::mutate(TD = truncation_distance,
                    pool = pool))

  # Now remove sightings beyond the truncation distance
  fit_sightings <- fit_sightings %>% dplyr::filter(PerpDistKm <= truncation_distance)

  # Remove 'Other' species, if needed
  other_sits <- c()
  if(other_species == 'remove'){
    (other_sits <- fit_sightings$SightNoDaily[which(fit_sightings$species == 'Other')])
    fit_sightings <- fit_sightings %>% dplyr::filter(species != 'Other')
    if('Other' %in% sample_size$species){
      sample_size$Ndet[sample_size$species == 'Other'] <- 0
    }
  }

  sample_size

  # Add unique identifier for each row
  fit_sightings$i_fit <- 1:nrow(fit_sightings)

  ##############################################################################
  ##############################################################################
  # Simplify cue or bino?

  if(verbose & any(simplify_cue, simplify_bino)){message('Polishing cue and/or bino (sighting method) levels ...')}

  if(simplify_cue){
    others <- which(fit_sightings$Cue %in% c(0,1,2,4,7))
    if(length(others)>0){fit_sightings$Cue[others] <- 5}
  }

  if(simplify_bino){
    others <- which(fit_sightings$Method != 4)
    if(length(others)>0){fit_sightings$Method[others] <- 5}
  }

  ##############################################################################
  ##############################################################################
  # Process factorial covariates

  if(verbose){message('\nInventorying covariate factors ...')}

  #=============================================================================
  # Setup master list of factor levels for each covariate

  covar_levels <- list()
  if(any(covariates_factor)){ # only proceed if at least one covariate is intended as a factor

    # Get the covariates intended as factors
    (covar_numeric <- covariates[! covariates_factor])
    (covar_factors <- covariates[covariates_factor])
    cfi=2 # loop through each of them
    for(cfi in 1:length(covar_factors)){
      (covari <- covar_factors[cfi]) # get this covariate

      # Find the matching column in the fit_filters sightings
      (dist_matchi <- which(tolower(names(fit_sightings))==covari))
      (dist_covari <- fit_sightings[,dist_matchi])

      # Assess sample size
      (level_counts <- dist_covari %>% table)
      sufficient <- TRUE
      if(length(level_counts) < covariates_levels){sufficient <- FALSE} ; sufficient
      if(min(level_counts) < covariates_n_per_level){sufficient <- FALSE} ; sufficient

      # If sample size is sufficient, add to the master list
      if(sufficient){
        fit_sightings[, dist_matchi] <- factor(fit_sightings[, dist_matchi])
        covar_levels[[length(covar_levels) + 1]] <- unique(dist_covari)
        names(covar_levels)[length(covar_levels)] <- covari
      }
    }
    covar_levels

    # Use covar_levels to update the list of covariates to try in detection function fitting
    new_covariates <- covar_numeric
    new_covariates_factor <- rep(FALSE, times=length(new_covariates))
    if(length(covar_levels)>0){
      new_covariates <- c(new_covariates, names(covar_levels))
      new_covariates_factor <- c(new_covariates_factor, rep(TRUE, times=length(covar_levels)))
    }
    (new_covariates)
    covariates <- new_covariates
    (new_covariates_factor)
    covariates_factor <- new_covariates_factor
  }

  covar_levels
  covariates
  if(verbose){message('--- covariates that will be used in detection function fitting:\n    ',paste(covariates, collapse=', '))}

  ##############################################################################
  ##############################################################################
  # Begin loops
  ##############################################################################
  ##############################################################################
  # Do the analysis once for the official estimate,
  # then optionally again for bootstrapping

  # Stage the results object
  RESULT <- list(pool = pool,
                 inputs = list(pool = fit_filters,
                               df = df_settings,
                               estimates = estimates,
                               bootstraps = bootstraps,
                               use_g0 = use_g0,
                               ss_correction = ss_correction),
                 estimate = data.frame(), # stage result
                 df = list())

  # Prepare instructions for estimates/bootstrap loop(s)
  loops <- c('estimate')
  if(bootstraps > 1){ loops <- c(loops, 'bootstrap') }
  loopi <- loops[1] # for debugging
  loopi <- loops[2] # for debugging

  for(loopi in loops){
    # if this is just the estimate loop, iter is 1
    (niter <- ifelse(loopi == 'bootstrap', bootstraps, 1))
    results <- data.frame() # stage final results object
    df_curves <- data.frame()

    # When loopi == 'estimate', this is run once *without* re-sampling, to get the formal estimate
    iter <- 1 # for debugging
    for(iter in 1:niter){

      # Attempt analysis up to 3 times before giving up
      # This is in place to handle the random refactoring issue thrown by mrds
      try_counter <- 0 # if the analysis fails, this will count number of attempts
      try_status <- NULL # when the analysis works, this will be changed to 1
      while(try_counter < 50 && is.null(try_status)){
        if(try_counter > 0 && verbose){message('\nSomething went wrong. Trying again ...\n')}
        try({

          if(niter > 1 & verbose){
            message('\n=====================================================')
            message('BOOTSTRAP ITERATION ',iter)
            message('=====================================================')
          }

          ##########################################################################
          ##########################################################################
          # Estimating the detection function

          if(verbose){message('\n___________________________________________')}
          if(verbose){message('Estimating the detection function ...\n')}

          # Reset data objects to ensure they represent the original non-bootstrapped data
          segments <- dist_segments
          sightings <- fit_sightings
          das <- dist_das

          #=========================================================================

          if(niter > 1){
            # If bootstrapping is happening, re-sample the data
            # this function maintains the relative distribution of effort across strata
            if(verbose){message('Preparing bootstrap dataset ...')}
            bs_data <- prep_bootstrap_datasets(segments,sightings)
            segments <- bs_data$segments
            sightings <- bs_data$sightings
            sightings$i_fit <- 1:nrow(sightings) # Replace unique identifier
            segment_picks <- bs_data$segment_picks

            # Re-factor sightings
            #sightings <- refactor_covariates(sightings, covar_levels)$data

            if(FALSE){ # NOT RUN
              # For development only
              # tests to make sure bootstrap resampling is working well
              sitis <- c()
              for(i in 1:10000){
                segments <- dist_segments
                sightings <- fit_sightings
                bs_data <- prep_bootstrap_datasets(segments,sightings)
                siti <- bs_data$sightings
                siti <- siti %>% dplyr::filter(OnEffort == TRUE,
                                               EffType %in% abund_eff_types,
                                               round(Bft) %in% abund_bft_range,
                                               PerpDistKm <= truncation_distance,
                                               species == '046',
                                               year == 2021,
                                               stratum == 'CNMI_EEZ')
                siti %>% nrow
                sitis <- c(sitis, nrow(siti))
                message(i)
              }
              hist(sitis)
              length(which(sitis == 0))
            }
          }

          if(loopi == 'bootstrap' & verbose){message('Fitting detection function ...')}

          #=========================================================================
          # Estimate detection function
          # using LTabundR function df_fit()

          df <- NULL
          df <- df_fit(sightings = sightings,
                       truncation_distance = fit_filters$truncation_distance,
                       covariates = covariates,
                       detection_function_base = detection_function_base,
                       base_model = base_model,
                       delta_aic = delta_aic,
                       toplot=toplot,
                       verbose= ifelse(loopi == 'estimate', verbose, FALSE))

          # Review
          df$all_models
          df %>% names
          df$best_models

          # Add name of this species pool to the df results
          df$best_models$pool <- pool
          df$all_models$pool <- pool

          # Store ds objects of best models
          df_models <- df$best_objects
          df_models %>% length
          df_models[1]

          # Get averaged df curve for best models, using LTabundR function df_curve()
          mod_curve <- df_curve(df_models, covariates, truncation_distance)

          # Get esw column from df_fit() results, then join to sightings
          fitted_sightings <- df$sightings
          fitted_sightings$esw
          (fit_sit_to_join <- fitted_sightings %>% dplyr::select(i_fit, esw))
          #fit_sit_to_join <- fitted_sightings %>% dplyr::select(SightNoDaily, esw)

          # Now assign the ESW for each unique sighting number to each species within that sighting
          new_sightings <- dplyr::left_join(sightings, fit_sit_to_join,
                                            by='i_fit', relationship = 'many-to-many')
          #new_sightings <- dplyr::left_join(sightings, fit_sit_to_join,
          #                                  by='SightNoDaily', relationship = 'many-to-many')
          # new_sightings <- dplyr::left_join(dist_sightings, fit_sit_to_join,
          #                                   by='SightNoDaily', relationship = 'many-to-many')
          # # This effectively 'ungroups' the sightings that were previously grouped and made Other (if any)

          dist_sightings$SightNoDaily %>% unique %>% sort
          #sightings$SightNoDaily %>% unique %>% sort
          #fitted_sightings$SightNoDaily %>% unique %>% sort
          #new_sightings$SightNoDaily %>% unique %>% sort
          new_sightings$esw
          which(is.na(new_sightings$esw)) %>% length

          # Remove Other sightings, EVEN from the abundance estimation stage
          if(other_species == 'remove' & length(other_sits)>0){
            new_sightings %>% nrow
            new_sightings <- new_sightings %>% dplyr::filter(! SightNoDaily %in% other_sits)
            new_sightings %>% nrow
          }

          loopi
          if(loopi == 'estimate'){
            # Finalize estimate results
            View(df$all_models, title='Distance')
            df$sample_size <- sample_size
            df$curve <- mod_curve
            RESULT$df <- df
          }

          ##########################################################################
          ##########################################################################
          # Density / abundance

          if(verbose){message('\n___________________________________________')}
          if(verbose){message('Estimating density & abundance ...\n')}

          # Filter sightings to strictly systematic effort
          abund_sightings <- new_sightings
          abund_sightings$id <- 1:nrow(abund_sightings) # add an id row
          abund_sightings <-
            abund_sightings %>%
            dplyr::filter(OnEffort == TRUE,
                          EffType %in% abund_eff_types,
                          round(Bft) %in% abund_bft_range,
                          PerpDistKm <= truncation_distance)
          nrow(abund_sightings)
          abund_sightings$species %>% table

          # Same for segments
          names(segments)
          abund_segments <-
            segments %>%
            dplyr::filter(OnEffort == TRUE,
                          EffType %in% abund_eff_types,
                          round(avgBft) %in% abund_bft_range)
          segments %>% nrow
          abund_segments %>% nrow

          # Same for DAS
          abund_das <- dist_das

          # Stage abundance results
          abund_results <- data.frame()

          # Loop through estimates
          (abund_loops <- length(estimates))
          abund_loopi <- 2 # debugging
          for(abund_loopi in 1:abund_loops){
            (est_filters <- estimates[[abund_loopi]])

            # Grab g0 parameters from the g0 tables ================================

            if(use_g0){
              g0_tabli <- g0_tables[[abund_loopi]]
              g0_tabli
              g0_i <- g0_tabli$g0
              g0_cvi <- g0_tabli$g0_cv
              g0_param <- g0_tabli$g0_param
              g0_threshold <- g0_tabli$g0_threshold
            }
            g0_i ; g0_cvi  ; g0_param ; g0_threshold

            # Grab species code(s) =================================================

            (sppi <- est_filters$spp)
            if(other_species == 'coerce'){sppi <- 'Other'}
            sppi

            # Filter according to inputs
            if(verbose){message('--- --- Filtering data according to `estimates` input ...')}
            cruisi <- NULL
            if('cruises' %in% names(est_filters)){cruisi <- est_filters$cruises}
            cruisi
            sppi
            if(!is.null(sppi) && length(sppi)==1 && sppi=='Other'){sppi <- NULL}
            sppi
            filtered_data <- filter_cohort(abund_segments,
                                           abund_sightings,
                                           abund_das,
                                           analysis_only = TRUE,
                                           spp = sppi,
                                           years = est_filters$years,
                                           regions = est_filters$regions,
                                           not_regions = est_filters$regions_remove,
                                           cruises = cruisi,
                                           verbose = ifelse(loopi == 'estimate', verbose, FALSE))
            region_segments <- filtered_data$segments
            region_sightings <- filtered_data$sightings

            # Get abundance estimation area
            (abund_area <- est_filters$area)

            # Check to see if manual effort length has been supplied
            (est_effort <- est_filters$forced_effort)

            if(niter > 1){
              # For this non-parametric bootstrap iteration,
              # draw a parametric bootstrap sample of g0
              #g0_i <- c(1,1)
              if(use_g0){
                if(g0[1] != 1 & g0_cv[1] != 0 & all(!is.na(g0_param[1,]))){
                  g0_i[1]= plogis(rnorm(1,g0_param[1,1],g0_param[1,2]))
                }
                g0_i[2] <- g0_i[1]
                if(g0[2] != g0[1] & g0[2] != 1 & g0_cv[2] != 0 & all(!is.na(g0_param[2,]))){
                  g0_i[2]= plogis(rnorm(1,g0_param[2,1],g0_param[2,2]))
                }
                g0_i
                if(verbose){message('--- --- g(0) for this bootstrap: ', round(g0_i[1], 5),' (small schools) | ', round(g0_i[2], 5),' (large schools)')}
              }
            }

            #===============================================================
            # Make a title for this region pool
            # if a region name was manually supplied, use it. If not, rename it
            # based on regions and regions_remove

            region_title <- est_filters$region_title
            if(is.null(region_title)){
              if(!is.null(est_filters$regions_remove)){
                (region_title <- paste0('(',
                                        paste(est_filters$regions, collapse = ' + '),
                                        ') - (',
                                        paste(est_filters$regions_remove, collapse = ' - '),
                                        ')'))
              }else{
                (region_title <- paste0('(',
                                        paste(est_filters$regions, collapse = ' + '),
                                        ')'))
              }
            }

            #===============================================================
            # Estimate abundance

            abundi <- abundance(segments = region_segments,
                                sightings = region_sightings,
                                das = abund_das,
                                strata = strata,
                                truncation_distance = fit_filters$truncation_distance,
                                ss_correction = ss_correction,
                                use_g0 = use_g0,
                                g0 = g0_i,
                                g0_threshold = g0_threshold,
                                region_pool = TRUE,
                                region_pool_name = region_title,
                                region_pool_area = abund_area,
                                year_pool = TRUE, #year_pool,
                                forced_effort = est_effort,
                                verbose= ifelse(loopi == 'estimate', verbose, FALSE))

            abundi # debugging review
            #abundi %>% print
            if(!is.null(abundi)){
              if(is.null(sppi)){sppi <- 'Other'}
              abundi <- data.frame(title = est_filters$title,
                                   species = paste(sppi, collapse='-'),
                                   abundi) ; abundi
              abund_results <- rbind(abund_results, abundi)
            } # end of if abundi is NULL
            message('')
          } # end of abund loops

          abund_results
          if(loopi == 'estimate'){
            # Finalize estimate results
            estimate_results <- abund_results
            View(estimate_results, title='Abundance')
            RESULT$estimate <- estimate_results
          }

          ##########################################################################
          ##########################################################################
          # Prepare result object(s)

          if(niter > 1){
            # Handle result for bootstrapped data
            # Saving only the output for specific regions/years
            # and adding to building dataframe
            (results_iter <- abund_results)

            results_iter <-
              results_iter %>%
              dplyr::mutate(i = iter) %>%
              dplyr::select(i, title, species, Region, year, km, ESW_mean, g0_est, n, ER, D, size_mean, N)

            results <- rbind(results, results_iter)

            # Add df curve to growing dataframe archiving the curves for each bootstrap
            mod_curve$km <- NULL
            curvi <- mod_curve %>% t %>% as.data.frame()
            curvi
            df_curves <- rbind(df_curves, curvi)

          }

          # for debugging bootstraps:
          # message('Max abundance = ',format(round(max(abund_results$N)), big.mark=','))
          #continue <- readline(paste0("Continue? 1 = yes, 0 = no"))
          #if(continue == 0){break}

          # Update results file
          if(!is.null(results_file)){
            saveRDS(RESULT, file=results_file)
          }

          try_status <- 1 # if the code got to here, try_status should no longer be NULL
        }) # end of try
        try_counter <- try_counter + 1
      } # end of while loop
      if(is.null(try_status)){stop(try_counter,' failed attempts to complete the analysis for this iteration! Stopped trying!')}
    } # end of iter-th bootstrapping loop
  } # end of estimate/bootstrap loop

  ##############################################################################
  ##############################################################################
  # Handle bootstrap results

  RESULT %>% names
  RESULT$estimate

  if(niter > 1){
    RESULT$bootstrap <- list(summary = NULL,
                             details = results,
                             df = df_curves)
    RESULT %>% names
    RESULT$bootstrap

    head(results)
    bs_summary <-
      results %>%
      dplyr::filter(is.finite(N) == TRUE) %>%
      dplyr::group_by(title, Region, year) %>%
      dplyr::summarize(species = paste(unique(species), collapse=', '),
                       iterations = dplyr::n(),
                       ESW_mean = mean(ESW_mean, na.rm=TRUE),
                       g0_mean = mean(g0_est, na.rm=TRUE),
                       g0_cv = sd(g0_est, na.rm=TRUE) / mean(g0_est, na.rm=TRUE),
                       km = mean(km, na.rm=TRUE),
                       ER = mean(ER, na.rm=TRUE),
                       D = mean(D, na.rm=TRUE),
                       size = mean(size_mean, na.rm=TRUE),
                       Nmean = mean(N, na.rm=TRUE),
                       Nmedian = median(N, na.rm=TRUE),
                       Nsd = sd(N, na.rm=TRUE),
                       CV = Nsd / Nmean,
                       L95 = lta_ci(Nmean, N)$bca_lognormal[1],
                       U95 = lta_ci(Nmean, N)$bca_lognormal[2])
                       #L95 = coxed::bca(N)[1],
                       #U95 = coxed::bca(N)[2])
    bs_summary
    RESULT$bootstrap$summary <- bs_summary

    # Update results file
    if(!is.null(results_file)){
      saveRDS(RESULT, file=results_file)
    }
  }

  message('Finished at ', Sys.time())
  if(try_counter > 1){
    message(' --- FYI: during the bootstrapping process, there were ', try_counter - 1,' failed attempts.')
  }

  return(RESULT)
}
