#' Review of `cruz` object structure
#'
#' This function prints an overview of the list structure and sample sizes
#' within your `cruz` object, which is produced from `LTabundR::process_surveys()`.
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#'
#' @return Nothing; messages are printed to the Console.
#' @export
#'
cruz_structure <- function(cruz){

  if(FALSE){ #========================================== debugging -- not run!
    data('example_cruz')
    cruz <- example_cruz
  } # ================================================== end debugging

  message('"cruz" list structure ========================')

  message('\n$settings')
  message('         $strata --- with ', length(cruz$settings$strata),' polygon coordinate sets')
  message('         $survey --- with ', length(cruz$settings$survey),' input arguments' )
  message('         $cohorts --- with ', length(cruz$settings$cohorts),' cohorts specified, each with ', length(cruz$settings$cohorts[[1]]),' input arguments' )

  message('\n$strata')
  message('       ... containing a summary dataframe of ',nrow(cruz$strata),' geostrata and their spatial areas')
  message('       ... geostratum names:')
  message('           ', paste(cruz$strata$stratum, collapse=', '))

  message('\n$cohorts')
  # Loop through each cohort
  coho <- cruz$cohorts
  for(i in 1:length(coho)){
    message('\n        $',names(coho)[i])
    message('            geostrata: ', paste(cruz$settings$cohorts[[i]]$strata, collapse=', '))
    cohi <- coho[[i]]
    for(j in 1:length(cohi)){
      nami <- names(cohi)[[j]]
      if(nami == 'segments'){
        message('            $segments  --- with ', nrow(cohi$segments),' segments (median = ', round(median(cohi$segments$dist), 1),' km)')
      }
      if(nami == 'sightings'){
        message('            $sightings --- with ', nrow(cohi$sightings),' detections')
      }
      if(nami == 'das'){
        message('            $das       --- with ', nrow(cohi$das),' data rows')
      }
      if(nami == 'subgroups'){
        message('            $subgroups --- with ', nrow(cohi$subgroups$subgroups),' subgroups, ', nrow(cohi$subgroups$sightings),' sightings, and ', nrow(cohi$subgroups$events),' events')
      }
    }
  }
}
