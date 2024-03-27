#' Estimate CV and 95% confidence intervals with the delta method
#'
#' This function loops through a list of results from `LTabundR::lta()` and updates
#' their CV and 95% CI estimates, which were produced using a bootstrap method,
#' using new estimates based on the delta method.

#' @param ltas A `list` of outputs from the function `lta()`.
#' @param verbose Boolean; print report of new estimates to the console?
#'
#' @return An updated version of the `ltas` input with new values for `CV`, `L95`, and `U95` in the `$bootstrap$summary` slot.
#' @export
#' @import dplyr
#' @import tidyr
lta_delta_method <- function(ltas,
                             verbose = TRUE){

  if(FALSE){ #==================================================================
    # dummy objects for debugging
    # set wd to a lta_manual folder containing results of lta()
    ltas <- lta_enlist('lta_manual/')
  } #===========================================================================

  ltass <- ltas
  i=1
  for(i in 1:length(ltas)){

    #===========================================================================
    # Get original results for this LTA list
    lti <- ltas[[i]]
    (esti <- lti$estimate)
    (esti2join <- esti %>% select(title, Region, year, D_real=D, N_real = N, g0_real=g0_est))

    (ole_summ <- lti$bootstrap$summary)
    (booti <- lti$bootstrap$details) %>% head

    if(FALSE){ #================================================================
      # debugging only
      # check that your D equation is correct
      esti <- esti %>% mutate(D_test = (n*size_mean)/(2*ESW_mean*km*g0_est))
      for(i in 1:nrow(esti)){
        estii <- esti[i,]
        if(estii$D > 0){
          message('LTA result D = ', round(estii$D, 5), ' | manually checked D = ', round(estii$D_test, 5))
        }
      }
    } #=========================================================================

    #===========================================================================
    # Re-calculate CV and CIs with delta method
    # grouping by each species-region-year
    suppressMessages({
      boots <- dplyr::left_join(booti, esti2join)

      (bootsumm <-
          boots %>%
          group_by(title, Region, year, D_real, N_real, g0_real) %>%
          mutate(D = (n*size_mean)/(2*ESW_mean*km *g0_real)) %>%
          summarize(g0_cv = g0_cv_small[1],
                    Dsd = sd(D, na.rm=TRUE),
                    Dvar = var(D, na.rm=TRUE)) %>%

          # Calculate delta method CV
          mutate(Dcv = Dsd / D_real) %>%
          mutate(CV_delta = sqrt(Dcv^2 + g0_cv^2)) %>%

          # Calulate log-normal CIs
          # https://influentialpoints.com/Training/log_normal_confidence_intervals.htm
          mutate(var_logD = log(1 + (Dvar/(D_real^2)))) %>%
          mutate(C = exp(1.96*sqrt(var_logD))) %>%
          mutate(L95_delta = N_real / C,
                 U95_delta = N_real * C) %>%

          # Clean up for joining
          select(title, Region, year, CV_delta, L95_delta, U95_delta))

      # Join new calculations to original bootstrap summary, replacing key values
      (new_summ <- dplyr::left_join(ole_summ, bootsumm) %>%
          mutate(CV_boot = CV) %>%
          mutate(CV = CV_delta) %>%
          mutate(L95_boot = L95,
                 U95_boot = U95) %>%
          mutate(L95 = L95_delta,
                 U95 = U95_delta))
    })
    #===========================================================================

    # Print results
    if(verbose){
      new_summ
      for(i in 1:nrow(new_summ)){
        newi <- new_summ[i,]
        if(!is.na(newi$CV)){
          message(newi$title,' ', newi$Region,' ', newi$year)
          message('--- CV      bootstrapped = ', round(newi$CV_boot, 2))
          message('            delta method = ', round(newi$CV, 2))
          message('--- 95% CI  bootstrapped = ', round(newi$L95_boot), ' - ', round(newi$U95_boot))
          message('            delta method = ', round(newi$L95), ' - ', round(newi$U95))
          message('')
        }
      }
    }

    # Update bootstrap summary in LTI object
    lti$bootstrap$summary <- new_summ

    # Update LTAS list
    ltass[[i]] <- lti
  }

  # Return updated LTAS list
  return(ltass)
}
