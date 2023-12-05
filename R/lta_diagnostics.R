#' Run diagnostics on a LTA result
#'
#' @param lta_result The result of `lta()` for a single species pool.
#' @param options Numeric vector indicating which output options to return.
#' These options are printed for the user when `describe_options` is `TRUE`.
#' @param describe_options Boolean; if `TRUE`, the function will print a list of output options for the user.
#' This helps the user determine which numbers to request with the input `options`.
#' @param wait Boolean; if `TRUE`, the function will wait for the user to press `<Enter>` in between each output option.
#'
#' @return A series of printed tables and plots, which the user can step through using the `Enter` key.
#' @export
#' @import dplyr
#' @import ggplot2
#'
lta_diagnostics <- function(lta_result,
                            options = 1:8,
                            describe_options = FALSE,
                            wait = TRUE){

  if(FALSE){
    # debugging
    ltas <- lta_enlist("/Users/ekezell/Desktop/projects/noaa ltabundr/marianas/lta_barlow/")
    lta_result <- ltas[[1]]
    lta_result <- ltas[[2]]
    lta_result <- ltas[[3]]

    options = NULL
    describe_options = TRUE
    wait = TRUE

    # try it
    lta_diagnostics(lta_result)
    lta_diagnostics(lta_result, options=1)
    lta_diagnostics(lta_result,
                    options = c(),
                    describe_options = TRUE)
  }

  if(describe_options){
    message('')
    message('List of options for outputs to provide: ===============')
    message('(use numbers in the input `options`)')
    message('')
    message('1 - Point estimate (encounter rate, density, abundance, g(0), etc.)')
    message('2 - Summary of bootstrap iterations, including CV of density/abundance')
    message('3 - Plot of detection function')
    message('4 - Histogram of bootstrapped detection counts')
    message('5 - Histogram of bootstrapped g(0) values')
    message('6 - Histogram of bootstrapped abundance estimates')
    message('7 - Scatterplot of abundance ~ g(0) relationship in boostraps')
    message('8 - Time series of point estimate CV as bootstraps accumulate')
    message('')
    message('======================================================')
  }

  df <-
    lta_result$bootstrap$details %>%
    mutate(Estimate = paste(title, Region, 'in', year))
  df %>% head


  # ============================================================================
  if(1 %in% options){
    message('\nBest estimate of density & abundance:')
    lta_result$estimate %>%
      mutate(km = round(km),
             Area_covered = round(Area_covered),
             ESW_mean = round(ESW_mean, 2),
             g0_est = round(g0_est, 3),
             g0_small = round(g0_small, 3),
             g0_large = round(g0_large, 3),
             g0_cv_small = round(g0_cv_small, 3),
             g0_cv_large = round(g0_cv_large, 3),
             ER_clusters = round(ER_clusters, 4),
             D_clusters = round(D_clusters, 4),
             N_clusters = round(N_clusters, 1),
             size_mean = round(size_mean, 1),
             size_sd = round(size_sd, 1),
             ER = round(ER, 4),
             D = round(D, 4),
             N = round(N)) %>%
      as.data.frame %>%
      print
    if(wait & length(which(options > 1))>0){readline(prompt="Press [enter] to continue")}
  }

  if(!is.null(lta_result$bootstrap)){

    if(2 %in% options){
      # ============================================================================
      message('\nBootstrap summary:')
      lta_result$bootstrap$summary %>%
        as.data.frame %>%
        print
      if(wait & length(which(options > 2))>0){readline(prompt="Press [enter] to continue")}
    }

    # ============================================================================
    if(3 %in% options){
      message('\nSee plot of detection function bootstraps...')
      df_plot(lta_result)
      if(wait & length(which(options > 3))>0){readline(prompt="Press [enter] to continue")}
    }


    # ============================================================================
    if(4 %in% options){
      message('\nSee plot of bootstrapped sighting counts...')
      p <-
        ggplot(df, aes(x=n)) +
        geom_histogram(alpha=.5, fill='darkblue', color='white', binwidth=.9) +
        facet_wrap(~Estimate, ncol=1, scales='free_x') +
        xlab('Sighting counts used in bootstraps') +
        ylab('')
      print(p)
      if(wait & length(which(options > 4))>0){readline(prompt="Press [enter] to continue")}
    }

    # ============================================================================
    if(5 %in% options){
      message('\nSee plot of bootstrapped g(0) estimates...')
      p <-
        ggplot(df, aes(x=g0_est)) +
        geom_histogram(alpha=.5, fill='darkblue', color='white') +
        facet_wrap(~Estimate, ncol=1, scales='free_x') +
        xlab('Bootstrapped g(0) estimates') +
        ylab('Frequency')
      print(p)
      if(wait & length(which(options > 5))>0){readline(prompt="Press [enter] to continue")}
    }

    # ============================================================================
    if(6 %in% options){
      message('\nSee plot of bootstrapped abundance estimates...')
      p <-
        ggplot(df, aes(x=N)) +
        geom_histogram(alpha=.5, fill='darkblue', color='white') +
        facet_wrap(~Estimate, ncol=1, scales='free_x') +
        xlab('Bootstrapped abundance estimates') +
        ylab('Frequency')
      print(p)
      if(wait & length(which(options > 6))>0){readline(prompt="Press [enter] to continue")}
    }

    # ============================================================================
    if(7 %in% options){
      message('\nSee plot of bootstrapped abundance ~ g(0) relationship')
      p <-
        ggplot(df, aes(x=g0_est, y=N)) +
        geom_point() +
        facet_wrap(~Estimate, ncol=1, scales='free_y') +
        xlab('Bootstrapped g(0)') +
        ylab('Bootstrapped abundance')
      print(p)
      if(wait & length(which(options > 7))>0){readline(prompt="Press [enter] to continue")}
    }

    # ============================================================================
    if(8 %in% options){
      message('\nSee plot of running CV of bootstrap iterations...')
      dfcv <-
        df  %>%
        group_by(Estimate) %>%
        mutate(cummean_N = cummean(N),
               devN = (N - cummean_N)^2,
               cumdev = cumsum(devN),
               cumvar = cumdev / i,
               cumsd = sqrt(cumvar),
               cumcv = cumsd / cummean_N) %>%
        ungroup()
      p <-
        ggplot(dfcv, aes(x=i, y=cumcv, color=Estimate)) +
        geom_path() +
        xlab('Bootstrap iteration') +
        ylab('CV of density / abundance')
      print(p)
    }
  } # end of if bootstrap is null

  # ============================================================================
}
