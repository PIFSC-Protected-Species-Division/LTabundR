#' Run diagnostics on a LTA result
#'
#' @param lta_result The result of `lta()` for a single species pool.
#'
#' @return A series of printed tables and plots, which the user can step through using the `Enter` key.
#' @export
#' @import dplyr
#' @import ggplot2
#'
lta_diagnostics <- function(lta_result){

  if(FALSE){
    # debugging
    ltas <- lta_enlist("/Users/ekezell/Desktop/projects/noaa ltabundr/marianas/lta_barlow/")
    lta_result <- ltas[[1]]
    lta_result <- ltas[[2]]
    lta_result <- ltas[[3]]

    # try it
    lta_diagnostics(lta_result)
  }

  df <-
    lta_result$bootstrap$details %>%
    mutate(Estimate = paste(title, Region, 'in', year))
  df %>% head


  # ============================================================================
  message('\nBest estimate of density & abundance:')
  lta_result$estimate %>%
    mutate(km = round(km),
           Area_covered = round(Area_covered),
           ESW_mean = round(ESW_mean, 2),
           g0_est = round(g0_est, 3),
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

  if(!is.null(lta_result$bootstrap)){

    # ============================================================================
    message('\nBootstrap summary:')
    lta_result$bootstrap$summary %>%
      as.data.frame %>%
      print

    # ============================================================================
    message('\nSee plot of detection function bootstraps...')

    df_plot(lta_result)

    readline(prompt="Press [enter] to continue")

    # ============================================================================
    message('\nSee plot of bootstrapped sighting counts...')

    p <-
      ggplot(df, aes(x=n)) +
      geom_histogram(alpha=.5, fill='darkblue', color='white', binwidth=.9) +
      facet_wrap(~Estimate, ncol=1, scales='free_x') +
      xlab('Sighting counts used in bootstraps') +
      ylab('')

    print(p)
    readline(prompt="Press [enter] to continue")

    # ============================================================================
    message('\nSee plot of bootstrapped g(0) estimates...')

    p <-
      ggplot(df, aes(x=g0_est)) +
      geom_histogram(alpha=.5, fill='darkblue', color='white') +
      facet_wrap(~Estimate, ncol=1, scales='free_x') +
      xlab('Bootstrapped g(0) estimates') +
      ylab('Frequency')

    print(p)
    readline(prompt="Press [enter] to continue")

    # ============================================================================
    message('\nSee plot of bootstrapped abundance estimates...')

    p <-
      ggplot(df, aes(x=N)) +
      geom_histogram(alpha=.5, fill='darkblue', color='white') +
      facet_wrap(~Estimate, ncol=1, scales='free_x') +
      xlab('Bootstrapped abundance estimates') +
      ylab('Frequency')

    print(p)
    readline(prompt="Press [enter] to continue")

    # ============================================================================
    message('\nSee plot of bootstrapped abundance ~ g(0) relationship')

    p <-
      ggplot(df, aes(x=g0_est, y=N)) +
      geom_point() +
      facet_wrap(~Estimate, ncol=1, scales='free_y') +
      xlab('Bootstrapped g(0)') +
      ylab('Bootstrapped abundance')

    print(p)
    readline(prompt="Press [enter] to continue")

    # ============================================================================
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

  } # end of if bootstrap is null

  # ============================================================================
  message('\nNo more diagnostics to show!')
}
