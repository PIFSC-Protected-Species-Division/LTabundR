#' Plot LTA results
#'
#' This function returns abundance estimate plots (i.e., with year on the x axis,
#' abundance on the y axis, the best estimate represented by a point, and the confidence
#' interval represented by a vertical line). This function is capable of producing multi-panel
#' plots for multiple species, and it is designed to produce a dynamic layout according to the
#' number of species pools provided.
#'
#' @param species A character vector of estimate titles. If `NULL`, all titles within `lta_result` will be used.
#' @param lta_result The result of `lta()` or `lta_enlist()`. See their documentation for details.
#' @param years A numeric vector of years whose estimates will be included in all plots,
#' even if an estimate is not available for a given `species` (to make the x-ranges of all plots equal)
#' @param nrows Number of rows in multi-panel plot. Can be `NULL` (which is the default).
#' @param ncols Number of columns in multi-panel plot. Can be `NULL` (which is the default).

#' @import ggplot2
#' @import dplyr
#'
#' @return A `ggplot2` plot, or many `ggplot2` plots arranged by `gridExtra::grid.arrange()`.
#'
#' @export
#'
lta_plot <- function(lta_result,
                     species = NULL,
                     years = NULL,
                     nrows = NULL,
                     ncols = NULL){

  if(FALSE){ # debugging only -- not ! =========================================
    species = c("Pantropical spotted dolphin",
                "Striped dolphin",
                "Spinner dolphin",
                "Rough-toothed dolphin",
                "Bottlenose dolphin",
                "Risso's dolphin")
    years <- c(2010, 2017)
    nrows = 2 ; ncols = 3

    species <- c('Pantropical spotted dolphin', 'Blue whale')
    years <- c(2002, 2010, 2017)
    species = NULL
    years = NULL
    nrows = NULL
    ncols = NULL

    # try it
    lta_plot(species, lta_result, years, nrows=1, ncols=2)
  } #===========================================================================

  ##############################################################################
  # Core plotting function

  lta_plot_core <- function(species_title,
                            lta_result,
                            years = NULL){

    if(FALSE){ # for debugging only  ===========================================
      species_title <- c('Pantropical spotted dolphin')
      species_title <- c('Blue whale')
      years <- c(2002, 2010, 2017)
      lta_plot_core(species_title, years, lta_result)
    } # end debugging  =========================================================

    # Simplify results =========================================================
    if(is.null(names(lta_result))){
      # This is a multi-stock results object

      estimates <- data.frame()
      bootstraps <- data.frame()
      for(i in 1:length(lta_result)){
        mri <- lta_result[[i]]
        estimates <- rbind(estimates, mri$estimate)
        bootstraps <- rbind(bootstraps, mri$bootstrap$summary)
      }
      estimates
      bootstraps

    }else{
      # This is a result for a single species pool
      estimates <- lta_result$estimate
      bootstraps <- lta_result$bootstrap$summary
    }

    # Join confidence intervals (bootstraps) to estimates  =====================
    suppressWarnings({ suppressMessages({
      results <-
        dplyr::left_join(estimates,
                         bootstraps %>% select('Region', 'species','year', 'CV', 'L95', 'U95'),
                         by=c('Region', 'species','year'))
    }) })
    results
    if('title.x' %in% names(results)){
      results <- results %>%
        dplyr::rename(title = title.x)
    }

    # Filter to species of interest  ===========================================
    #print(species_title)
    (matches <- which(results$title == species_title))
    if(length(matches)==0){stop('We could not find an estimate title matching your `species_title` input!')}
    (mri <- results[matches,])

    # Get years if needed  =====================================================
    mri$year <- as.numeric(as.character(mri$year))
    if(is.null(years)){years <- mri$year %>% unique}

    if(! all(mri$N == 0)){
      # only proceed if at least one of the estimates is non-zero.

      # Revise zero estimates  =================================================
      bads <- which(mri$N == 0)
      if(length(bads)>0){
        mri$N[bads] <- NA
        mri$L95[bads] <- NA
        mri$U95[bads] <- NA
      }

      # Prepare plot  ==========================================================
      p <-
        ggplot2::ggplot(mri,
                        ggplot2::aes(x=year, y=N, pch=Region)) +
        ggplot2::geom_point(size=2,
                            position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::geom_errorbar(mapping=aes(x=year, ymin=L95, ymax=U95),
                               position = ggplot2::position_dodge(width = 0.8),
                               width=0) +
        ggplot2::scale_y_continuous(label=scales::comma, limits=c(0-.12*max(mri$U95), 1.02*max(mri$U95))) +
        ggplot2::scale_x_continuous(breaks=years, limits=c(min(years)-1, max(years)+1)) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::labs(title=species_title) +
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(size=9))

      if(length(unique(mri$Region))==1){
        p <-
          p +
          ggplot2::theme(legend.position = 'none')
      }else{
        p <- p +
          ggplot2::theme(
            legend.justification = c(1,0),
            legend.position = c(.98,.02),
            legend.background = ggplot2::element_blank(),
            legend.box.background = ggplot2::element_rect(colour = "grey90"),
            legend.direction = 'horizontal',
            legend.title= ggplot2::element_blank(),
            legend.margin= ggplot2::margin(t=1, b=1, r=2, l=0),
            legend.text = ggplot2::element_text(size = 5))
      }
      #p

      return(p)
    }else{
      # if no estimate is non-zero, return NULL
      return(NULL)
    }
  }

  ##############################################################################
  # Handle inputs

  if(is.null(species)){
    suppressWarnings({
      if(is.null(names(lta_result))){
        estimates <-
          lapply(lta_result,'[[','estimate') %>%
          dplyr::bind_rows()
      }else{
        estimates <- lta_result$estimate
      }
      species <-
        estimates %>%
        dplyr::group_by(title) %>%
        dplyr::filter(any(D>0)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(as.numeric(substr(species, 1, 3))) %>%
        dplyr::pull(title) %>%
        unique
    })
  }

  if(is.null(ncols)){
    (ncols <- min(length(species),3))
  }

  if(is.null(nrows)){
    (nrows <- max(1, floor(length(species)/ncols)))
    if(ncols*nrows < length(species)){nrows <- nrows + 1}
  }

  ##############################################################################
  # Produce plots dynamically using do.call
  # The plots are what are returned

  do.call(gridExtra::grid.arrange,
          c(lapply(species,
                   function(species_title){
                     lta_plot_core(species_title, lta_result, years)
                   }) %>% Filter(f=Negate(is.null)),
            nrow=nrows,
            ncol=ncols))

}
