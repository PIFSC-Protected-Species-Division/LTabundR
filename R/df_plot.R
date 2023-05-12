#' Plot the best-fit detection function model(s)
#'
#' This function adapts `mrds::plot.ds()` to plot the detection functions fit and
#' provided by the output of `lta()` and `lta_multistock()`. Arguments allow for stylizing
#' the plot as well as displaying multiple best-fitting models on the same histogram.
#'
#' @param lta_result The result of `lta()` for a single species pool.
#' @param model_colors A vector of colors; if more than one is provided,
#' the vector must be the same length as the number of best-fitting models
#' (`lta_result$df$best_models`)
#' @param model_pch A vector of point `pch`, of either length 1 or the same as the
#' number of best-fitting models.
#' @param hist_bars Number of histogram bars to show.
#' @param hist_col Color of histogram bars.
#' @param hist_border Border color of histogram bars.
#' @param pt_show If `0`, no detection probabilities for individual detections will be shown.
#' If `1`, only detection probabilities for the first best-fitting model will be shown, as points.
#' (This is the default.)
#' If `2`, points will be displayed for *all* best-fitting models.
#' @param pt_cex The size of detection points, if shown.
#' @param pt_alpha The transparency (0 = transparent; 1=solid) of points, if shown.
#' @param line_col Color of detection function line.
#' @param line_lty The line type for the detection function, following conventional base plot `lty` values.
#' @param line_lwd Detection function line thickness.
#' @param line_alpha Detection function line transparency, as a fraction between 0 and 1.
#' @param bootstrap_show Show detection function curves for all bootstrap iterations? Default is `TRUE`.
#' @param bootstrap_col Color of bootstrap detection function curves, if shown.
#' @param bootstrap_lwd Thickness of bootstrap detection function curves, if shown.
#' @param bootstrap_alpha Transparency of bootstrap detection function curves, if shown.
#' @param main_show Show a main title? `TRUE` (default) or `FALSE`.
#' @param main The text for the main title; if `NULL`, the species pool name will be provided.
#' @param main_cex Size of main title.
#' @param legend_show Show a legend, indicating the line/point/color for each best-fitting model?
#' `TRUE` or `FALSE` (default).
#' @param ymax Maximum of y-axis (can be helpful when making space for the legend). Can be `NULL`.
#' @param legend_x Left-right position of legend, if shown.
#' @param legend_y Up-down position of legend, if shown.
#' @param legend_cex Size of legend text, if shown.
#' @param xlab Text for x axis label, if shown.
#'
#' @return A plot is printed.
#' @export
#'
df_plot <- function(lta_result,
                    model_colors = 'black',
                    model_pch = 1,
                    hist_bars = 15,
                    hist_col = "grey90",
                    hist_border = 'grey60',
                    pt_show = 1,
                    pt_cex = .8,
                    pt_alpha = .3,
                    line_col = 'darkblue',
                    line_lty = 1,
                    line_lwd = 2.5,
                    line_alpha = .7,
                    bootstrap_show = TRUE,
                    bootstrap_col = 'steelblue4',
                    bootstrap_alpha = .2,
                    bootstrap_lwd = .5,
                    main_show = TRUE,
                    main = NULL,
                    main_cex = 1.25,
                    legend_show = FALSE,
                    ymax = NULL,
                    legend_x = 3.2,
                    legend_y = 1.2,
                    legend_cex = .8,
                    xlab = "Distance (km) from trackline"){

  if(FALSE){ # debugging only -- not run! ========================================
    data('lta_result')
    model_colors = RColorBrewer::brewer.pal(n = 4, name = "Dark2")
    model_colors = 'black'
    model_pch = c(16, 15, 17, 18)
    model_pch = c(16,16,16,16)
    hist_bars = 15
    hist_col = "grey90"
    hist_border = 'grey60'
    pt_show = TRUE
    pt_cex = .8
    pt_alpha = .2
    line_col = 'black'
    line_lty = c(1,1,1,1)
    line_lwd = 2
    line_alpha = .7
    main_show = TRUE
    main = NULL
    main_cex = 1.25
    legend_show = TRUE
    ymax = 1.5
    legend_x = 3.5
    legend_y = 1.2
    legend_cex = .8
    xlab = "Distance (km) from trackline"

    # try it
    df_plot(lta_result)
  }
  # End debugging ================================================================

  # Save argument as a diff object name, for convenience
  lti <- lta_result

  # Some of this code was adapted from a mrds function, and thus is difficult to comment on (eric k)
  # MRDS-specified  values
  lower <- 0
  vname <- "distance"
  pdf <- FALSE
  which = 2
  nc <- hist_bars
  breaks = NULL
  jitter.v = rep(0, 3)
  subset = NULL
  pl.den = NULL
  pl.ang = NULL
  pages = 0
  ylab = NULL

  # Harvest data from lta_result
  km <- lti$df$curve$km
  curvest <- lti$df$curve$p
  curvebs <- lti$bootstrap$df
  if(is.null(curvebs)){bootstrap_show <- FALSE}

  # Prepare line plot parameters for this model
  ldots <- list()
  ldots$lty <- line_lty
  ldots$col <- adjustcolor(line_col, alpha.f = line_alpha)
  ldots$lwd <- line_lwd

  (legend_info <- lti$df$best_models)
  best_models <- lti$df$best_objects
  i=1

  # Loop through each model
  for(mi in 1:length(best_models)){

    model = best_models[[mi]]
    dat <- model$data

    # Prpare point plot parameters for this model
    pdots <- list()
    pdots$col <- adjustcolor(model_colors[min(c(mi, length(model_colors)))],
                             alpha.f= pt_alpha)
    pdots$pch <- model_pch[min(c(mi, length(model_pch)))]


    # MRDS code chunk ##############################################################
    # I don't understand this code in full, so cannot comment it adequately (eric k)
    # BEGIN

    # decide which plots to show
    show <- rep(FALSE, 2)
    show[2] <- TRUE

    # Density of shaded lines - default is set all to 20
    denval1 <- pl.den[1]
    # Angle of shaded lines - default is set all to -45
    angval1 <- pl.ang[1]

    # code from dpexplot:
    width <- model$meta.data$width
    left <- model$meta.data$left
    ddfobj <- model$ds$aux$ddfobj
    point <- model$ds$aux$point
    if(is.null(model$ds$aux$int.range)){
      int.range <- c(0,width)
    }else{
      int.range <- model$ds$aux$int.range
    }
    if(is.matrix(int.range)){
      max.range <- as.vector(int.range[1,])
      int.range <- int.range[2:dim(int.range)[1],]
      range.varies <- TRUE
    }else{
      max.range <- int.range
      normalize <- FALSE
      range.varies <- FALSE
    }

    selected <- rep(TRUE, nrow(ddfobj$xmat))

    if(is.matrix(int.range)){
      int.range <- int.range[selected, ]
    }

    xmat <- ddfobj$xmat[selected,]

    if(!is.null(ddfobj$scale)){
      z <- ddfobj$scale$dm[selected, , drop=FALSE]
    }else{
      z <- matrix(1, nrow=1, ncol=1)
    }

    if(length(model$fitted)==1){
      pdot <- rep(model$fitted, sum(as.numeric(selected)))
    }else{
      pdot <- model$fitted[selected]
      Nhat <- sum(1/pdot)
    }

    zdim <- dim(z)[2]
    n <- length(xmat$distance)

    if(!is.null(breaks)){
      nc <- length(breaks)-1
    }

    if(is.null(nc)){
      nc <- round(sqrt(n), 0)
    }

    # Set logical hascov=TRUE when detection function has
    #  covariates other than distance and observer
    hascov <- FALSE
    if(!ddfobj$intercept.only){
      hascov <- TRUE
    }

    # Compute a grid for distance (xgrid), and covariates zgrid for
    # plotting of detection functions.
    if(!hascov){
      xgrid <- seq(0, width, length.out=101)
      zgrid <- matrix(rep(z[1,], length(xgrid)), byrow=TRUE, ncol=sum(zdim))
    }

    # create intervals of distance (breaks) for the chosen number of classes (nc).
    if(is.null(breaks)){
      if(is.null(model$meta.data$binned)){
        binned <- FALSE
      }else{
        binned <- model$meta.data$binned
      }
      if(binned){
        breaks <- model$ds$aux$breaks
        nc <- length(breaks)-1
      }else{
        breaks <- c(max(0, (max.range[1])),
                    max.range[1]+((max.range[2]-max.range[1])/nc)*(1:nc))
        if(breaks[1]>left){
          breaks <- c(left, breaks)
          nc <- nc+1
        }
      }
    }

    #===============================================================================
    # test breaks for validity and reset as needed
    test.breaks <- function(breaks, left, width){

      # Make sure break points are in order
      if(any(breaks!=sort(breaks))){
        stop("Break points are out of order.")
      }

      # if any endpoint > width, issue warning and reset endpoints
      if(any(breaks>1.000001*width)){
        message(paste0("Specified endpoints > ", width, "; values reset."))
        breaks <- c(breaks[breaks<width], width)
      }

      # if last endpoint does not include width reset last endpoint
      if(breaks[length(breaks)]<0.999999*width){
        message(paste0("Last interval endpoint did not include ", width,
                       ". It was reset."))
        breaks <- c(breaks, width)
      }

      # if any endpoint < left, issue warning and reset endpoints
      if(any(breaks<0.99999*left)){
        message(paste0("Specified endpoints < ", left, "; values reset."))
        breaks <- c(left, breaks[breaks>left])
      }

      # if first endpoint does not include left reset last first point
      if(breaks[1]>1.00001*left){
        message(paste0("First interval endpoint did not include ", left,
                       ". It was reset"))
        breaks <- c(left, breaks)
      }
      return(breaks)
    }
    #===============================================================================

    breaks <- test.breaks(breaks, model$meta.data$left, width)
    nc <- length(breaks)-1
    lower <- min(breaks)
    upper <- max(breaks)
    dat <- dat[selected,]
    keep <- dat[ ,vname]>=lower & dat[ ,vname]<=upper

    # get the histogram object
    hist.obj <- hist(dat[ ,vname][keep], breaks=breaks, plot=FALSE)

    # Rescaling for the histogram
    if(normalize & !point){
      bindata <- function(x, r, breaks){
        return(hist(r[r>=x[1] & r<=x[2]], breaks=breaks, plot=FALSE)$counts)
      }
      sumit<-function(x,n,wt){
        return(sum(x/(wt*n)))
      }
      expected.counts <- apply(int.range, 1, bindata,
                               r=(0:1000)*width/1001, breaks=breaks)
      expected.counts <- apply(expected.counts, 1, sumit, n=1001, wt=pdot)
    }else{
      if(!point){
        expected.counts <- (breaks[2:(nc+1)]-breaks[1:nc])*(Nhat/breaks[nc+1])
      }else{
        if(!pdf){
          expected.counts <- -apply(matrix(c(breaks[2:(nc+1)]^2, breaks[1:nc]^2),
                                           ncol=2, nrow=nc),
                                    1, diff)*(Nhat/breaks[nc+1]^2)
        }else{
          expected.counts <- sum(hist.obj$counts)
        }
      }
    }

    # rescale the histogram object by the expected counts
    # but only if we don't have point/pdf plots
    if(!(pdf & point)){
      hist.obj$density <- hist.obj$counts/expected.counts
      hist.obj$density[expected.counts==0] <- 0
    }
    hist.obj$equidist <- FALSE

    ### Actual plotting starts here

    # do the paging, using devAskNewPage() means we can just call plots and
    # R will make the prompt for us
    if(pages!=1 & sum(show)!=1){
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
    }else if(sum(show)!=1){
      opar <- par(mfrow=c(1, sum(show)))
      on.exit(par(opar))
    }

    # END
    # MRDS code chunk ##############################################################

    #===============================================================================
    # This function is adapted from a non-exported function in MRDS

    histline <- function(height, breaks, lineonly=FALSE, outline=FALSE,
                         ymax=NULL, xlab="x", ylab="y", det.plot=FALSE,
                         hist_col, hist_border,
                         add=FALSE){ #}, ...){

      # make hist object
      hh <- list()
      hh$breaks <- breaks
      hh$counts <- height
      hh$density <- height#/sum(height)
      hh$mids <- breaks[-length(breaks)] + diff(breaks)/2
      hh$xname <- "hh"
      hh$equidist <- FALSE
      hh$col <- hist_col
      hh$border <- hist_border
      class(hh) <- "histogram"

      if(is.null(ymax)){ymax <- max(height)}
      ylim <- c(0, ymax)

      # make the plot
      if(det.plot){
        plot(hh, ylim=ylim, xlab=xlab, ylab=ylab, yaxp=c(0, 1, 5), main="",
             add=add, freq=FALSE, col = hist_col, border = hist_border)
      }else{
        plot(hh, ylim=ylim, xlab=xlab, ylab=ylab, main="", add=add, freq=FALSE,
             col = hist_col, border = hist_border)
      }
      # put a box around
      box()
    }

    #===============================================================================

    ## Detection function plot overlaid on histogram of observed distances
    # area under the histogram
    hist_area <- sum(hist.obj$density*diff(breaks))
    point_vals <- mrds::detfct(xmat$distance, ddfobj, select=selected, width=width)

    # set y labels, limits and tick marks (det.plot) depending on if we
    # are plotting PDF or df
    if(is.null(ylim)) ylim<-c(0, max(hist.obj$density, max(point_vals)))
    if(is.null(ylab)) ylab <- "Detection probability"
    det.plot <- TRUE

    # plot the histogram =========================================================
    if(mi==1){ # only if this is the first model in the list of best models
      histline(hist.obj$density, breaks=breaks, lineonly=FALSE,
               xlab=xlab, ylab=ylab, ymax=ymax,
               hist_col = hist_col, hist_border = hist_border,
               det.plot=det.plot)
    }

    # MRDS code chunk ##############################################################
    # BEGIN

    # when we have covariates
    if(hascov){
      finebr <- seq(0, width, length.out=101)
      xgrid <- NULL
      linevalues <- NULL
      newdat <- xmat
      for(i in 1:(length(finebr)-1)){
        x <- (finebr[i]+finebr[i+1])/2
        xgrid <- c(xgrid, x)
        newdat$distance <- rep(x, nrow(newdat))

        detfct.values <- mrds::detfct(newdat$distance, ddfobj, select=selected,
                                width=width)

        if(!normalize&range.varies){
          detfct.values[x<int.range[, 1] | x>int.range[, 2]] <- 0
        }

        linevalues <- c(linevalues, sum(detfct.values/pdot)/sum(1/pdot))
      }
      ## without covariates
    }else{
      if(!is.null(ddfobj$scale)){
        ddfobj$scale$dm <- ddfobj$scale$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }
      if(!is.null(ddfobj$shape)){
        ddfobj$shape$dm <- ddfobj$shape$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }

      # goofy workaround -- gamma is 0 at x=0, so add a little to the grid
      #  value so we don't have a weird drop
      if(ddfobj$type=="gamma" & left==0){
        xgrid[1] <- xgrid[1]+1e-6
      }

      linevalues <- mrds::detfct(xgrid, ddfobj, width=width, left=left)

    }

    # END
    # MRDS code chunk ############################################################

    # STage the line plot
    if(mi==1){
      ldots$x <- xgrid
      ldots$y <- linevalues
    }

    # plot the points ============================================================

    showpoints <- FALSE
    if(pt_show==1 & mi==1){showpoints <- TRUE}
    if(pt_show==2){showpoints <- TRUE}
    if(range.varies & showpoints){
      warning("Point values can be misleading for g(x) when the range varies")
    }
    if(showpoints){
      jitter.p <- rnorm(length(point_vals), 1, jitter.v[1])
      pdots$x <- xmat$distance
      pdots$y <- point_vals*jitter.p
      do.call(points, pdots)
    }

  } # end of loop through models

  # Plot the bootstrap lines  ==================================================

  if(bootstrap_show){
    for(i in 1:nrow(curvebs)){
      curvi <- curvebs[i,] %>% as.numeric
      curvi
      lines(curvi~km, col=adjustcolor(bootstrap_col,alpha.f=bootstrap_alpha), lwd=bootstrap_lwd)
    }
  }

  #  plot the main estimate line ===============================================

  do.call(lines, ldots)

  # Add main title =============================================================

  if(main_show){
    if(is.null(main)){
      main <- lti$pool
    }
    title(main, cex.main= main_cex)
  }

  # Add legend =================================================================

  if(legend_show){
    legend(x=legend_x,
           y=legend_y,
           legend=legend_info$Formula,
           col=adjustcolor(model_colors, alpha.f=pt_alpha),
           pch=model_pch,
           lty=line_lty,
           lwd=line_lwd,
           cex = legend_cex,
           xjust=0,
           yjust=0)
  }

}
