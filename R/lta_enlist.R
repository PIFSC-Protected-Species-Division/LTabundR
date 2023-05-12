#' Compile a directory of LTA results into a list
#'
#' This function reads in the line-transect-analysis results (from `LTabundR::lta()`) within a directory
#' and concatenates them into a `list`.
#'
#' @param lta_path The path to a directory containing `.RData` files, each containing a object
#' resulting from `LTabundR::lta()`. Typically, each `RData` file contains the product of `lta()` for a
#' single species or species pool. See the [vignette case studies](https://emk-noaa.github.io/LTAvignette/whiceas.html#density-abundance) for examples.
#'
#' @return A `list` of LTA results.
#' @export
#'
lta_enlist <- function(lta_path){
  # To debug this function, you need the path
  # to a folder with results of lta() saved in RData object(s).
  # e.g.:
  # lta_path <- '../test_code/eric/whiceas/lta/'

  # Get list of RData files in the lta_path
  lf <- list.files(lta_path)
  # Make sure these filepaths include the lta_path
  (lf <- paste0(lta_path, lf))

  # Loop through each file name, read it in, and add to list
  results <- list()
  for(i in 1:length(lf)){
    (lfi <- lf[i])
    resulti <- readRDS(lfi)
    results[[length(results) + 1]] <- resulti
  }

  return(results)
}
