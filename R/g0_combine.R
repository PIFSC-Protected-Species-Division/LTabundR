#' Combine estimates of weighted g(0) and its CV
#'
#' This function is typically not called by the user, but it certainly can be.
#' It is called as a subroutine within `LTabundR::lta()`.
#'
#' @param g0s A numeric vector of weighted estimates of trackline detection probability, g(0).
#' Any length is allowed.
#'
#' @param g0cvs A numeric vector, the same length as `g0s`, of weighted estimates of the CV of g(0).
#'
#' @return A list with two slots: `$g0` holds the combined g(0) estimate, and `$CV` holds the
#' combined CV.
#'
#' @export
#'
g0_combine <- function(g0s, g0cvs){

  if(FALSE){  # For debugging ==================================================
    #minke # s/b  # fw  # bw
    g0s <- c(0.11,   .41,   .33,  .55)
    g0cvs <- c(0.98, .20,   .27,  .35)
    g0s <- g0s[1] # works for single-species and combine_g0 applications
    g0cvs <- g0cvs[1]
  } #===========================================================================

  # This code is based on the Excel equations used in Bradford et al 2022 (WHICEAS)

  # Combine g0 estimate
  (g0i <- mean(g0s))

  # Combine CV estimate
  (g0int <- g0s^2 * g0cvs^2) # intermediate step
  (g0int2 <- sum(g0int / (length(g0s)^2))) # combined variance
  (g0cvi <- round((sqrt(g0int2) / g0i), 3)) # combined CV

  return(list(g0 = g0i,
              CV = g0cvi))
}
