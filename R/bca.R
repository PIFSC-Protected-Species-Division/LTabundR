#' BCA method function
#'
#' Replicated from the `coxed` package, which has been archived on `CRAN`.
#' Moving code here so that `LTabundR` can still use this function.
#'
#' @param theta Expectation.
#' @param conf.level Confidence level.
#'
#' @return A two-element vector with the lower and upper confidence interval calculated using the BCA method.
#' @export
#'
bca <- function(theta, conf.level = 0.95){
  low <- (1 - conf.level)/2
  high <- 1 - low
  sims <- length(theta)
  z.inv <- length(theta[theta < mean(theta)])/sims
  z <- qnorm(z.inv)
  U <- (sims - 1) * (mean(theta, na.rm = TRUE) - theta)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^{
    3/2
  }
  a <- top/under
  lower.inv <- pnorm(z + (z + qnorm(low))/(1 - a * (z + qnorm(low))))
  lower <- quantile(theta, lower.inv, names = FALSE)
  upper.inv <- pnorm(z + (z + qnorm(high))/(1 - a * (z + qnorm(high))))
  upper <- quantile(theta, upper.inv, names = FALSE)
  return(c(lower, upper))
}
