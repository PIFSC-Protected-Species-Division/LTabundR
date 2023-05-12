#' Compute confidence intervals of a bootstrapped LTA estimate
#'
#' @param estimate The actual estimate
#' @param bootstraps The bootstrapped values
#' @param ci The percent confidence interval sought (as a decimal).
#'
#' @return A `list` with three versions of the 95% confidence interval,
#' in each case returned as a two-element vector: `$quantile` is the simple quantile-based
#' CI of the bootstraps; `$bca` is the bias-correction with acceleration; and `$bca_lognormal`
#' is the log-normal version of the `bca` CI. Adapted from Karin Forney's Excel magic.
#'
#' @export
#'
lta_ci <- function(estimate,
                   bootstraps,
                   ci = 0.95){

  if(FALSE){ # debugging values
    b18 <- 21360 # actual estimate
    b20 <- 1000 # number of bootstraps
    ci = 0.95
  }

  # Adapting this code from Karin Forney's Excel doc -- hence the confusing objects
  # (they correspond to Excel row/columns)

  boots <- bootstraps
  b18 <- estimate
  b20 <- length(bootstraps)
  b21 <- sum(boots) # sum of bootstraps
  b19 <- b21/b20 # avg of bootstraps
  b22 <- mean(boots) # mean of bootstraps
  b24 <- ci # % confidence interval in decimals
  b23 <- 0.5*(1 - b24) # alpha 1-tailed # should be 0.025
  (boots_c <- sapply(boots, function(x){ifelse(x < b18, 1, 0)})) # replicate column C
  (boots_d <- sapply(boots, function(x){(b22 - x)^3})) # replicate column D
  (boots_e <- sapply(boots, function(x){(b22-x)^2}))
  (b25 <- qnorm(sum(boots_c)/length(boots))) # z0 # should be 0.095
  (b26 <- sum(boots_d)/(6*sum(boots_e)^1.5)) # acc # should be -0.006

  # These represent the percentiles actually used for BCA
  # confidence limits to correct for bias and acceleration.
  (b28 <- pnorm(b25+(b25+qnorm(b23))/(1-(b26*(b25+qnorm(b23)))))) # alpha1 # should be 0.037
  (b29 <- pnorm(b25+(b25+qnorm(1-b23))/(1-(b26*(b25+qnorm(1-b23)))))) # alpha 2 # should be 0.983

  # Percentile method
  (lci_perc <- quantile(boots, b23) %>% as.numeric %>% round) # lower ci # should be 0
  (uci_perc <- quantile(boots,1-b23) %>% as.numeric %>% round)  # upper ci # should be 73721

  # BCA method
  (lci_bca <- quantile(boots,b28) %>% as.numeric %>% round)  # lower limit # should be 0
  (uci_bca <- quantile(boots,b29) %>% as.numeric %>% round)  # upper limit # should be 78333

  # Log-normal BCA
  (e39 <- sd(boots)/mean(boots))   # CV of bootstrap estimates # should be 0.871
  (lci_bcaln <- b18/exp(qnorm(1-(1-b24)/2)*sqrt(log(1+(e39*e39)))) %>% as.numeric %>% round)  # lower ci # should be 4896
  (uci_bcaln <- b18*exp(qnorm(1-(1-b24)/2)*sqrt(log(1+(e39*e39)))) %>% as.numeric %>% round)  # upper ci # should be 93191

  list(quantile = c(lci_perc, uci_perc),
       bca = c(lci_bca, uci_bca),
       bca_lognormal = c(lci_bcaln, uci_bcaln))
}
