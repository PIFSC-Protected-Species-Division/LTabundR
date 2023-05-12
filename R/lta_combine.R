#' Combine LTA estimates from separate regions
#'
#' Combine the results of `LTabundR::lta()` from separate regions,
#' weighting density by respective area and
#' handling the combination of CV and 95% confidence interval estimates.
#'
#' @param d1 Density estimate (animals per square km) from region 1.
#' @param n1 Abundance estimate from region 1.
#' @param cv1 CV estimate from region 1.
#' @param lci1 Lower 95% confidence interval of abundance from region 1.
#' @param uci1 Upper 95% confidence interval of abundance from region 1.
#' @param area1 Area, in square km, of region 1.
#' @param d2 Density estimate for region 2.
#' @param n2 Abundance estimate for region 2.
#' @param cv2 CV estimate for region 2.
#' @param lci2 LCI for region 2.
#' @param uci2 UCI for region 2.
#' @param area2 Area for region 2.
#'
#' @return A one-row `data.frame` with these columns:
#' \enumerate{
#' \item `D`: the combined density (animals per square km), weighted by the areas of constituent regions.
#' \item `N`: the combined abundance.
#' \item `CV`: the combined CV of density and abundance.
#' \item `LCI`: the lower 95% confidence interval of abundance.
#' \item `UCI`: the upper 95% confidence interval of abundance.
#' \item `area`: combined area.
#' }
#' @export
#'
lta_combine <- function(d1,
                        n1,
                        cv1,
                        lci1,
                        uci1,
                        area1,
                        d2,
                        n2,
                        cv2,
                        lci2,
                        uci2,
                        area2){


  if(FALSE){ # debudding only -- not run =========================================

    # 2002 MHI spotted dolphin
    d1 <- 15.07
    n1 <- 2372
    cv1 <- 1.01
    lci1 <- 456
    uci1 <- 12338
    area1 <- 157397

    # 2002 EEZ spotted dolphin
    d2 <- 6.51
    n2 <- 14559
    cv2 <- 0.74
    lci2 <- 4000
    uci2 <- 52998
    area2 <- 2235180

    # Combined should be: D = 7.08, N = 16931, CV = 0.65, LCI = 5289, UCI = 54202
    lta_combine(d1, n1, cv1, lci1, uci1, area1,
                d2, n2, cv2, lci2, uci2, area2)

    # 2002 MHI sperm whale
    d1 <- 0.79
    n1 <- 169
    cv1 <- 0.64
    lci1 <- 54
    uci1 <- 528
    area1 <- 212455

    # 2002 EEZ sperm whale
    d2 <- 2.21
    n2 <- 4945
    cv2 <- 1.00
    lci2 <- 970
    uci2 <- 25197
    area2 <- 2235180

    # Combined should be: D = 2.09, N = 5114, CV = 0.96, LCI = 1043, UCI = 25060
    lta_combine(d1, n1, cv1, lci1, uci1, area1,
                d2, n2, cv2, lci2, uci2, area2)

  } # end not run ================================================================

  # This code is based on Excel code from Amanda Bradford from Bradford et al 2021

  # Weighted mean for Density
  (D <- stats::weighted.mean(x = c(d1, d2), w = c(area1, area2)))

  # Simple sum of abundance
  (N <- n1 + n2)

  # Caclulate CV of abundance
  (CV <- round( sqrt((cv1*n1)^2 + (cv2*n2)^2) / N, 3))

  # LCI
  # in excel = N / EXP(NORMSINV(1-(1-0.95)/2)*SQRT(LN(1+(CV*CV))))
  (LCI <- round(N / exp(qnorm(1-(1-0.95)/2)*sqrt(log(1+(CV*CV))))))

  # UCI
  # in excel = N3*EXP(NORMSINV(1-(1-0.95)/2)*SQRT(LN(1+(O3*O3))))
  (UCI <- round(N * exp(qnorm(1-(1-0.95)/2)*sqrt(log(1+(CV*CV))))))

  # Calculate new area
  new_area <- area1 + area2

  # Prepare results
  (result <- data.frame(D, N, CV, LCI, UCI, area = new_area))

  return(result)
}

