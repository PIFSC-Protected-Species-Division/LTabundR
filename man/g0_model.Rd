% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g0_model.R
\name{g0_model}
\alias{g0_model}
\title{Estimate relative g(0) in different survey conditions}
\usage{
g0_model(
  spp,
  truncation_distance = 5.5,
  cruz,
  cohort = 1,
  eff_types = "S",
  jackknife_fraction = 0.1,
  constrain_shape = FALSE,
  k = 4,
  toplot = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{spp}{A character vector of species code(s) whose relative trackline detection probability (g(0))
you want to estimate.}

\item{truncation_distance}{The truncation distance, in km, to apply to sightings.}

\item{cruz}{Your \code{cruz} object (produced from \code{LTabundR::process_surveys()}).
Ensure that segment lengths are short; Barlow (2015) used 10km segments.
See using the built-in \code{LTabundR} dataset, \code{data(noaa_10km_1986_2020)}, if it applies to your study.}

\item{cohort}{The cohort whose data pertains to the species of interest, provided as a number indicating which slot in \code{cruz$cohorts} should be referenced.}

\item{eff_types}{Effort types to filter segments and sightings to before conducting analysis. The
default is systematic effort only (\code{"S"}). Can be \code{NULL}.}

\item{jackknife_fraction}{The proportion of data to leave out within each jackknife permutation,
which is used to calculate the CV of the \emph{Rg(0)} estimates.
The default is 0.1 (i.e., 10\% of the data, yielding 10 jackknife loops), after Barlow (2015).}

\item{constrain_shape}{Some \emph{Rg(0)} curves will not decline monotonically due to sample size
issues at low Bft (0-2) or high Bft (5-6) states. To coerce monotonic decline, set this to
\code{TRUE}, and the function will use a shape-constrained GAM (\code{scam()} from package \code{scam}) instead of a
classic \code{mgcv::gam()}.}

\item{k}{Smoothing term for the Bft spline in the GAM. Default (and the value used in Barlow 2015) is 4.}

\item{toplot}{Boolean, with default \code{TRUE}, indicating whether segment length histograms and detection function plots (\code{Distance::plot.ds()})
should be displayed (during estimation of effective strip width).}

\item{verbose}{Boolean, with default \code{TRUE}, indicating whether or not updates should be printed to the Console.}
}
\value{
A list:
\itemize{
\item \code{Rg0} a \code{data.frame} of estimates of Relative g(0) at each Beaufort.
\item \code{gam} the \code{mgcv::gam()} model output.
\item \code{jackknife} A list of jackknife results: \verb{$g0} contains the g(0) estimates from
each jacknife iteration; \verb{$ESW} contains the effective half-strip-width estimates; \verb{$gams} contains the gam model objects.
\item \code{summary}: A dataframe summarizing results; \strong{this is the primary output you are likely to use.}.
\item \code{cruz10}: A modified nascent \code{cruz} object that has been re-segmentized to have segments of 10km-length.
This usually takes a while to create, so this output gives you the option of passing this object
on to your next call of \code{g0_bft_model()} (see \code{cruz10} argument above) to save time.
}
}
\description{
This function is an implementation of Barlow (2015), "Inferring trackline detection
probabilities, g(0), for cetaceans from apparent densities in different survey
conditions" (\emph{Marine Mammal Science}), for processed \code{Wincruz} survey data.
This function predicts the relative g(0) (compared to Beaufort sea state 0)
for all Beaufort sea states 0 - 6.
}
\details{
After Barlow (2015), this function implements the following procedure:
\enumerate{
\item Filter sightings to the specified species code(s),
and associate those sightings with their respective 10-km segment ID.
\item Determine the presence or absence of the species of interest for each segment.
\item Estimate the Effective Search Area (ESA) for each Beaufort sea state represented in the data.
This is done by fitting a detection function to the sightings data (half-normal key; using function \code{Distance::ds()}),
using Beaufort as the only covariate. With this detection function model,
the probability of detection (\code{p(det)}) for sightings in each Beaufort state is determined (\code{Distance::predict.ds()$fitted}).
The effective strip width (ESW) for each Beaufort state is determined by multiplying \code{p(det)} by the supplied \code{truncation_distance}.
The ESW is used to calculate the ESA in each segment (ESA = 2 x L x ESW).
Note that Beaufort states 0 and 1 are combined (after Barlow 2015),
since they are typically under-represented in open-ocean fieldwork.
\item A binomial Generalized Additive Model (GAM) (\code{"logit"} link function)
is fit to the segments (using \code{mgcv::gam}), based upon Equation 3 from Barlow (2015),
which includes Beaufort sea state, Latitude x Longitude, and Year as predictors
(using the \code{mgcv::s()} spline function) and the log of ESA as an offset (\code{stats::offset()})
to predict the probability of apparent presence in each segment.
To prevent over-fitting, the spline functions for Beaufort and Year are constrained with \code{k=4},
and model complexity penalty is inflated (\code{gamma = 1.4} in \code{mgcv::gam()}).
\item The GAM model is used to predict the probability of apparent species presence (\code{mgcv::predict.gam()}) in
fake segements of equal length, year, and Latitude/Longitude but with different Beaufort sea states, 0 - 6.
\item By comparing the probability for Beaufort 0 (in which rates of apparent presence would be highest) to
that for other Beaufort states, the Relative g(0) (\emph{Rg(0)})is estimated (Equation 4 in Barlow 2015).
\item The Coefficient of Variation (CV) for \emph{Rg(0)} is then estimated using a jackknife procedure,
in which a fraction of the data are sequentially removed (input \code{jackknife_fraction}, with default 10\%),
\emph{Rg(0)} is re-estimated by implementing steps 4 - 6 above, and this process is repeated for all sequential fractions of the data.
The CV is derived from these pseudo-estimates (let's call them \code{jacks}) with the equation \verb{n*CV(jacks) - (n-1)*CV(jacks))},
in which \code{n} is the number of jackknife estimates (\code{1 / jackknife_fraction}) and
\code{CV} is the standard deviation of jacks divided by their mean.
}
}
