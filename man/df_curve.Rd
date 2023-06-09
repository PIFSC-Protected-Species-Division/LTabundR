% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_curve.R
\name{df_curve}
\alias{df_curve}
\title{Get detection function curve}
\usage{
df_curve(models, covariates = NULL, truncation_distance)
}
\arguments{
\item{models}{A list of best-fitting detection function model(s) (produced by \code{LTabundR::fit_df()}).}

\item{covariates}{If \code{NULL}, the function will assume that this detection function does not use covariates.
If covariates \emph{are} used, this input should be anything \emph{except} \code{NULL}.}

\item{truncation_distance}{Truncation distance used, in km.}
}
\value{
A \code{data.frame} of length 101, with two columns:
\code{km} is distance from the trackline, and \code{p} is the average detection probability at that distance,
based on the data's covariates.
}
\description{
This is an internal function, typically not called by the user.
It returns a single detection function curve based on one or more detection function models.
If multiple models are provided, their curves will be averaged together to provide a single curve,
weighting the average based upon the model AIC.
}
\details{
This function is adapted from code in \code{mrds::plot.ds()} and \code{mrds::detfct()}.
}
