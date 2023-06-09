% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cruz_combine.R
\name{cruz_combine}
\alias{cruz_combine}
\title{Combine several \code{cruz} objects}
\usage{
cruz_combine(cruzes)
}
\arguments{
\item{cruzes}{A \code{list} of \code{cruz} objects, e.g., \code{list(cruz1, cruz2, cruz3)}}
}
\value{
A single \code{cruz} object.
}
\description{
This function combines processed \code{cruz} objects
(the outputs of \code{LTabundR::process_surveys()}),
under the assumption that the survey settings in both objects are the exact same.
If a cohort name occurs in multiple \code{cruz} object, the contents of the instances of the cohort
are checked for redundancy, and only non-redundant content is combined. If different
cohorts occur in the supplied \code{cruz} objects,
}
