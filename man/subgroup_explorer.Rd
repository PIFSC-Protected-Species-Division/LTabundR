% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgroup_explorer.R
\name{subgroup_explorer}
\alias{subgroup_explorer}
\title{Simple  explorer for subgroups data}
\usage{
subgroup_explorer(cruz, cohort = "pseudorca")
}
\arguments{
\item{cruz}{Your \code{cruz} object (produced from \code{LTabundR::process_surveys()}).}

\item{cohort}{The cohort whose data you would like to review, provided as the number or name of the slot in \code{cruz$cohorts} to be referenced.}
}
\value{
A \code{Shiny} app to explore subgroup event data for a cohort within a \code{cruz} object.
}
\description{
Simple  explorer for subgroups data
}
