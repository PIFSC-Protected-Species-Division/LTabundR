% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgroup_phases.R
\name{subgroup_phases}
\alias{subgroup_phases}
\title{Review & stage edits to subgroup phase assignments}
\usage{
subgroup_phases(cruz, cohort)
}
\arguments{
\item{cruz}{Your \code{cruz} object (produced from \code{LTabundR::process_surveys()}).}

\item{cohort}{The cohort whose subgroup phases you would like to review and stage edits for, provided as either the cohort name (e.g., \code{"all"} or \code{"pseudorca"}) or a number indicating which slot in \code{cruz$cohorts} should be referenced.}
}
\value{
A \code{Shiny} app that allows you to make manually stage revisions to the phase assigned to each
subgroup (this is in reference to the protocol developed for false killer whales by NOAA PIFSC). Those
phases were assigned automatically during the \code{process_surveys()} routine; this is a way for you
to review those assignments and prepare a set of revisions. When you close the app,
those revisions are returned as a \code{list()}, which you can then save to file and/or
pass to \code{LTabundR::subgroup_edit()}, which will implement the revisions by modifying the data in your \code{cruz} object.
This function, \code{subgroup_phases()}, does not edit the \code{cruz} object in any form.
}
\description{
Review & stage edits to subgroup phase assignments
}
