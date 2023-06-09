% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_sightings.R
\name{summarize_sightings}
\alias{summarize_sightings}
\title{Inventory sightings and species counts from a \code{Wincruz} survey}
\usage{
summarize_sightings(cruz, cohort = 1)
}
\arguments{
\item{cruz}{Your \code{cruz} object (produced from \code{LTabundR::process_surveys()}).}

\item{cohort}{The cohort whose data you would like to map, provided as a number indicating which slot in \code{cruz$cohorts} should be referenced.}
}
\value{
A list with various summary tables. In each table, each row is a count for a single species code.
\enumerate{
\item \code{simple_totals}: includes all sightings, even if they will not be included in analysis.
Seven columns:
\code{code} (species code), \code{short_name}, \code{scientific_name}, \code{common_name}, \code{n} (number of sightings), \code{ss_mean} (mean school size), \code{ss_sd} (standard devication of school size).
\item \code{analysis_totals}: only includes sightings that meet all inclusion criteria for the analysis.
Same columns as \code{simple_totals}.
\item \code{stratum_simple_totals}: inclues all sightings, even if they will not be included in analysis,
parsed by each geostratum. Same columns as \code{simple_totals}, with the addition of \code{year}, \code{Cruise}, and \code{stratum}.
\item \code{stratum_analysis_totals}: only includes sightings that meet all inclusion criteria for the analysis,
parsed by each geostratum. Same columns as \code{simple_totals}, with the addition of \code{year}, \code{Cruise}, and \code{stratum}.
}
}
\description{
Inventory and summarize sightings within a \code{LTabundR} \code{cruz} object in various ways.
}
