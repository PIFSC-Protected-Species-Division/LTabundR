% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lta_report.R
\name{lta_report}
\alias{lta_report}
\title{Build LTA tables for standard reports}
\usage{
lta_report(lta_result, cruz = NULL, verbose = TRUE)
}
\arguments{
\item{lta_result}{An object holding the result of \code{LTabundR::lta()} or \code{LTabundR::lta_enlist()}.}

\item{cruz}{The cruz object (produced from LTabundR::process_surveys())
that was passed to \code{lta()} to produce \code{lta_result}.
This is optional; if not supplied, only part of Table 1 will be able to be filled in
and \code{tableA2} will not be provided (see \code{Value} below).}

\item{verbose}{Boolean, with default \code{TRUE}, indicating whether or not updates should be printed to the Console.}
}
\value{
A list with five slots:
\enumerate{
\item \code{table1a}: Sighting counts for all species in the \code{cruz} dataset for the years in which estimates were prepared.
If the \code{cruz} argument was \code{NULL}, this slot is also \code{NULL}. If not, a count of all sightings and systematic-only sightings
(i.e., \code{EffType = "S"} and \code{Bft <= 6}) are given for each species-year-region in the \code{cruz} data.
These counts are provided separately from the \verb{$table1b} slot below, since those counts are based on the
\code{lta_result} object, and will not include sightings for species that did not have a specific LTA estimate
specified when it was made. We also include this separately so as to give the user full flexibility in how they summarize
sighting counts by region/population/stock.
\item \code{table1b}: Sighting counts used in estimates of density/abundance. Columns are prepared
for total sightings (Ntot) and systematic sightings (Nsys), but they are left blank, since it
is not clear how sightings from multiple regions in \verb{$table1a} would be concatenated for this table,
since that involves stock-level designations.
The user can fill in those gaps accordingly.
\item \code{table2}: Sample sizes and truncation distances for detection functions
modeled by using pooled species/sightings
\item \code{table3}: Line-transect parameter estimates (mean Effective Strip half-Width, ESW;
mean school size; trackline detection probability, g(0); and its CV).
\item \code{table4}: Density and abundance estimates.
\item \code{tableA1} : Appendix table with study areas for each geostratum/cohort/year.
\item \code{tableA2}: A \code{list} in which each slot is a \code{data.frame} with Effort & Beaufort allocation,
parsed by geostratum, for each year. If the \code{cruz} argument was \code{NULL}, this slot is also \code{NULL}.
}
}
\description{
This function formats the results from an LTA analysis (\code{LTabundR::lta()}) into
that expected for standard tables in NOAA stock assessment reports (Tables 1, 2, 3, and 4, as well as appendix tables).
}
