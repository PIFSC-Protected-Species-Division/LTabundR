% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strata_area.R
\name{strata_area}
\alias{strata_area}
\title{Combine geostrata and compute area}
\usage{
strata_area(
  strata_all,
  strata_keep,
  strata_remove = NULL,
  remove_land = TRUE,
  toplot = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{strata_all}{A named \code{list} in which each slot is a \code{data.frame} of
coordinates for a geostratum polygon. Each \code{data.frame} must have \code{Lat} and \code{Lon}
as the first two columns, providing coordinates in decimal degrees in which
South and West coordinates are negative. Other columns are allowed,
but the first two need to be \code{Lon} and \code{Lat}. The name of the slot
holding the \code{data.frame} will be used as a reference name for the stratum.
For an example of formatting, see \code{data(strata_cnp)}.
If you are working with a standard NOAA survey region, such as the Central North Pacific (CNP),
Eastern Tropical Pacific (ETP), or California Current System (CCS),
you can use built-in polygons available in \code{data(strata_cnp)}, \code{data(strata_etp)}, or
\code{data(strata_ccs)}, respectively. To explore and/or select strata contained
within those built-in datasets, use the functions \code{strata_explore()} and \code{strata_select()}.}

\item{strata_keep}{A character vector of the names of the geostrata
within \code{strata_all} that you want to use}

\item{strata_remove}{A character vector of the names of the
geostrata you want to remove. For example,
perhaps you want to keep the \code{HI_EEZ} stratum but remove insular stock boundaries.}

\item{remove_land}{A Boolean, with default \code{TRUE}, indicating whether or not you want to
remove land from the resulting survey area.}

\item{toplot}{If \code{TRUE} (the default), diagnostic maps will be plotted for your review.}

\item{verbose}{If \code{TRUE} (the default), status updates will be printed to the Console.}
}
\value{
A list with the following slots:
\enumerate{
\item \code{km2} The area of the resulting survey area polygon, in square km.
\item \code{km2_keep} The area of the \code{strata_keep} polygons, after combining them to account
for any overlapping portions of the polygons, but before removing land.
\item \code{km2_remove} The area of the \code{strata_remove} polygons, if any were provided,
after combining them to account for any overlapping portions of the polygons,
but before removing land.
\item \code{km2_with_land} The area of the resulting survey area polygon \emph{before} removing land.
\item \code{sf} The \code{sf} object of the resulting polygon.
}
Note that if \code{toplot} is \code{TRUE} (the default), a diagnostic plot will also be produced.
}
\description{
This function allows for complex combinations / subtractions of geostratum polygons
and calculates the resulting area before and after the removal of dry land.
Diagnostic plots are provided that ensure the result is correct.
}
