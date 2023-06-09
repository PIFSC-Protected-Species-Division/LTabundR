% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/das_load.R
\name{das_load}
\alias{das_load}
\title{Read in and process a \code{DAS} file}
\usage{
das_load(das_file, perform_checks = FALSE, print_glimpse = FALSE)
}
\arguments{
\item{das_file}{Filepath to WinCruz \code{DAS} file with survey data.
Filepath can be absolute or relative. It can also be a URL.}

\item{perform_checks}{Boolean, with default of \code{FALSE}; should \code{swfscDAS::das_check()} be run first, with diagnostics printed to the console?}

\item{print_glimpse}{Boolean, with default of \code{FALSE}; print a glimpe (\code{dplyr::glimpse()}) of the formatted \code{DAS} dataframe?}
}
\value{
A \code{data.frame} of the \code{DAS} file, with fields parsed into columns and with new columns for survey status,
sighting conditions, and observer positions.
}
\description{
This is an internal function typically not called by a user directly.
It is the first subroutine called within \code{process_surveys()}.
This function is essentially a wrapper for \code{swfscDAS::das_read()}, which reads in a raw
\code{DAS} file (or a URL to an \code{DAS} file in an online repository), and \code{swfscDAS::das_process()}, which formats each row into columns,
with survey status, sighting conditions, and observer positions added to each row as new columns.
}
