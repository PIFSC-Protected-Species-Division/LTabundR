% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_surveys.R
\name{process_surveys}
\alias{process_surveys}
\title{Process & prepare \code{Wincruz} survey data for analysis.}
\usage{
process_surveys(
  das_file,
  settings,
  edits = NULL,
  process_sightings = TRUE,
  process_subgroups = TRUE,
  save_local = FALSE
)
}
\arguments{
\item{das_file}{One or more filenames/filepaths to DAS files with survey data,
supplied as a character vector. If multiple files are supplied, each file will be
processed separately then combined using \code{LTabundR::cruz_combine()}. URL's for
online \code{DAS} reposities are also accepted.}

\item{settings}{An object representing the output of \code{load_settings()}. See that function's documentation for details.}

\item{edits}{An optional list of staged edits for modifying the \code{DAS} data (after reading in; not the actual data files themselves)
before proceeding with data processing. These edits must take the form of the input for
the \code{LTabundR} function \code{das_editor()} (see its documentation for details).
If \code{edits} are supplied, a temporary version of the \code{DAS} data will be created
(this temporary version will then be deleted at the end of this function's procedure.)}

\item{process_sightings}{A Boolean, with default \code{TRUE}, indicating whether or not sightings should be processed in addition to effort.
When troubleshooting effort segments, it could be useful to set this to \code{FALSE} to expedite processing time.}

\item{process_subgroups}{A Boolean, with default \code{TRUE}, indicating whether or not subgroups should be found in the survey data and analyzed.}

\item{save_local}{If TRUE (not the default), the resulting \code{cruz} object
will be saved in the current working directory as a \code{.RData} file. This may be
advantageous, so that this function only needs to be run once in order to have the
data formatted and ready for analysis. The \code{.RData} filename will be the same as \code{das_file}
(or the first one provided, if multiple are input),
except with a different extension.}
}
\value{
A \code{cruz} object, which is a nested list with the following primary slots:
\enumerate{
\item \code{settings}, containing the \code{settings} list you provided as an argument.
\item \code{strata}, containing a dataframe summarizing the geostrata provided (their name and area, in square km).
\item \code{cohorts}, containing a named list for each cohort you specified within the \code{settings} argument.
}
Each cohort slot has a similar structure:
\cr \cr
\verb{cruz$cohorts$<cohort>$<details>}
\cr \cr
\itemize{
\item The name of each \verb{<cohort>} slot is drawn from the \code{id} slot within that cohort's settings (e.g., \code{settings$cohorts[[1]]$id}).
\item All cohorts have the same three slots for \verb{<details>}:
\enumerate{
\item \code{segments}, a \code{data.frame} with metadata for each effort segment (see below).
\item \code{das}, a \code{data.frame} of the survey data (see below).
\item \code{sightings}, a \code{data.frame} with details for each sighting (see below).
}
\item Some cohorts may also have a fourth \verb{<details>} slot, \code{subgroups},
if subgroups were found in the data for the species specified in the cohort.
This slot will contain a list (see below).
}
\cr
\strong{\code{segments}} data structure \cr
A \code{data.frame} with metadata for each segment; each row is a segment.
\enumerate{
\item \code{Cruise}: Cruise number
\item \code{ship}: Ship name initials
\item \code{stratum}: Stratum designation for this segment
\item \code{seg_id}: Unique segment identifier
\item \code{yday}: Numeric day of year
\item \code{dist}: Distance surveyed in this segment
\item \code{lat1}: Start latitude of segment, decimal degrees
\item \code{lon1}: Start longitude of segment, decimal degrees
\item \code{DateTime1}: Date and time for start of segment, formatted as a \code{lubridate::datetime} object
\item \code{timestamp1}: Numeric timestamp for start of segment (seconds since 00:00:00 UTC on 1 January 1970)
\item \code{lat2}: Ending latitude
\item \code{lon2}: Ending longitude
\item \code{DateTime2}: Date and time of end of segment
\item \code{timestamp2}: Numeric timestamp for end of segment
\item \code{mlat}: Latitude of middle of segment (i.e., the coordinate for the row of \code{DAS} data that is at \verb{nrow(<segment data>) / 2})
\item \code{mlon}: Longitude of middle of segment
\item \code{mDateTime}: Date and time of middle of segment
\item \code{mtimestamp}: Numeric timestamp of middle of segment
\item \code{use}: A decision as to whether or not this segment will be included in the analysis, based upon user-specified criteria in settings.
\item \code{Mode}: Effort mode (\code{P} for passing or \code{C} for closing)
\item \code{EffType}: Effort type (\code{S} for systematic, \code{N} for non-systematic -- i.e., off design-based trackline -- and \code{F} for fine-scale)
\item \code{OnEffort}: If \code{TRUE}, standard search protocols are in practice
\item \code{ESWsides}: Number of sides for which the effective strip width (ESW) will apply. When traveling nearshore, this may be only 1
\item \code{year}: Year
\item \code{month}: Numeric month
\item \code{day}: Numeric day of month
\item \code{min_line}: The line number of \code{DAS} data at the start of this segment
\item \code{max_line}: The final line number
\item \code{n_rows}: Number of rows of \code{DAS} data in this segment
\item \code{avgBft}: Weighted average Beaufort sea state during this segment
\item \code{avgSwellHght}: Weighted average Swell Height, in feet, during this segment
\item \code{avgHorizSun}: Weighted average horizontal sun angle, corresponding to a clock face, during this segment
\item \code{avgVertSun}: Weighted average vertical sun angle (12 = overhead, 1-3 = at the horizon) during this segment
\item \code{avgGlare}: Weighted average Glare status during this segment
\item \code{avgVis}: Weighted average visibility (defines as the distance, in nautical miles, at which a dolphin could be seen surfacing with the water, not the sky, as the background) during this segment
\item \code{avgCourse}: Weighted average ship heading during this segment
\item \code{avgSpdKt}: Weighted average speed, in knots, during this segment
}
\cr
\strong{\code{das}} data structure \cr
The \code{data.frame} of \code{DAS} survey data, as read and formatted by \code{swfscDAS::das_read()} and \code{swfscDAS::das_process()}
See the latter function documentation for details on columns. We have added the following columns during the preparation
of the \code{cruz} object:
\itemize{
\item \verb{stratum_<stratum name>} A Boolean indicating whether or not this row of data occurs within this geostratum polygon.
There will be a column like this for each geostratum provided in your \code{settings} object.
\item \code{year} Year
\item \code{month} Numeric month
\item \code{day} Numeric day of month
\item \code{yday} Numeric day of year
\item \code{km_int} Distance, in kilometers, between this row of data and the next. See documentation for \code{process_km()} for details.
\item \code{km_cum} Cumulative distance traveled up to this point in the \code{DAS} data.
\item \code{ship} Ship name initials
\item \code{stratum} Final stratum designation, decided based upon user-specific settings.
\item \code{seg_id} Identifier for the segment containing this row of data.
\item \code{use}: A Boolean decision as to whether or not this segment will be included in the analysis, based upon user-specified criteria in settings.
}
\cr
\strong{\code{sightings}} data structure \cr
The \code{data.frame} of processed sightings, as prepared by \code{swfscDAS::das_sight(return.format = 'complete')}.
See that function's documentation for details on columns. Note that the unique sighting identifier can be
found in column \code{SightNodaily}, not \code{SightNo}.
That function returns up to several rows for each sighting (\code{DAS} event codes \code{S}, \code{s}, \code{A}, \code{1}, \code{2}, ..., etc.).
We have processed the result further such that each row represents the school size estimate
for a single species within a single sighting. For example, single-species sightings will always have just one row.
We have also added the following columns during the preparation
of the \code{cruz} object:
\itemize{
\item See the columns added to the \code{das} slot above; those have all been propagated to this \code{sightings} table.
\item \code{species}: species code (character string) for the species represented by this row.
\item \code{best}: Best estimate of school size (calibrated and averaged across observer estimates according to \code{settings})
\item \code{low}: Low estimate of school size (ditto)
\item \code{high}: High estimate of school size (ditto)
\item \code{prob}: If \code{TRUE}, this species code is a probable identification (\code{DAS} event code \verb{?}).
\item \code{mixed}: If \code{TRUE}, this species occurred in a mixed-species school.
\item \code{ss_tot}: Total school size (calibrated and averaged across observer estimates according to \code{settings})
\item \code{ss_percent}: Percent of the school comprised by this species (averaged across observer estimates).
\item \code{n_sp}: Number of species in this sighting.
\item \code{n_obs}: Number of observers who contributed a school size estimate for this species.
\item \code{n_best}: Number of valid best estimates of school size.
\item \code{n_low}: Number of valid low estimates.
\item \code{n_high}: Number of valid high estimates.
\item \code{calibr}: Boolean indicating whether school size calibration was possible, if attempted.
\item \code{ss_valid}: Boolean indicating whether or not the school size estimate
for this sighting is valid and appropriate for use in abundance estimation and/or detection function fitting
with a school-size covariate.
\item \code{included}: Boolean indicating whether the sightings should be included
in the analysis based on the specified settings. Any sighting with \code{use == FALSE}
will also have \code{included == FALSE}, but it \emph{is} possible for sightings
to have \code{use == TRUE} with \code{included == FALSE}. For example, if the setting
\code{abeam_sightings} is set to \code{FALSE}, a sighting with a bearing angle beyond
the ship's beam can be excluded from the analysis  (\code{included == FALSE})
even though the effort segment it occurs within will still be used (\code{use == TRUE}).
}
\cr
\strong{\code{subgroups}} data structure \cr
If subgroup events (\code{DAS} event code \code{G}) are found pertaining to the species in your cohort,
this slot will have a list with three slots:
\enumerate{
\item \strong{\code{sightings}:} A \code{data.frame} in which each row is a school size estimate
for a single phase for a single sighting, with all subgroup school sizes summed together.
Columns:
\enumerate{
\item \code{Cruise}
\item \code{Date}
\item \code{SightNo}
\item \code{Phase}
\item \code{DateTime} Mean date and time of estimates of this sighting
\item \code{Lat} (same -- mean)
\item \code{Lon} (same -- mean)
\item \code{Species}
\item \code{Angle} (same -- mean)
\item \code{RadDist} (same -- mean)
\item \code{seg_id} Identifier for the segment containing this row of data.
\item \code{PerpDist} (same -- mean)
\item \code{GSBest} Sum of arithmetic means of best estimates of subgroups
\item \code{GSBest_geom} Sum of geometric means
\item \code{EffType} Type of effort (systematic, nonsystematic, or fine-scale)
\item \code{OnEffort} Whether or not standard search protocols are in use (\code{TRUE} or \code{FALSE}).
\item \code{use} Whether the segment on which this sighting occurred meets analysis inclusion criteria.
\item \verb{stratum_[stratum name]} A set of columns, one for each geostratum in the \code{cruz} object settings file,
indicating whether or not this location occurs within each geostratum.
\item \code{stratum} The geostratum to which this location was assigned, based upon cohort settings.
}
\item \strong{\code{subgroups}:} A \code{data.frame} in which each row is a single phase
for a single subgroup, with all school size estimates averaged together (both arithmetically and geometrically). Columns:
\enumerate{
\item \code{Cruise}
\item \code{Date}
\item \code{DateTime} First date and time of estimates of this subgroup
\item \code{Lat} (same -- first data point for estimates of this subgroup)
\item \code{Lon} (same)
\item \code{OnEffort} (same)
\item \code{EffType} (same)
\item \code{SightNo} (same)
\item \code{Species} (same)
\item \code{SubGrp} (same)
\item \code{Angle} Mean angle to this subgroup
\item \code{RadDist} Mean radial distance to this subgroup
\item \code{seg_id} Identifier for the segment containing this row of data.
\item \code{PerpDist} Mean perpendicular distance to this subgroup
\item \code{GSBest} Arithmetic mean of best estimates of this subgroup. If no best estimates are given, the \code{GSL} value will be used here.
\item \code{GSH} Arithmetic mean of high estimates of this subgroup
\item \code{GSL} Arithmetic mean of low estimates of this subgroup
\item \code{GSBest_geom} Geometric mean of best
\item \code{GSH_geom} Geometric mean of high
\item \code{GSL_geom} Geometric mean of low
\item \code{Phase} Phase (1 or 2)
\item \code{use} Whether the segment on which this sighting occurred meets analysis inclusion criteria.
\item \verb{stratum_[stratum name]} A set of columns, one for each geostratum in the \code{cruz} object settings file,
indicating whether or not this location occurs within each geostratum.
\item \code{stratum} The geostratum to which this location was assigned, based upon cohort settings.
}
\item \strong{\code{events}:} A \code{data.frame} in which each row is single subgroup size estimate from a single observer for a single phase (Phase 1 -- on effort / passing mode -- or Phase 2 -- off effort / closing mode).
This is effectively the "raw" subgroup data. Columns:
\enumerate{
\item \code{Cruise} Cruise number
\item \code{Date} Date, in format YYYY-MM-DD
\item \code{DateTime} Date and time, in format YYYY-MM-DD HH:MM:SS
\item \code{Lat} Latitude, in decimal degrees
\item \code{Lon} Longitude, in decimal degrees
\item \code{OnEffort} If \code{TRUE}, standard search protocols are in use. If \code{FALSE}, non-standard protocols (or no searching at all).
\item \code{EffType} Effort type (\code{S} for systematic, \code{N} for non-systematic, and \code{F} for fine-scale)
\item \code{SightNo} Sighting number of the day (the count resets every day)
\item \code{Species} Species code (character string)
\item \code{Line} Line number in \code{DAS} data
\item \code{SubGrp} Subgroup identifier code
\item \code{Event} Count of estimates for this subgroup-phase
\item \code{Obs} Observer code
\item \code{GSBest} Best estimate of subgroup size
\item \code{GSH} High estimate of subgroup size
\item \code{GSL} Low estimate of subgroup size
\item \code{Angle} Angle between bow and subgroup
\item \code{RadDist} Radial distance to subgroup, in km
\item \code{use} Whether the segment on which this sighting occurred meets analysis inclusion criteria.
\item \verb{stratum_[stratum name]} A set of columns, one for each geostratum in the \code{cruz} object settings file,
indicating whether or not this location occurs within each geostratum.
\item \code{stratum} The geostratum to which this location was assigned, based upon cohort settings.
\item \code{PerpDist} Perpendicular distance to subgroup from the trackline represented by the ship heading, in km
\item \code{sgid} Unique subgroup identifier
\item \code{sitid} Unique sighting identifier
\item \code{phase} Phase in protocol; all \code{OnEffort == TRUE} estimates are Phase 1; all \code{OnEffort == FALSE} estimates are Phase 2.
}
}
\cr\cr
This \code{cruz} object can be carried forward into data exploration (e.g., see the \code{LTabundR} function \code{cruz_explorer()}),
analyses (e.g., see the \code{LTabundR} function \code{lta()}),
or passed to mapping functions (see the \code{LTabundR} functions that begin with \code{map_...})
or summary functions (see the \code{LTabundR} functions that begin with \code{summarize_...}).
}
\description{
This function is the main command you will use to begin working with your survey data in \code{R}.
It takes \code{Wincruz} survey data -- \code{.DAS} file(s) -- and user-specified settings
to prepare the data for density estimation and/or habitat modeling analyses.
\cr \cr
This function was designed to be similar to \code{ABUND9}, the Fortran program
written by Jay Barlow (NOAA SWFSC) for the same purpose.
}
\details{
This function is a wrapper for several subroutines, which are executed in the following order:
\enumerate{
\item Read and format the survey data contained in your \code{DAS} file using functions developed in the package \code{swfscDAS}
(this step is carried out using the internal \code{LTabundR} function \code{load_das()}).
\item Interpolate the \code{DAS} data, if instructed by settings. See \code{load_survey_settings()} for details.
\item Determine which \code{DAS} events occur within the geo-strata
provided by the user (using the internal \code{LTabundR} function \code{process_strata()}).
\item Remove invalid entries, determine the ship used in each cruise,
calculate the distance transited between each \code{DAS} row, and initiate the
data structure for the eventual \code{cruz} object output.
(using the internal \code{LTabundR} function \code{format_das()}).
\item Parse survey effort into "segments", which are used in variance estimation,
and determine which segments should be included in the analysis based upon user-specified settings
(using the internal \code{LTabundR} function \code{segmentize()}).
\item Process sightings (optional) by determining which should be included in the analysis based upon user-specified settings,
and refine school size estimates by calibrating observer estimates and averaging estimates from multiple observers.
(using the internal \code{LTabundR} function \code{process_sightings()}).
\item Process subgroup size estimates (optional) for false killer whales
(using the internal \code{LTabundR} function \code{process_subgroups()}).
\item The above process is repeated for each \code{DAS} file; if there are multiple
\code{DAS} files, the results are combined using \code{LTabundR::cruz_combine()}.
\item Optionally save the result as an \code{RData} object to easily pass the processed data to other \code{R} scripts.
}
}
