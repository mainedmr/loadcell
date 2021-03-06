% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing.R
\name{parse_peaks}
\alias{parse_peaks}
\title{Parse peaks}
\usage{
parse_peaks(data, span = 0.05, peakdist = 10, peakheight = 200)
}
\arguments{
\item{data}{Object of class `LoadCellHauls`}

\item{span}{Loess smoothing factor; defaults to .05}

\item{peakdist}{The minimum distance between peaks in index values}

\item{peakheight}{The minimum height of peaks in LBF}
}
\value{
An object of class `LoadCellPeaks`, containing all the attributes of
object class `LoadCellHauls`, plus the following attributes:
\describe{
\item{span}{The span used to apply Loess smoothing.}
\item{peakdist}{The peak distance parameter used to apply Loess smoothing.}
\item{peakheight}{The peak height parameter used to apply Loess smoothing.}
\item{peak_analysis}{Boolean indicating if peak analysis was successful; if
False, the remaining attributes will not be present.}
\item{smoothed_peaks}{A dataframe containing the Index and Load for peaks
located in the smoothed data for the top trapcount peaks.}
\item{smoothed_valleys}{A dataframe containing the Index and Load for valleys
located in the smoothed data corresponding to peaks located.}
\item{smoothed}{A dataframe containing the Index and Load for each load
after Loess smoothing was applied.}
\item{actual_peaks}{A dataframe containing the Index and Load values for
actual peaks corresponding to the smoothed peaks located within the load cell
 data.}
}
}
\description{
Searches for peaks and valleys in load data from an object of class
`LoadCellHauls`.
}
\author{
Bill DeVoe, \email{William.DeVoe@maine.gov}
}
