% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_hauls}
\alias{plot_hauls}
\title{Plot Hauls}
\usage{
plot_hauls(data, fileout)
}
\arguments{
\item{data}{An object of class `LoadCellPeaks`.`}

\item{fileout}{*Optional* - Filename to save plots into. A PDF with this name
will be created in the current working directory. If the file exists, it will
be overwritten.}
}
\value{
Boolean True if successful. If a fileout is provided, a PDF will be
created containing the plots. If the fileout argument is omitted, plots will
be output to the console/plot viewer.
}
\description{
For an input object of class `LoadCellPeaks`, creates a plot for each haul
containing the load cell data, smoothed data, and peak analysis.
}
\author{
Bill DeVoe, \email{William.DeVoe@maine.gov}
}
