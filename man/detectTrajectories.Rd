% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{detectTrajectories}
\alias{detectTrajectories}
\title{Detect and arrange trajectories}
\usage{
detectTrajectories(filteredDataSet, dxMax, dyMax)
}
\arguments{
\item{filteredDataSet}{The output of the spatially filtered data.}

\item{dxmax}{The maximum displacement of the proteins along the substrate,
this parameter is passed to the FindTrajectory() function.}

\item{dymax}{The maximum displacement of the protein across the substrate
before it is considered dissociated. this parameter is passed to the
FindTrajectory() function.}
}
\value{
A tidy data frame of all detected trajectories along with all the
signal localization attributes and metadata in separate columns across all
data sets.
}
\description{
A parent function for finding, arranging and collecting
trajectories across all existing data sets. The data #' for each data set
is run through a series other functions sequentially and the end result is
collected in tidy data frame.
}
