% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{TransformData}
\alias{TransformData}
\title{Transform data using metadata}
\usage{
TransformData(InputData, nameList, separator, numerName)
}
\arguments{
\item{InputData}{A data frame which is an output of the readJsonFiles()
function; this is a tabular data frame containing information on all the
localized signals.}

\item{nameList}{A vector of characters representing the name of the
variables provided in the files name.}

\item{numerName}{A vector of charterers representing variables that contain
numerical values.}

\item{seprator}{The symbol that is used to separate different parts of the
file names.}
}
\value{
A data frame containing all data and metadata
}
\description{
A function that ingrates the result of the signal localization
analysis with information on experimental conditions provided as metadata
in the files name.
}
