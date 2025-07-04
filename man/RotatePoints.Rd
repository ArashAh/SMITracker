% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{RotatePoints}
\alias{RotatePoints}
\title{Apply rotation matrix}
\usage{
RotatePoints(xPos, yPos, estimate)
}
\arguments{
\item{xPos}{The X coordinate of the detected signal.}

\item{yPos}{The Y coordinate of the detected signal.}

\item{estimate}{The slope by which the rotation matrix is applied.}
}
\value{
a vector containing the X and Y coordinates of the signal after
rotation.
}
\description{
A function that applies a rotation matrix to each pair of
x and y coordinates.
}
