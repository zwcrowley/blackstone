% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorHelpers.R
\name{labelColorMaker}
\alias{labelColorMaker}
\title{Helper function that creates text label colors.}
\usage{
labelColorMaker(colors, names = NULL)
}
\arguments{
\item{colors}{Required, a character vector of hex color codes, usually for the color palette of a chart.}

\item{names}{Optional, a character vector the same length of \code{colors} argument to add names to the returned vector.}
}
\value{
a character vector of colors either "black" or "white" for labeling text in fill colors.
}
\description{
A function to label the text color in charts inside the fill of bar charts, returns either "black" or "white" depending on the luminance of the color scale passed to it.
}
