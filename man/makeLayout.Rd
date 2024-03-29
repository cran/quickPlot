% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting-helpers.R
\name{.makeLayout}
\alias{.makeLayout}
\title{Make an optimal layout of plots}
\usage{
.makeLayout(arr, visualSqueeze, legend = TRUE, axes = TRUE, title = TRUE)
}
\arguments{
\item{arr}{an object of class \code{.arrangement}.}

\item{visualSqueeze}{Numeric. The proportion of the white space to be used
for plots. Default is 0.75.}

\item{legend}{Logical indicating whether legend should be included as part of
layout calculation. Default is \code{TRUE}.}

\item{axes}{Logical indicating whether the axes should be included as part of
layout calculation. Default is \code{TRUE}.}

\item{title}{Logical indicating whether the names of each plot should be
written above plots and should be included as part of layout
calculation. Default is \code{TRUE}.}
}
\description{
Internal function. Using the size of the current device, and number and
dimension ratios of the plots, place them optimally in the plotting region.
}
\author{
Eliot McIntire
}
\keyword{internal}
