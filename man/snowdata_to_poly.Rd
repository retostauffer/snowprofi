% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snowdata_to_poly.R
\name{snowdata_to_poly}
\alias{snowdata_to_poly}
\title{Convert Snowdata to Polygonformat}
\usage{
snowdata_to_poly(x, baseheight = 0, cores = parallel::detectCores() - 1)
}
\arguments{
\item{x}{object returned by \code{\link{snowdata_read_wfj}}}

\item{baseheight}{numeric, single value. Typically 0 (ground height),
used to construct the polygon for the first layer.}

\item{cores}{number of cores to be used for parallelization.
Default is maximum minus 1.}
}
\value{
An data.frame prepared to be used with
\code{\link[ggplot2]{geom_poly}}.
}
\description{
Input is the object as returned by \code{\link{snowpack_read_wfj}}.
We will, internally, ensure that we got what we expected, a zoo object
with some specific columns needed to generate the polygons.
}
\seealso{
\link[snowprofi]{snowpack_read_wfj}
}
\author{
Reto
}
