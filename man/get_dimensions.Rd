% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get_dimensions}
\alias{get_dimensions}
\title{Retrieve dimensions of series in the ECB's SDW}
\usage{
get_dimensions(key, ...)
}
\arguments{
\item{key}{A character string identifying the series to be retrieved}

\item{...}{Arguments passed to GET (e.g. timeout(10) to add maximum request time)}
}
\value{
A list of data frames, one for each series retrieved
}
\description{
Retrieve dimensions of series in the ECB's SDW
}
\examples{
hicp_dims <- get_dimensions("ICP.M.U2.N.000000.4.ANR")
hicp_dims[[1]]
}
