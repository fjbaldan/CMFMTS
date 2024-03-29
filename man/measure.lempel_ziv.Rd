% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lempel_ziv.R
\name{measure.lempel_ziv}
\alias{measure.lempel_ziv}
\title{Lempel-Ziv Complexity}
\usage{
measure.lempel_ziv(time_serie)
}
\arguments{
\item{time_serie}{A time serie, object of type ts.}
}
\value{
The value of the complexity measure: Lempel-Ziv.
}
\description{
Based on https://github.com/benjaminfrot/LZ76/blob/master/C/LZ76.c
}
