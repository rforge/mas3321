% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdi.R
\name{hdiGamma}
\alias{hdiGamma}
\title{The highest density interval (HDI) for a Gamma(a,b) distribution}
\usage{
hdiGamma(p = 0.95, a, b)
}
\arguments{
\item{p}{confidence level for the HDI.}

\item{a}{a positive number.}

\item{b}{a positive number.}
}
\value{
Vector containing the limits of the HDI.
}
\description{
The highest density interval (HDI) for a Gamma(a,b) distribution
}
\examples{
hdiGamma(0.9, 2, 2)
}
\keyword{character}
