\name{hdiGamma}
\alias{hdiGamma}
\title{The highest density interval (HDI) for a Gamma(a,b) distribution}
\usage{
  hdiGamma(p = 0.95, a, b)
}
\arguments{
  \item{p}{Confidence level for the HDI}

  \item{a}{is a positive number}

  \item{b}{is a positive number}
}
\value{
  Vector containing the limits of the HDI
}
\description{
  The highest density interval (HDI) for a Gamma(a,b)
  distribution
}
\examples{
hdiGamma(0.9, 2, 2)
}
\author{
  Richard Boys
}
\keyword{character}

