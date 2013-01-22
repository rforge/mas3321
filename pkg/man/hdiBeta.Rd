\name{hdiBeta}
\alias{hdiBeta}
\title{The highest density interval (HDI) for a Beta(a,b) distribution.}
\usage{
  hdiBeta(p = 0.95, a, b)
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
  The highest density interval (HDI) for a Beta(a,b)
  distribution.
}
\examples{
hdiBeta(0.9, 2, 2)
}
\keyword{character}

