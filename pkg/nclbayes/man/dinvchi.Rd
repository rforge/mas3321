% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distns.R
\name{dinvchi}
\alias{dinvchi}
\alias{pinvchi}
\alias{qinvchi}
\alias{rinvchi}
\title{The Inverse Chi distribution}
\usage{
dinvchi(x, a, b)

pinvchi(x, a, b)

qinvchi(p, a, b)

rinvchi(n, a, b)
}
\arguments{
\item{x}{vector of quantities.}

\item{a}{first parameter. Must be strictly positive.}

\item{b}{second parameter. Must be strictly positive.}

\item{p}{vector of probabilities.}

\item{n}{number of observations.}
}
\description{
Density, distribution function, quantile function and random generation for the inverse
chi distribution with parameters a and b.
}
\note{
If X~Inv-Chi(a,b) then it has density f(x)=2b^ax^(-2*a-1)e^(-b/x^2)/Gamma(a). Also 1/X^2~Gamma(a,b).
}
\examples{
dinvchi(1, 2, 2)
pinvchi(1, 2, 2)
qinvchi(0.95, 2, 2)
rinvchi(1, 2, 2)
}
\keyword{character}
