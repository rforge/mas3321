\name{dgt}
\alias{dgt}
\alias{pgt}
\alias{qgt}
\alias{rgt}
\title{The Generalised t distribution}
\usage{
  dgt(x, a, b, c)

  pgt(x, a, b, c)

  qgt(p, a, b, c)

  rgt(n, a, b, c)
}
\arguments{
  \item{x}{vector of quantities}

  \item{p}{vector of probabilities}

  \item{n}{number of observations}

  \item{a}{degrees of freedom. Must be strictly positive.}

  \item{b}{mean}

  \item{c}{scale. Must be strictly positive.}
}
\description{
  Density, distribution function, quantile function and
  random generation for the generalised t distribution with
  a degrees of freedom, mean b and scale c
}
\note{
  If X~t_a(b,c) then it has density
  f(x)=(1_(x-b)^2/(ac))^(-(a+1)/2)/(sqrt(ac)*Beta(a,b)).
  Also (X-b)/sqrt(c)~t_a.
}
\examples{
dgt(1, 10, 2, 2)
pgt(1, 10, 2, 2)
qgt(0.95, 10, 2, 2)
rgt(1, 10, 2, 2)
}
\keyword{character}

