\name{dnormgamma}
\alias{dnormgamma}
\alias{NGacontour}
\alias{rnormgamma}
\title{The Normal-Gamma distribution}
\usage{
  dnormgamma(mu, tau, b, c, g, h)

  rnormgamma(n, b, c, g, h)

  NGacontour(mu, tau, b, c, g, h, p = NULL, ...)
}
\arguments{
  \item{mu}{vector of quantities}

  \item{tau}{vector of quantities}

  \item{b}{mean}

  \item{c}{must be strictly positive.}

  \item{g}{must be strictly positive.}

  \item{h}{must be strictly positive.}

  \item{n}{sample size}

  \item{p}{probability}

  \item{...}{Arguments to be passed to the plot function
  when plotting contours}
}
\description{
  Density and random generation for the normal-gamma
  distribution with parameters b, c, g and h. Also contours
  and confidence regions.
}
\note{
  If (mu,tau)^T~NGa(b,c,g,h) then mu|tau~N(b,1/(c*tau)) and
  tau~Ga(g,h). Also mu~t_(2g)(b,h/(gc)).
}
\examples{
dnormgamma(1, 2, 1, 2, 5, 3)
rnormgamma(1, 1, 2, 5, 3)
mu=seq(2,4,len=1000)
tau=seq(0,30,len=1000)
NGacontour(mu,tau,3,1,4,0.35,0.95)
NGacontour(mu,tau,2.5,30,20,2,0.95,add=TRUE,lty=3)
}
\keyword{character}

