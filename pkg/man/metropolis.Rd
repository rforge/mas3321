\name{metropolis}
\alias{metropolis}
\title{Metropolis algorithm to simulate realisations from a standard normal distribution}
\usage{
  metropolis(N, initial, a, show = TRUE)
}
\arguments{
  \item{N}{length of MCMC chain}

  \item{initial}{starting value for the algorithm}

  \item{a}{size of uniform innovations}

  \item{show}{logical. If true then acceptance rate for the
  proposals will be given}
}
\description{
  Metropolis algorithm to simulate realisations from a
  standard normal distribution using a U(-a,a) random walk
  proposal
}
\examples{
mcmcSummary(metropolis(100,0,1),rows=1)
}

