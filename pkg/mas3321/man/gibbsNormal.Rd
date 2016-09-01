\name{gibbsNormal}
\alias{gibbsNormal}
\title{Gibbs sampler for a normal random sample with a semi-conjugate prior}
\usage{
  gibbsNormal(N, initial, priorparam, n, xbar, s)
}
\arguments{
  \item{N}{length of MCMC chain}

  \item{initial}{starting value for the algorithm}

  \item{priorparam}{prior parameters b,c,g,h}

  \item{n}{size of random sample}

  \item{xbar}{mean of random sample}

  \item{s}{standard deviation of random sample}
}
\description{
  Simulates realisations from the posterior distribution
  for the mean and precision in a normal distribution based
  on a random sample and a semi-conjugate prior by using a
  Gibbs sampler
}
\examples{
mcmcAnalysis(gibbsNormal(N=100,initial=c(10,0.25),priorparam=c(10,1/100,3,12),n=100,xbar=15,s=4.5),rows=2)
}

