\name{gibbsNormal2}
\alias{gibbsNormal2}
\title{Gibbs sampler for a normal random sample with a conjugate prior}
\usage{
  gibbsNormal2(N, initial, priorparam, n, xbar, s)
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
  on a random sample and a conjugate normal-gamma prior
  distribution by using a Gibbs sampler
}
\examples{
mcmcAnalysis(gibbsNormal2(N=100,initial=c(5.41,25),priorparam=c(5.41,0.25,2.5,0.1),n=23,xbar=5.4848,s=0.1882),rows=2)
}

