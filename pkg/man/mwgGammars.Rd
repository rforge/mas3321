\name{mwgGammars}
\alias{mwgGammars}
\title{Metropolis within Gibbs algorithm for a gamma random sample}
\usage{
  mwgGammars(N, initial, innov, priorparam, n, xbar, xgbar,
    show = TRUE)
}
\arguments{
  \item{N}{length of MCMC chain}

  \item{initial}{starting value for the algorithm}

  \item{innov}{standard deviation of normal random walk
  innovation for index parameter}

  \item{priorparam}{prior parameters a,b,c,d}

  \item{n}{size of random sample}

  \item{xbar}{(arithmetic) mean of random sample}

  \item{xgbar}{geometric mean of random sample}

  \item{show}{logical. If true then acceptance rate for the
  proposals will be given}
}
\description{
  Simulates realisations from the posterior distribution
  for the index and shape parameters in a gamma
  distribution based on a random sample and independent
  gamma priors by using a Metropolis within Gibbs algorithm
  and a normal random walk proposal for the index parameter
}
\examples{
mcmcSummary(mwgGammars(100,(0.62/0.4)^2,0.8,c(2,1,3,1),50,0.62,0.46),rows=2)
}

