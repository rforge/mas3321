\name{mcmcProcess}
\alias{mcmcProcess}
\title{Function to remove burn-in and thin MCMC output}
\usage{
  mcmcProcess(input, burnin = 1000, thin = 1)
}
\arguments{
  \item{input}{matrix containing MCMC realisations}

  \item{burnin}{number of initial iterations to delete}

  \item{thin}{level of thinning}
}
\description{
  Function chops off burnin iterations and then thins the
  output by taking every thin iterates
}

