\name{NGaposterior}
\alias{NGaposterior}
\title{Parameters of the NGa posterior distribution when sampling from a normal population
and using a NGa prior distribution}
\usage{
  NGaposterior(priorpara, n, xbar, s)
}
\arguments{
  \item{priorpara}{vector of prior parameters b,c,g,h}

  \item{n}{sample size}

  \item{xbar}{sample mean}

  \item{s}{sample standard deviation}
}
\description{
  Parameters of the NGa posterior distribution when
  sampling from a normal population and using a NGa prior
  distribution
}
\examples{
NGaposterior(c(0,1,1,1),10,1,1)
}

