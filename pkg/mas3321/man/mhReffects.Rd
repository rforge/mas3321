\name{mhReffects}
\alias{mhReffects}
\title{Metropolis-Hastings algorithm for a one-way normal random effects model}
\usage{
  mhReffects(N, initial, intau, innu, priorparam, m, n,
    ybar, s, show = TRUE, innLogscale = FALSE)
}
\arguments{
  \item{N}{length of MCMC chain}

  \item{initial}{starting values for the algorithm}

  \item{intau}{standard deviation of normal random walk
  innovation for data precision parameter tau}

  \item{innu}{standard deviation of normal random walk
  innovation for random effects precision parameter nu}

  \item{priorparam}{prior parameters a,b,c,d,e,f}

  \item{m}{number of treatments}

  \item{n}{vector containing the number of observations on
  each treatment}

  \item{ybar}{vector containing the mean of observations on
  each treatment}

  \item{s}{vector containing the standard deviation of
  observations on each treatment}

  \item{show}{logical. If true then acceptance rate for the
  proposals will be given}

  \item{innLogscale}{logical. If true then proposals are
  made on a log scale}
}
\description{
  Simulates realisations from the posterior distribution
  for the population mean and precision components in a
  one-way normal random effects model with a semi-conjugate
  prior. The method marginalises over the random effects
  and uses univariate normal or log normal random walk
  proposals for the precision components.
}
\examples{
data(contamination)
n=tapply(contamination$acc,contamination$keyboard,length)
ybar=tapply(contamination$acc,contamination$keyboard,mean)
s=sqrt(tapply(contamination$acc,contamination$keyboard,var)*(n-1)/n)
mcmcAnalysis(mhReffects(N=100,initial=c(200,2e-5,1),intau=1e-5,innu=7.9,priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=10,n=n,ybar=ybar,s=s,show=TRUE),rows=3)
}

