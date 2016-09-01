\name{elicitNGa}
\alias{elicitNGa}
\title{Elicitation of normal-gamma prior distribution by using quantiles for mu and tau}
\usage{
  elicitNGa(mup1 = 0.025, muquantile1, mup2 = 0.975,
    muquantile2, taup1 = 0.025, tauquantile1,
    taup2 = 0.975, tauquantile2)
}
\arguments{
  \item{mup1}{probability}

  \item{mup2}{probability}

  \item{taup1}{probability}

  \item{taup2}{probability}

  \item{muquantile1}{lower mup1-th quantile for mu}

  \item{muquantile2}{lower mup2-th quantile for mu}

  \item{tauquantile1}{lower taup1-th quantile for tau}

  \item{tauquantile2}{lower taup2-th quantile for tau}
}
\value{
  Vector containing the parameters of the normal-gamma
  distribution
}
\description{
  Elicitation of normal-gamma prior distribution by using
  quantiles for mu and tau
}
\examples{
b=20;c=10;g=0.15;h=6 # 5\% and 95\% quantiles are#'
muq1=qgt(0.05,2*g,b,h/(g*c));muq2=qgt(0.95,2*g,b,h/(g*c))
tauq1=qgamma(0.05,g,h);tauq2=qgamma(0.95,g,h)
elicitNGa(0.05,muq1,0.95,muq2,0.05,tauq1,0.95,tauq2) # this function determines the original values of b, c, g and h
}
\keyword{character}

