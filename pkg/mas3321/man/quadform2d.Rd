\name{quadform2d}
\alias{quadform2d}
\title{Function calculates a quadratic form for 2-d vectors}
\usage{
  quadform2d(beta1, beta2, b, c)
}
\arguments{
  \item{beta1}{dimension 1}

  \item{beta2}{dimension 2}

  \item{b}{2-d vector containing (location) mean}

  \item{c}{2x2 centering matrix}
}
\description{
  Function calculates a quadratic form for 2-d vectors
}
\examples{
quadform2d(1,1,c(0,0),diag(1,2))
}

