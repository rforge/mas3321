\name{dbvnorm}
\alias{dbvnorm}
\title{The Bivariate Normal distribution}
\usage{
  dbvnorm(x, y, b = c(0, 0), c = diag(1, 2))
}
\arguments{
  \item{x}{vector of quantities}

  \item{y}{vector of quantities}

  \item{b}{mean vector (2x1)}

  \item{c}{covariance matrix. Must be positive definite 2x2
  matrix.}
}
\description{
  Density of the bivariate normal distribution with mean
  vector b and covariance matrix c.
}
\note{
  Something here.
}
\examples{
dbvnorm(1, 2, c(1,2), diag(1,2))
}
\keyword{character}

