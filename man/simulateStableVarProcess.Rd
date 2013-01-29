\name{simulateStableVarProcess}
\alias{simulateStableVarProcess}
\title{Simulate data from stable VAR process}
\usage{
  simulateStableVarProcess(dim = 3, order = 2, sd = 1,
    length = 1000)
}
\arguments{
  \item{dim}{the dimension of the VAR (bivariate,
  trivariate,...)}

  \item{order}{the order of the VAR (how many lags)}

  \item{sd}{the standard deviation of the noise to be
  included}

  \item{length}{the length of the series to be generated}
}
\value{
  a matrix containing the simulated data
}
\description{
  This function can be used to simulate data from a random,
  stable VAR process.
}
\references{
  G.N. Boshnakov, B.M. Iqelan (2009). Generation of time
  series models with given spectral properties. Journal of
  Time Series Analysis 30(3):349-368.
}

