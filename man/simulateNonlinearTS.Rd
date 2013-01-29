\name{simulateNonlinearTS}
\alias{simulateNonlinearTS}
\title{Generate nonlinear time series}
\usage{
  simulateNonlinearTS(length, lags = 4, seed = 1,
    maxRoot = 5)
}
\arguments{
  \item{length}{the length of the series to be generated}

  \item{lags}{the number of lags}

  \item{seed}{a seed used for random number generation}

  \item{maxRoot}{the roots of the characteristic
  polynomials are chosen between 1.1 and \code{maxRoot}}
}
\value{
  a list containing all the parameters, and a member
  \code{ts} with the generated series
}
\description{
  This function can be used to generate nonlinear time
  series. It is similar to the function
  \code{\link{simulateLinearTS}}, but applies nonlinear
  functions to certain lags. The nonlinear functions
  currently used are: cos, sin, tanh, atan, and
  exp(-x/10000). For a detailed explanation see the
  referenced literature.
}
\references{
  C. Bergmeir, J.M. Benítez, On the Use of Cross-validation
  for Time Series Predictor Evaluation. Information
  Sciences 191 (2012) 192-213.
}

