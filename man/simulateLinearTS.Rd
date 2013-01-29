\name{simulateLinearTS}
\alias{simulateLinearTS}
\title{Generate AR, MA, and ARMA series}
\usage{
  simulateLinearTS(length, ar = TRUE, ma = TRUE, lags = 4,
    seed = 1, maxRoot = 5, n.start = NA, ...)
}
\arguments{
  \item{length}{the length of the series to be generated}

  \item{ar}{if \code{TRUE}, series has an AR part}

  \item{ma}{if \code{TRUE}, series has an MA part}

  \item{lags}{the number of lags}

  \item{seed}{a seed used for random number generation}

  \item{maxRoot}{the roots of the characteristic
  polynomials are chosen between 1.1 and \code{maxRoot}}

  \item{n.start}{burn-in period. if NA, calculated
  automatically by arima.sim}

  \item{...}{parameters passed to arima.sim}
}
\value{
  a list containing all the parameters, and a member
  \code{ts} with the generated series
}
\description{
  This function can be used to generate pure AR, pure MA,
  and ARMA series. The AR part will be stationary and the
  MA part invertible. Therefore, the coefficients are not
  given directly, but a value \code{maxRoot} which controls
  the interval from which the roots for the characteristic
  polynomials are chosen. So, all the roots of the
  characteristic polynomials are real-valued. For a
  detailed explanation see the referenced literature.
}
\references{
  C. Bergmeir, J.M. Benítez, On the Use of Cross-validation
  for Time Series Predictor Evaluation. Information
  Sciences 191 (2012) 192-213.
}

