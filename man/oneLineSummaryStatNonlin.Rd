\name{oneLineSummaryStatNonlin}
\alias{oneLineSummaryStatNonlin}
\title{Generate a "one-line-summary" of type "StatNonlin"}
\usage{
  oneLineSummaryStatNonlin(ts, name,
    alphaStationary = 0.01, alphaNonlinear = 0.01,
    order = 1, ...)
}
\arguments{
  \item{ts}{coredata of a time series}

  \item{name}{the name of the time series}

  \item{alphaStationary}{significance level for the
  stationarity tests}

  \item{alphaNonlinear}{significance level for the
  linearity tests}

  \item{order}{the order to be assumed by tests which need
  it}

  \item{...}{currently not used}
}
\description{
  This function computes a bunch of measures of the time
  series, mainly statistical tests on the stationarity and
  linearity of the series.
}

