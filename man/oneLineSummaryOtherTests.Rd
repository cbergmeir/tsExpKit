\name{oneLineSummaryOtherTests}
\alias{oneLineSummaryOtherTests}
\title{Generate a "one-line-summary" of type "OtherTests"}
\usage{
  oneLineSummaryOtherTests(ts, name, order = 1,
    alpha = 0.01)
}
\arguments{
  \item{ts}{coredata of a time series}

  \item{name}{the name of the time series}

  \item{order}{the order to be assumed by tests which need
  it}

  \item{alpha}{significance level for the tests}
}
\description{
  This function computes a bunch of measures of the time
  series, like a runs test, the Bds test, the
  McLeod.Li.test, etc.
}

