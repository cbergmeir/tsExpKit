\name{oneLineSummaryDescStat}
\alias{oneLineSummaryDescStat}
\title{Generate a "one-line-summary" of type "DescStat"}
\usage{
  oneLineSummaryDescStat(ts, name, order = 1)
}
\arguments{
  \item{ts}{coredata of a time series}

  \item{name}{the name of the time series}

  \item{order}{the order to be assumed by tests which need
  it}
}
\description{
  This function computes a bunch of descriptive statistics
  on the time series, such as the mean, the standard
  deviation, the kurtosis, the skewness, and different
  versions of permutation entropy.
}

