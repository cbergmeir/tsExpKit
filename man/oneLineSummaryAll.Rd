\name{oneLineSummaryAll}
\alias{oneLineSummaryAll}
\title{Generate a "one-line-summary" for a time series}
\usage{
  oneLineSummaryAll(ts, name, type=c("StatNonlin",
    "DescStat", "OtherTests"), ...)
}
\arguments{
  \item{ts}{coredata of a time series}

  \item{name}{the name of the time series}

  \item{type}{the type of the "one-line-summary" to
  calculate. Currently implemented are \code{"StatNonlin"},
  \code{"DescStat"}, and \code{"OtherTests"}}

  \item{...}{parameters passed to the respective
  "one-line-summary" function}
}
\description{
  This function calls, depending on the parameter
  \code{type}, one of the following functions, and returns
  their result: \code{\link{oneLineSummaryStatNonlin}},
  \code{\link{oneLineSummaryOtherTests}}, or
  \code{\link{oneLineSummaryDescStat}}.
}

