\name{normalizeATSDir}
\alias{normalizeATSDir}
\title{Normalize all time series in an ATS directory}
\usage{
  normalizeATSDir(inPath, normParameters = list(),
    outPath = NULL)
}
\arguments{
  \item{inPath}{the input path, i.e., the ATS directory}

  \item{normParameters}{a list giving all parameters that
  are passed to \code{\link{normalize}}}

  \item{outPath}{the output path. If \code{NULL}, it is
  generated automatically as a subdirectory of the
  \code{inPath}}
}
\description{
  The function loads all files in an ATS directory into an
  ATSList, applies \code{\link{normalize}}, and saves the
  result to an ATS directory. If the ATS directory contains
  training and test sets, the test sets are normalized
  using the parameters of the corresponding training sets.
}
\seealso{
  \code{\link{partitionATSDir}}, \code{\link{embedATSDir}}
}

