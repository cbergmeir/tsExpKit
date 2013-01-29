\name{embedATSDir}
\alias{embedATSDir}
\title{Embed all time series given in an ATS directory}
\usage{
  embedATSDir(inPath,
    embeddingParameters = list(lags = c(-4, -3, -2, -1, 0)),
    outPath = NULL)
}
\arguments{
  \item{inPath}{the input path, i.e., the ATS directory}

  \item{embeddingParameters}{a list giving all parameters
  that are passed to \code{\link{embedATSList}}}

  \item{outPath}{the output path. If \code{NULL}, it is
  generated automatically as a subdirectory of the
  \code{inPath}}
}
\description{
  The function loads all files in an ATS directory into an
  ATSList, applies \code{\link{embedATSList}}, and saves
  the result to an ATS directory.
}
\seealso{
  \code{\link{partitionATSDir}},
  \code{\link{normalizeATSDir}}
}

