\name{partitionATSDir}
\alias{partitionATSDir}
\title{Partition (to training and test sets) all time series in an ATS directory}
\usage{
  partitionATSDir(inPath,
    partitionParameters = list(type = c("blockedCV"), order = 0, numPartitions = 5, ratioValSet = 0.2),
    outPath = NULL, savePlots = TRUE)
}
\arguments{
  \item{inPath}{the input path, i.e., the ATS directory}

  \item{partitionParameters}{a list giving all parameters
  that are passed to \code{\link{computePartitions}}}

  \item{outPath}{the output path. If \code{NULL}, it is
  generated automatically as a subdirectory of the
  \code{inPath}}

  \item{savePlots}{if \code{TRUE}, a directory containing
  plots of all partitions is generated}
}
\description{
  The function loads all files in an ATS directory into an
  ATSList, applies \code{\link{computePartitions}}, and
  saves the result to an ATS directory.
}
\seealso{
  \code{\link{embedATSDir}}, \code{\link{normalizeATSDir}}
}

