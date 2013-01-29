\name{plotPartitionsATS}
\alias{plotPartitionsATS}
\title{Save plots of data partitions to files}
\usage{
  plotPartitionsATS(aTS, partitions, partDir)
}
\arguments{
  \item{aTS}{the original time series as an
  \code{\link{aTS}} object}

  \item{partitions}{a partitioned time series. Usually,
  this is the output of \code{\link{computePartitions}}}

  \item{partDir}{the destination directory}
}
\description{
  This function is used to plot the partitions of a time
  series as overlay over the original series.
}

