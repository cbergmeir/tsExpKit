\name{computePartitions}
\alias{computePartitions}
\alias{computePartitions.aTS}
\alias{computePartitions.eTS}
\title{Compute partitions of a time series}
\usage{
  computePartitions(obj, ...)

  \method{computePartitions}{aTS} (obj, ...)

  \method{computePartitions}{eTS} (obj, order, ...)
}
\arguments{
  \item{obj}{the \code{\link{aTS}} object}

  \item{...}{function parameters passed to
  \code{\link{computePartitionIndices}}}

  \item{order}{For \code{\link{eTS}} objects: If the
  \code{order} is missing, it is extracted for the
  attributes of the series.}
}
\value{
  the partitioned time series
}
\description{
  Compute partitions of a time series

  This function calls \code{\link{computePartitionIndices}}
  to calculate the partition indices, and then finds the
  values to the partition indices.
}

