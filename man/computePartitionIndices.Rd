\name{computePartitionIndices}
\alias{computePartitionIndices}
\title{Compute the indices for the partitioning scheme}
\usage{
  computePartitionIndices(len, type = "lastBlock",
    order = 0, ratioLB = 0.15, numPartitions = 0,
    ratioValSet = 0, seed = 1, notEmbedded = FALSE)
}
\arguments{
  \item{len}{the length of the target series}

  \item{type}{one of \code{c("lastBlock", "CV", "noDepCV",
  "blockedCV")}}

  \item{order}{the embedding dimension, if the indices are
  computed for a series that is not embedded. If the
  indices are used with an embedded series, this has to be
  set to zero.}

  \item{ratioLB}{if between zero and one, defines the ratio
  of data that are used as test set. If greater one,
  defines the absolute value of instances in the test set.}

  \item{numPartitions}{the numer of partitions to generate,
  for the cross-validation schemes}

  \item{ratioValSet}{if an additional validation set is
  generated, its ratio (zero to not generate additional
  validation set)}

  \item{seed}{the seed to use for randomly partitioning the
  data during cross-validation}

  \item{notEmbedded}{if this flag is true, the indices are
  shifted by ``order'' into the future, so that the results
  are the indices in the not embedded series, of the values
  which are the target values in the embedded series.}
}
\description{
  This function computes the indices that can later be used
  to partition the time series into training and test sets,
  for different validation schemes. Currently supported
  are: "lastBlock", "CV", "noDepCV", "blockedCV".
}

