\name{computeIndicesLastBlockEval}
\alias{computeIndicesLastBlockEval}
\title{Function to split data into training and test set for last block evaluation.}
\usage{
  computeIndicesLastBlockEval(len, order, ratio = 0.15)
}
\arguments{
  \item{len}{the length of the time series to be used}

  \item{order}{the order of the embedding to be used}

  \item{ratio}{the ratio of training and test set}
}
\value{
  a named list with the following elements:
  \item{trainIndices}{a matrix containing the training
  indices} \item{testIndices}{a matrix containing the test
  indices}
}
\description{
  This function is very similar to
  \code{\link{computeTrainingAndTestIndices}}, but instead
  of a vector of indices, it takes a length and an
  embedding order.
}

