\name{computeTrainingAndTestIndices}
\alias{computeTrainingAndTestIndices}
\title{Function to split data indices into indices for training and test set}
\usage{
  computeTrainingAndTestIndices(indices, ratio = 0.15)
}
\arguments{
  \item{indices}{the indices to be used}

  \item{ratio}{ratio of training and test sets (default:
  15\% of the data is used for testing)}
}
\value{
  a named list with the following elements:
  \item{trainIndices}{a matrix containing the training
  indices} \item{testIndices}{a matrix containing the test
  indices}
}
\description{
  This function is very similar to
  \code{\link{computeTrainingAndTestIndices}}. It splits
  the given indices to indices for a training and a test
  set. Test set is taken from the end of the data. If the
  data is to be shuffled, this should be done before
  calling this function.
}

