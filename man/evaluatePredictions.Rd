\name{evaluatePredictions}
\alias{evaluatePredictions}
\title{Function to evaluate predictions}
\usage{
  evaluatePredictions(qualifiedPredictionFileNames,
    usedData, denorm = FALSE)
}
\arguments{
  \item{qualifiedPredictionFileNames}{a list of file names
  with paths to all the prediction files to use}

  \item{usedData}{the \code{usedData} structure which
  defines the input files. From here, the reference and
  benchmark values are extracted.}

  \item{denorm}{if \code{TRUE}, all error measures are also
  calculated using denormalized predictions and
  denormalized reference values and benchmarks.}
}
\description{
  This function is used to generate from predictions and
  the respective "real" reference values (determined by
  \code{usedData}) evaluations in form of tables which
  contain all errors of these predictions (i.e., errors on
  the test set).
}

