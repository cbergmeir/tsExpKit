\name{plotPredictions}
\alias{plotPredictions}
\title{Function to plot predictions}
\usage{
  plotPredictions(qualifiedPredictionFileNames,
    referenceDataDir, originalDataDir, targetDir)
}
\arguments{
  \item{qualifiedPredictionFileNames}{a list of file names
  with paths to all the prediction files to use}

  \item{referenceDataDir}{the directory where the reference
  data is located (test sets)}

  \item{originalDataDir}{the directory where the original
  data is located (full time series)}

  \item{targetDir}{directory to which to save to the plots}
}
\description{
  This function is used to plot predictions as overlay on
  the original time series.
}
\details{
  TODO: Why does this function take directory names, and
  \code{\link{evaluatePredictions}} takes a \code{usedData}
  structure?
}

