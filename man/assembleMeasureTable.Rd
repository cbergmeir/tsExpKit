\name{assembleMeasureTable}
\alias{assembleMeasureTable}
\title{Create a table for evaluation of various experiment runs}
\usage{
  assembleMeasureTable(resultCollection, measure,
    na.rm = TRUE)
}
\arguments{
  \item{resultCollection}{a list containing the paths to
  the different experiment runs}

  \item{measure}{the filename of the measure, e.g.
  \code{"predEval_RMSE.csv"}.}

  \item{na.rm}{if \code{TRUE}, cases where NAs are present
  are removed}
}
\description{
  This function can be used to unite evaluations of
  different experiment runs, both accross datasets and
  methods, for later evaluation.
}
\details{
  TODO: Currently no examples available. Consult with the
  maintainer if you can't figure out how to use this
  function.
}

