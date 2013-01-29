\name{savePlotsATSList}
\alias{savePlotsATSList}
\title{Save plots of every time series in an \code{\link{ATSList}} to files}
\usage{
  savePlotsATSList(atsList, filename, acf = TRUE,
    toSeparateJpegs = FALSE)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} to use}

  \item{filename}{the filename or the prefix for the
  filename}

  \item{acf}{if \code{TRUE}, the acf plot is generated and
  saved along with the plot of the series}

  \item{toSeparateJpegs}{if \code{TRUE}, every time series
  plot is saved as a separate jpeg file. Otherwise, a
  single ".pdf" file is generated.}
}
\description{
  This function saves plots of all time series in an
  \code{\link{ATSList}} to disk.
}

