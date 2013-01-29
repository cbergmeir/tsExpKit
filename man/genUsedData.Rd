\name{genUsedData}
\alias{genUsedData}
\title{Function to generate the \code{usedData} structure}
\usage{
  genUsedData(useCases, onlyUseStationarySeries = TRUE,
    removeDuplicatesByDescStatTable = TRUE,
    useSelectionFile = FALSE, selectionFile = "")
}
\arguments{
  \item{useCases}{the use cases to add}

  \item{onlyUseStationarySeries}{should the files be
  filtered for stationary series?}

  \item{removeDuplicatesByDescStatTable}{should duplicate
  series be determined by statistical properties and be
  removed?}

  \item{useSelectionFile}{should a selection file be used
  to determine, which series actually to use?}

  \item{selectionFile}{if \code{useSelectionFile} is TRUE,
  here the filename of the selection file has to be given}
}
\description{
  Function to generate the \code{usedData} structure, which
  determines the data to be used during the
  experimentation.
}

