\name{writeATSListToKeelDirectory}
\alias{writeATSListToKeelDirectory}
\title{Write a list of aTS objects to a directory, in KEEL format}
\usage{
  writeATSListToKeelDirectory(atsList, path,
    useColNames = FALSE, compression = "none",
    compFilename = "series.zip")
}
\arguments{
  \item{atsList}{the atsList to write out}

  \item{path}{the path to write the data to}

  \item{useColNames}{should the column names be used?}

  \item{compression}{the compression to use. Possible
  values are \code{zipSingle}, \code{zipPackage}, and
  \code{none}}

  \item{compFilename}{the file name of the compressed
  output file}
}
\description{
  Write a list of aTS objects to a directory, in KEEL
  format
}

