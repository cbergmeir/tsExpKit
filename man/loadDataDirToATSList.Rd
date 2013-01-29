\name{loadDataDirToATSList}
\alias{loadDataDirToATSList}
\title{Load a directory of files into an \code{\link{ATSList}}}
\usage{
  loadDataDirToATSList(path, pattern="+\\\\.dat",
    fileReadFunction=scan)
}
\arguments{
  \item{path}{the path of the directory}

  \item{pattern}{a pattern, to e.g. choose the file ending
  of the files}

  \item{fileReadFunction}{the function that is to be used
  to read single files}
}
\value{
  an \code{\link{ATSList}} containing all the time series
  that were read
}
\description{
  This function compiles a list of filenames according to
  \code{path} and \code{pattern}, and then uses the
  \code{fileReadFunction} to read the contents of the
  files. Then, \code{\link{aTS}} objects are constructed
  from the file contents and added to an
  \code{\link{ATSList}}.
}

