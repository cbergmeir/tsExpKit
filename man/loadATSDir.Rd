\name{loadATSDir}
\alias{loadATSDir}
\title{Load an ATS directory into an \code{\link{ATSList}}}
\usage{
  loadATSDir(path, pattern="+\\\\.RData", ...)
}
\arguments{
  \item{path}{the path of the directory}

  \item{pattern}{a pattern, to e.g. choose the file ending
  of the files}

  \item{...}{currently not used}
}
\value{
  an \code{\link{ATSList}} containing all the time series
  that were read
}
\description{
  This function loads an ATS directory into an
  \code{\link{ATSList}}.
}

