\name{loadDatDirToATSList}
\alias{loadDatDirToATSList}
\title{Load a directory of ".dat" files into an \code{\link{ATSList}}}
\usage{
  loadDatDirToATSList(path)
}
\arguments{
  \item{path}{the path of the directory}
}
\description{
  This function calls \code{\link{loadDataDirToATSList}}
  with the parameters \code{pattern="+\\.dat"} and
  \code{fileReadFunction=scan}.
}

