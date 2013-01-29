\name{loadDifToATSList}
\alias{loadDifToATSList}
\title{Load xls files into an aTS list}
\usage{
  loadDifToATSList(path, base)
}
\arguments{
  \item{path}{the path to use}

  \item{base}{the filename (without extension)}
}
\value{
  an \code{\link{ATSList}}
}
\description{
  Use OpenOffice to generate a dif file (without any
  header, ASCII encoding). Then, use this function to load
  it to an ATSList
}
\details{
  TODO: Before using this function, it should be revised
  and updated (e.g. to use \code{\link{assemblePathName}}).
}

