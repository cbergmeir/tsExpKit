\name{removeFileEnding}
\alias{removeFileEnding}
\title{Remove file ending from a path string}
\usage{
  removeFileEnding(file, ending)
}
\arguments{
  \item{file}{a string}

  \item{ending}{a substring}
}
\value{
  the \code{file} string without the last characters. The
  amount of characters removed is determined by the length
  of \code{ending}.
}
\description{
  This function is a convenience function that simply does:
  \code{substr(file, 0, nchar(file)-nchar(ending))}
}

