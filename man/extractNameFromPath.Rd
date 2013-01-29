\name{extractNameFromPath}
\alias{extractNameFromPath}
\title{Get the filename without file ending from a path}
\usage{
  extractNameFromPath(path, suffix)
}
\arguments{
  \item{path}{a string}

  \item{suffix}{a substring to remove from the end of
  \code{path}}
}
\value{
  a substring of \code{path}
}
\description{
  This function calls subsequently
  \code{\link{removeFileEnding}}, and
  \code{\link{getFileNameFromPath}}
}

