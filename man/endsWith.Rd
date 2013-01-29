\name{endsWith}
\alias{endsWith}
\title{Utility function to find ending of a string}
\usage{
  endsWith(myString, mySubString)
}
\arguments{
  \item{myString}{string to determine ending}

  \item{mySubString}{string to compare to}
}
\value{
  \code{TRUE}, if \code{myString} ends with
  \code{mySubString}, FALSE otherwise
}
\description{
  Determines, of the string \code{myString} ends with the
  string \code{mySubString}. The function is useful, e.g.,
  when determining the file ending of a filename.
}

