\name{getFileNameFromPath}
\alias{getFileNameFromPath}
\title{Get the filename from a full path}
\usage{
  getFileNameFromPath(path)
}
\arguments{
  \item{path}{a string}
}
\value{
  returns a substring of \code{path}
}
\description{
  This function splits the input string along slashes ("/")
  and returns everything after the last slash it finds.
}

