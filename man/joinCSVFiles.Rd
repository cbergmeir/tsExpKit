\name{joinCSVFiles}
\alias{joinCSVFiles}
\title{Join all ".csv" files in a directory into one file}
\usage{
  joinCSVFiles(path, pattern="\\\\.csv", targetFilename)
}
\arguments{
  \item{path}{the directory where the ".csv" files are}

  \item{pattern}{the file ending. This will usually be
  "\\.csv"}

  \item{targetFilename}{the target file which contains all
  files joined together.}
}
\description{
  This function finds all ".csv" files in a directory and
  joins them into one big file, using \code{rbind}.
}

