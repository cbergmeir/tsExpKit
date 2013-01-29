\name{list.dirs}
\alias{list.dirs}
\title{find all subdirectories in a directory}
\usage{
  list.dirs(path = ".", pattern = NULL, all.dirs = FALSE,
    ignore.case = FALSE, recursive = FALSE)
}
\arguments{
  \item{path}{the path to explore}

  \item{pattern}{passed to \code{list.files}}

  \item{all.dirs}{passed to \code{list.files}}

  \item{ignore.case}{passed to \code{list.files}}

  \item{recursive}{explore the directory recursively,
  passed to \code{list.files}}
}
\description{
  This function calls \code{list.files}, and filters the
  result for directories
}
\author{
  The code is taken from a post on stackoverflow:

  http://stackoverflow.com/questions/4749783/how-to-obtain-a-list-of-directories-within-a-directory-like-list-files-but-in
}

