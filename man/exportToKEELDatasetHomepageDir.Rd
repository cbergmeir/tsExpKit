\name{exportToKEELDatasetHomepageDir}
\alias{exportToKEELDatasetHomepageDir}
\title{Function to export from a time series repository to a format that is used on the department homepage}
\usage{
  exportToKEELDatasetHomepageDir(dataPackagePath,
    dataPackageName, wholeEmbeddedSeriesPath,
    partitionsPath, partSuffix = "5-fold",
    targetPath = NULL)
}
\arguments{
  \item{dataPackagePath}{path of the data package}

  \item{dataPackageName}{name of the data package}

  \item{wholeEmbeddedSeriesPath}{path of the embedded
  series (unpartitioned)}

  \item{partitionsPath}{path of the partitioned series}

  \item{partSuffix}{a string to append to the filenames}

  \item{targetPath}{the output path}
}
\description{
  Function to export from a time series repository to a
  format that is used on the department homepage
}

