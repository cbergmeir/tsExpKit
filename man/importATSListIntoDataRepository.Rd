\name{importATSListIntoDataRepository}
\alias{importATSListIntoDataRepository}
\title{Function to initially import an ATSList into a time series repository}
\usage{
  importATSListIntoDataRepository(atsList, dataPath,
    dataPackageName, partSuffix = "5-fold",
    embeddingParameters = list(lags = c(-4, -3, -2, -1, 0)),
    partitionParametersKeelExp = list(type = c("blockedCV"), order = 0, numPartitions = 5, ratioValSet = 0),
    partitionParametersNorm = list(type = c("lastBlock"), order = 0, ratioLB = 0.2, ratioValSet = 0),
    normParameters = list(), genDescStatTab = TRUE,
    genStatNonlinTab = TRUE, genStatOtherTestsTab = FALSE,
    minLength = 10)
}
\arguments{
  \item{atsList}{the time series list}

  \item{dataPath}{path of the data package}

  \item{dataPackageName}{name of the data package}

  \item{partSuffix}{a string to append to the filenames}

  \item{embeddingParameters}{parameters for the embedding}

  \item{partitionParametersKeelExp}{parameters for
  partitioning in the export to KEEL}

  \item{partitionParametersNorm}{parameters for
  partitioning in the versions of the series that are
  normalized}

  \item{normParameters}{parameters of the normalization}

  \item{genDescStatTab}{Should the descriptive statistics
  table be generated?}

  \item{genStatNonlinTab}{Should the table of the results
  of the linearity tests be generated? (This may be slow)}

  \item{genStatOtherTestsTab}{Should the table with the
  other tests be generated? (This also may be slow)}

  \item{minLength}{minimal length of the series to be
  considered}
}
\description{
  Function to initially import an ATSList into a time
  series repository
}

