\name{evaluateModels}
\alias{evaluateModels}
\title{Function to evaluate models built during an experiment run}
\usage{
  evaluateModels(qualifiedModelFileNames,
    calcDenorm = TRUE)
}
\arguments{
  \item{qualifiedModelFileNames}{a list of file names with
  paths to all the model files to use}

  \item{calcDenorm}{if \code{TRUE}, all error measures are
  also calculated for denormalized model fits, along with
  the measures for normalized fits that are always
  calculated}
}
\description{
  This function is used to generate from a bunch of models
  saved as ".RData" files evaluations in form of tables
  which contain all training/fitting errors of the models
  and, depending on the models, some other tables with
  model characteristics.
}

