\name{runPrediction}
\alias{runPrediction}
\title{Calculates predictions from a list of models and input data, models are in memory}
\usage{
  runPrediction(usedData, model, expPath,
    predictFunc = predict)
}
\arguments{
  \item{usedData}{the usedData structure to use (see
  \code{\link{genUsedData}})}

  \item{model}{a list of all the models (usually, the
  output of \code{\link{runExperiments}})}

  \item{expPath}{the output path, see
  \code{\link{assembleExperimentsPath}} and
  \code{\link{runExperiments}}}

  \item{predictFunc}{the function that is applied to the
  model to get the prediction}
}
\description{
  This function uses a bunch of models, created with
  \code{\link{runExperiments}}, and input data defined in
  \code{usedData}, together with a prediction function to
  calculate predictions for all data (in the test sets)
  applying all the trained models. The difference between
  this function and the function
  \code{\link{runPredictions}} is, that the models are not
  saved on disk and then reloaded. Depending on the
  implementation of the models, this may cause problems.
}
\details{
  TODO: I think this function is not needed/used any more,
  as the predictions are calculated directly in
  \code{\link{runExperiments}} after model building.
}
\seealso{
  \code{\link{genUsedData}}, \code{\link{runExperiments}},
  \code{\link{runExperiments}},
  \code{\link{runPredictions}}
}

