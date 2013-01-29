\name{measureError}
\alias{measureError}
\title{Calculate an error measure for a predictions}
\usage{
  measureError(measure, prediction, reference, benchmark)
}
\arguments{
  \item{measure}{currently implementad are the error
  measures: "MSE", "RMSE", "SSE", "MAE", "MdAE", "MAPE",
  "MdAPE", "sMAPE", "sMdAPE", "MRAE", "MdRAE", "GMRAE",
  "RelMAE", "RelRMSE", "LMR", "PB", "PBMAE", "PBMSE", "DF",
  "DF_unnorm", "DF_percent", "DF_sig"}

  \item{prediction}{a vector containing the predictions}

  \item{reference}{a vector containing the "true" reference
  values}

  \item{benchmark}{if needed by the error measure, a
  benchmark forecast, such as the naive forecast (always
  take the last known value as forecast).}
}
\description{
  This function can be used to calculate a lot of different
  error measures for predictions.
}

