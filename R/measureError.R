#' This function can be used to calculate a lot of different error measures for predictions.
#' 
#' @title Calculate an error measure for a predictions
#' @param measure currently implementad are the error measures:
#' "MSE", "RMSE", "SSE", "MAE", "MdAE", "MAPE", "MdAPE", "sMAPE", "sMdAPE", "MRAE", "MdRAE", 
#' "GMRAE", "RelMAE", "RelRMSE", "LMR", "PB", "PBMAE", "PBMSE", "DF", "DF_unnorm", "DF_percent", "DF_sig"
#' @param prediction a vector containing the predictions
#' @param reference a vector containing the "true" reference values
#' @param benchmark if needed by the error measure, a benchmark forecast, such as the naive forecast (always take the last known value as forecast).  
#' @export
measureError <- function(measure, prediction, reference, benchmark)  
{
  #if(mode(prediction) != "numeric") warning("prediction has to be numeric");
#  if(mode(reference) == "numeric") residuals = reference-prediction
#  if(mode(benchmark) == "numeric") relativeResiduals = prediction/benchmark
#  if(mode(reference) == "numeric" && mode(benchmark) == "numeric") baseResiduals = reference-benchmark

  residuals = reference - prediction
  baseResiduals = reference - benchmark
  relativeResiduals = residuals / baseResiduals
  
  switch(measure, 
      MSE = {
        mean(residuals^2)
      },
      RMSE = {
        sqrt(mean(residuals^2))
      },
      SSE = {
        sum(residuals^2)
      },      
      MAE = {
        mean(abs(residuals))
      },
      MdAE = {
        median(abs(residuals))
      },
      MAPE = {
        e <- abs(residuals/reference)
        mean( e[is.finite(e)] )  
        #mean( e )
      },
      MdAPE = {
        e <- abs(residuals/reference)
        median( e[is.finite(e)] )  
      },
      sMAPE = {
        e <- 2*abs(residuals)/(abs(reference)+abs(prediction))
        mean( e[is.finite(e)] )  
      },  
      sMdAPE = {
        e <- 2*abs(residuals)/(abs(reference)+abs(prediction))
        median( e[is.finite(e)] )  
      },  
      MRAE = {
        mean(abs(relativeResiduals))
      },  
      MdRAE = {
        median(abs(relativeResiduals))
      },  
      GMRAE = {
        exp(mean(log(abs(relativeResiduals))))
      },  
      RelMAE = {
        mae = mean(abs(residuals))
        maeBase = mean(abs(baseResiduals))
        mae/maeBase   
      },  
      #the RelRMSE is equivalent to Theil's U
      RelRMSE = {
        rmse = sqrt(mean(residuals^2))
        rmseBase = sqrt(mean(baseResiduals^2))
        rmse/rmseBase   
      },  
      LMR = {
        mse = mean(residuals^2)
        mseBase = mean(baseResiduals^2)
        log(mse/mseBase)   
      },
      PB = {
        mean(abs(relativeResiduals)<1)
      },   
      PBMAE = {
        mae = mean(abs(residuals))
        maeBase = mean(abs(baseResiduals))
        mean(mae < maeBase)
      },   
      PBMSE = {
        mse = mean(residuals^2)
        mseBase = mean(baseResiduals^2)
        mean(mse < mseBase) 
      },
      MDA = {
        z = (((reference - benchmark) > 0) == ((prediction - benchmark) > 0))
        
        res <- seq(1,1,length=length(reference))
        res[-which(z)] <- - res[-which(z)]
        mean(res)
      },
      MDV = {
        z = (((reference - benchmark) > 0) == ((prediction - benchmark) > 0))
        
        res <- abs(reference - benchmark)
        res[-which(z)] <- - res[-which(z)]
        mean(res)
      },      
      MDPV = {
        z = (((reference - benchmark) > 0) == ((prediction - benchmark) > 0))
        
        res <- abs((reference - benchmark) / benchmark)
        res[-which(z)] <- - res[-which(z)]
        mean(res)
      },
      DF_sig = {
        x <- (prediction - benchmark) * (reference - benchmark) #/ abs(benchmark)
        2*sigmoid(x*5)-1
      }
      
      
  
  )
}

#' This function calls \code{\link{measureError}} for all error measures available.
#' 
#' @param prediction a vector containing the predictions
#' @param reference a vector containing the "true" reference values
#' @param benchmark if needed by the error measure, a benchmark forecast, such as the naive forecast (always take the last known value as forecast).  
#' @title This function calculates all error measures that are available
#' @return a \code{data.frame} containing all the measures
#' @export
measureErrorAll <- function(prediction, reference, benchmark)
{

  res <- list()
  
  allMeasures <- c("MSE", "RMSE", "SSE", "MAE", "MdAE", "MAPE", "MdAPE", "sMAPE", "sMdAPE", 
      "MRAE", "MdRAE", "GMRAE", "RelMAE", "RelRMSE", "LMR", "PB", "PBMAE", "PBMSE", "MDA", "MDV", "MDPV")#, "DF_sig")
  
  for(measure in allMeasures) {
    res[[measure]] <- as.numeric(measureError(measure, prediction, reference, benchmark))
    #resTest <- lapply(resTest, as.numeric)
  }
  
  as.data.frame(res)
}