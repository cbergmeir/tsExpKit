#' @title Compute partitions of a time series
#' @param obj the \code{\link{aTS}} object
#' @param ... function parameters passed to \code{\link{computePartitionIndices}}
#' @export
computePartitions <- function(obj, ...) UseMethod("computePartitions")

#' This function calls \code{\link{computePartitionIndices}} to calculate the partition indices, and
#' then finds the values to the partition indices.
#' 
#' @return the partitioned time series
#' @rdname computePartitions
#' @S3method computePartitions aTS
#' @method computePartitions aTS
computePartitions.aTS <- function(obj, ...) {

  indices <- computePartitionIndices(len=nSamples(obj), ...)
  
#  res <- lapply(indices, function(ind){
#        if(!is.list(ind)) {
#          resInd <- obj[ind]
#        } else {
#          resInd <- lapply(ind, function(indMethod){
#                
#                if(is.matrix(indMethod)) {
#                  resIndMethod <- list()
#                  for(i in 1:ncol(indMethod)) {
#                    resIndMethod[[i]] <- obj[indMethod[,i]]
#                  }
#                  #names(resIndMethod) <- NULL
#                } else {
#                  resIndMethod <- obj[indMethod]
#                }
#                resIndMethod
#              })
#          attr(resInd, "shortname") <- attr(ind, "shortname")
#          #print(attr(ind, "shortname"))
#        }
#        
#        resInd
#      })

  #res <- lapply(indices, function(ind){ obj[ind] })
  
  res <- lapply(indices, function(ind){                
                if(is.matrix(ind)) {
                  resInd <- list()
                  for(i in 1:ncol(ind)) {
                    resInd[[i]] <- obj[na.omit(ind[,i])]
                  }
                } else {
                  resInd <- obj[na.omit(ind)]
                }
                resInd
              })
  
  #names(res) <- c("outSetData", "inSetTrainData", "inSetTestData")
  
  attr(res, "partitionParameters") <- attr(indices, "partitionParameters")
  res    
}


#' This function is used to save a partitioned time series to an ATS directory. 
#' 
#' @title Save partitioned time series to ATS directory
#' @param partitions a partitioned time series. Usually, this is the output of \code{\link{computePartitions}}
#' @param path the directory to which to save the series 
#' @export
savePartitions <- function(partitions, path) {
  
    setTypePathName <- path
  
  for(setType in names(partitions)){
    
    if(setType == "outSetData") {
      
      outSet <- partitions[[setType]]
      outSet <- setName(outSet, paste(getName(outSet), "-val",sep=""))
      save.aTS(outSet, setTypePathName)
      
    } else {
      
        if(setType == "inSetTestData") {
          modSelTypeShortName <- "tst"
        } else {
          modSelTypeShortName <- "tra"
        }
        
        nSets <- length(partitions[[setType]])
        for (modSelIter in 1:nSets) {

          currPart <- partitions[[setType]][[modSelIter]]
          aTSattr(currPart, "tsname") <- getName(currPart)
          currPartName <- paste(getName(currPart), "-", nSets, "-", modSelIter, modSelTypeShortName, sep="")
          currPart <- setName(currPart, currPartName)
          aTSattr(currPart, "shortname") <- paste(nSets, "-", modSelIter, modSelTypeShortName, sep="")
          save.aTS(currPart, setTypePathName)
          
        }
    }
  }  
}

#' This function is used to plot the partitions of a time series as overlay over the original series. 
#' 
#' @title Save plots of data partitions to files
#' @param aTS the original time series as an \code{\link{aTS}} object 
#' @param partitions a partitioned time series. Usually, this is the output of \code{\link{computePartitions}}
#' @param partDir the destination directory 
#' @export
plotPartitionsATS <- function(aTS, partitions, partDir) {
  
  trainPartitions <- partitions[["inSetTrainData"]]#loadATSDir(partDir, pattern=paste(getName(aTS),"+tra\\.RData",sep=""))
  testPartitions <- partitions[["inSetTestData"]]#loadATSDir(partDir, pattern=paste(getName(aTS),"+tst\\.RData",sep=""))
  
  pdf(file=assemblePathName(partDir, paste(getName(aTS), "-partitions.pdf", sep="")),onefile=TRUE,width=20,height=3*length(trainPartitions)) 
  par(mfrow=c(length(trainPartitions),1))
  
  for(currPartInd in 1:length(trainPartitions)) {

    currTestPart <- testPartitions[[currPartInd]]
    
    plot(aTS)
    points(trainPartitions[[currPartInd]], col="green")
    points(currTestPart, col="red")
  }  
  
  dev.off()
  
}

#' This function saves an \code{\link{aTS}} object to an ".RData" file.
#' 
#' @param aTS the \code{\link{aTS}} object of the time series 
#' @param dataPath the path to which to save the series to
#' @param name the (file-)name to which to save to 
#' @param ... currently not used
#' @title Save an aTS object to an ".RData" file
#' @S3method save aTS
#' @method save aTS
save.aTS <- function(aTS, dataPath, name, ...) {

  if(missing(name)) name <- getName(aTS)
  
  path <- assemblePathName(dataPath, name)
  
  assign(name, aTS)
  
  save(list=name, file = paste(path, ".RData", sep=""))
  
}



#' This function generates a list with some statistical 
#' information about the time series in the \code{\link{aTS}} object.
#' 
#' @title Generic summary function for aTS objects
#' @param object the \code{\link{aTS}} object
#' @param ... additional function parameters (currently not used)
#' @S3method summary aTS
#' @method summary aTS
summary.aTS <- function(object, ...) {
  res <- list()  
  res[["zoo_summary"]] <- NextMethod(.Generic)
  res[["adf.test"]] <- adf.test(coredata(object))
  res[["tnn.test"]] <- tnnTest(coredata(object))
  res
}


#' This function calls \code{\link{oneLineSummaryAll}} on the \code{coredata} of 
#' the \code{\link{aTS}} object.
#' 
#' @title Generate a "one-line-summary" for an aTS object
#' @param aTS the \code{\link{aTS}} object
#' @param ... parameters passed to \code{\link{oneLineSummaryAll}}
#' @export 
oneLineSummary <- function(aTS, ...) {
  oneLineSummaryAll(coredata(aTS), getName(aTS), ...)
}

#' This function calls, depending on the parameter \code{type}, one of the following functions, and returns their result:
#' \code{\link{oneLineSummaryStatNonlin}}, \code{\link{oneLineSummaryOtherTests}}, or \code{\link{oneLineSummaryDescStat}}.
#' 
#' @title Generate a "one-line-summary" for a time series
#' @param ts coredata of a time series
#' @param name the name of the time series
#' @param type the type of the "one-line-summary" to calculate. Currently 
#' implemented are \code{"StatNonlin"}, \code{"DescStat"}, and \code{"OtherTests"}
#' @param ... parameters passed to the respective "one-line-summary" function
#' @usage oneLineSummaryAll(ts, name, type=c("StatNonlin", "DescStat", "OtherTests"), ...)
#' @export 
oneLineSummaryAll <- function(ts, name, type=c("StatNonlin", "DescStat", "OtherTests"), ...) {
  
  if(type=="StatNonlin")
    oneLineSummaryFunc <- oneLineSummaryStatNonlin
  else if(type=="DescStat")
    oneLineSummaryFunc <- oneLineSummaryDescStat
  else
    oneLineSummaryFunc <- oneLineSummaryOtherTests
  
  oneLineSummaryFunc(ts, name, ...)
}

#' This function computes a bunch of measures of the time series, mainly 
#' statistical tests on the stationarity and linearity of the series.
#' 
#' @title Generate a "one-line-summary" of type "StatNonlin"
#' @param ts coredata of a time series
#' @param name the name of the time series
#' @param alphaStationary significance level for the stationarity tests
#' @param alphaNonlinear significance level for the linearity tests
#' @param order the order to be assumed by tests which need it 
#' @param ... currently not used
#' @export 
oneLineSummaryStatNonlin <- function(ts, name, alphaStationary=0.01, alphaNonlinear=0.01, order=1, ...) {
  
  if(missing(name))
    name <- deparse(substitute(ts))
  
#  res <- list()
#  res <- append(res, list("nSamples"=nSamples(aTS)))
  res <- list("len"=length(ts))  
  
  #adf test for stationarity (unit root)
  if(missing(order))
    adfRes <- try(adf.test(ts))
  else
    adfRes <- try(adf.test(ts, k=order))
  
  if(!inherits(adfRes, "try-error")) {
    res <- append(res, list("pAdf"=adfRes$p.value))
    res <- append(res, list("stAdf"=(adfRes$p.value <= alphaStationary)))    
  } else {
    res <- append(res, list("pAdf"=NA, "stAdf"=NA))
  }
  
  #tnn test for nonlinearity
  tnnRes <- tnnTest(ts, lag=order)

  #F distribution seems to work much better..
  #res <- append(res, list("pTnnChi2"=tnnRes@test$p.value[1]))
  #res <- append(res, list("nlTnnChi2"=(tnnRes@test$p.value[1] <= alphaNonlinear)))
  
  res <- append(res, list("pTnnF"=tnnRes@test$p.value[2]))
  res <- append(res, list("nlTnnF"=(tnnRes@test$p.value[2] <= alphaNonlinear)))
  
  #wnn test for nonlinearity
  wnnRes <- tnnTest(ts, lag=order)
  
  #F distribution seems to work much better..
  #res <- append(res, list("pWnnChi2"=wnnRes@test$p.value[1]))
  #res <- append(res, list("nlWnnChi2"=(wnnRes@test$p.value[1] <= alphaNonlinear)))
  
  res <- append(res, list("pWnnF"=wnnRes@test$p.value[2]))
  res <- append(res, list("nlWnnF"=(wnnRes@test$p.value[2] <= alphaNonlinear)))
  
  
  #tests from TSA package
  keenanRes <- Keenan.test(ts, order=order)
  res <- append(res, list("pKeenan"=keenanRes$p.value))
  res <- append(res, list("nlKeenan"=(keenanRes$p.value <= alphaNonlinear)))
  
  tsayRes <- Keenan.test(ts, order=order)
  res <- append(res, list("pTsay"=tsayRes$p.value))
  res <- append(res, list("nlTsay"=(tsayRes$p.value <= alphaNonlinear)))
  
#  #tests from tsDyn package
#  
#  deltaRes <- try(delta.lin.test(ts, B=70))
#  if(inherits(deltaRes, "try-error")) deltaRes <- NA
#  
#  pDeltaMed <- median(deltaRes, na.rm=TRUE)
#  res <- append(res, list("pDelta.Med"=pDeltaMed))
#  res <- append(res, list("sigDelta.Med"=(pDeltaMed <= alphaNonlinear)))
#  
#  #the test for nonlinearity used in ncstar
#  eTS <- eTS(ts, lags=c((-order):0))
#  
#  ncstarTest <- linearityTest(eTS, sig=1-alphaNonlinear)
#  
#  res <- append(res, list("pNcstar"=(1-ncstarTest$pValue)))
#  res <- append(res, list("nlNcstar"=ncstarTest$isSignificant))
#  
  
  res <- data.frame(res)
  rownames(res) <- name
  res
} 

#' This function computes a bunch of measures of the time series, 
#' like a runs test, the Bds test, the McLeod.Li.test, etc.
#' 
#' @title Generate a "one-line-summary" of type "OtherTests"
#' @param ts coredata of a time series
#' @param name the name of the time series
#' @param order the order to be assumed by tests which need it
#' @param alpha significance level for the tests
#' @export
oneLineSummaryOtherTests <- function(ts, name, order=1, alpha=0.01) {
  
  if(missing(name))
    name <- deparse(substitute(ts))
  
  res <- list("len"=length(ts)) 
  
  #test from fNonlinear

  #data has to be centered for this version of the runs test to work
  #runsRes1 <- runsTest(ts)
  #res <- append(res, list("pRuns"=runsRes1$p.value))
  #res <- append(res, list("sigRuns"=(runsRes1$p.value <= alpha)))

  bdsRes <- bdsTest(ts, m=order)
  res <- append(res, list("pBdsEps1"=bdsRes@test$p.value[1]))
  res <- append(res, list("sigBdsEps1"=(bdsRes@test$p.value[1] <= alpha)))

  res <- append(res, list("pBdsEps2"=bdsRes@test$p.value[2]))
  res <- append(res, list("sigBdsEps2"=(bdsRes@test$p.value[2] <= alpha)))
  
  res <- append(res, list("pBdsEps3"=bdsRes@test$p.value[3]))
  res <- append(res, list("sigBdsEps3"=(bdsRes@test$p.value[3] <= alpha)))
  
  res <- append(res, list("pBdsEps4"=bdsRes@test$p.value[4]))
  res <- append(res, list("sigBdsEps4"=(bdsRes@test$p.value[4] <= alpha)))

  bdsRes2 <- bdsTest(ts)
  pBdsMed <- median(bdsRes2@test$p.value, na.rm=TRUE)
  res <- append(res, list("pBds.Med"=pBdsMed))
  res <- append(res, list("sigBds.Med"=(pBdsMed <= alpha)))
  
  
  #tests from TSA package
  runsRes <- try(runs(ts, k=median(ts)))
  if(inherits(runsRes, "try-error")) {
    runsRes <- list(pvalue=NA)
  }
  
  res <- append(res, list("pRunsTSA"=runsRes$pvalue))
  res <- append(res, list("sigRunsTSA"=(runsRes$pvalue <= alpha)))
  
  #tests from tsDyn package

  deltaRes <- try(delta.test(ts, B=70))
  if(inherits(deltaRes, "try-error")) deltaRes <- NA
  
  pDeltaMed <- median(deltaRes, na.rm=TRUE)
  res <- append(res, list("pDelta.Med"=pDeltaMed))
  res <- append(res, list("sigDelta.Med"=(pDeltaMed <= alpha)))

  
  #mcLeodLiRes <- McLeod.Li.test(NULL, ts, )
  
  res <- data.frame(res)
  rownames(res) <- name
  res
  
}

#' This function computes a bunch of descriptive statistics on the 
#' time series, such as the mean, the standard deviation,
#' the kurtosis, the skewness, and different versions of permutation entropy. 
#' 
#' @title Generate a "one-line-summary" of type "DescStat"
#' @param ts coredata of a time series
#' @param name the name of the time series
#' @param order the order to be assumed by tests which need it
#' @export 
oneLineSummaryDescStat <- function(ts, name, order=1) {
  
  if(missing(name))
    name <- deparse(substitute(ts))
  
  #calculate length of TS
  res <- list("len"=length(ts)) 
 # print("length ts")
 # print(res)
  #calculate mean and std deviation
  res <- append(res, list("mean"=mean(ts)))
  res <- append(res, list("sd"=sd(ts)))
 # print("mean and sd")
 # print(res)
  
  #from TSA
  #calculate kurtosis and skewness
  res <- append(res, list("kurt"=TSA::kurtosis(ts)))
  res <- append(res, list("skew"=TSA::skewness(ts)))
 # print("kurt and skew")
 # print(res)
 
  res <- data.frame(res)
  rownames(res) <- name
#print("final res")
#  print(res)
  res
  
}

#' This function calls the function \code{\link{normalize}} on 
#' the \code{coredata} of the \code{\link{aTS}} object
#' 
#' @title normalize an \code{\link{aTS}} object
#' @param obj the \code{\link{aTS}} object
#' @param ... additional function parameters (currently not used)
#' @export
normalize <- function(obj, ...) UseMethod("normalize")

#' @param type the type of the normalization
#' @param normParams normalization parameters to be used. 
#' When omitted, the type is used and the parameters are calculated from the data.
#' @rdname normalize
#' @S3method normalize aTS
#' @method normalize aTS
normalize.aTS <- function(obj, type="norm", normParams=type, ...) {
  
  res <- obj
  
  normData <- normalizeData(coredata(obj), normParams)

  coredata(res) <- normData
  
  aTSattr(res, "normParams") <- attr(normData, "normParams")
  res
}

#' This function is a getter function for the \code{normParams} atsAttribute.
#' 
#' @title Get the normalization parameters of a time series
#' @param obj the \code{\link{aTS}} object
#' @param ... additional function parameters (currently not used)
#' @export
getNormParams <- function(obj, ...) UseMethod("getNormParams")


#' @rdname getNormParams
#' @S3method getNormParams aTS
#' @method getNormParams aTS
getNormParams.aTS <- function(obj, ...) {
  aTSattr(obj, "normParams")
}

