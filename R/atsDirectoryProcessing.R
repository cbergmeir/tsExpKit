#' The function loads all files in an ATS directory into an ATSList, 
#' applies \code{\link{embedATSList}}, and saves the result to an ATS directory.
#' 
#' @title Embed all time series given in an ATS directory 
#' @param inPath the input path, i.e., the ATS directory
#' @param embeddingParameters a list giving all parameters that are passed to \code{\link{embedATSList}}
#' @param outPath the output path. If \code{NULL}, it is generated automatically as a subdirectory of the \code{inPath}
#' @seealso \code{\link{partitionATSDir}}, \code{\link{normalizeATSDir}}
#' @export 
embedATSDir <- function(inPath, embeddingParameters=list(lags=c(-4,-3,-2,-1,0)), outPath=NULL) {

  atsList <- loadATSDir(inPath, pattern="+\\.RData")
  
  if(missing(outPath)) {
    outPath <- inPath
  }  

  shortNameEmbed <- paste("emb-",computeOrder(embeddingParameters$lags), sep="")
  outPath <- assemblePathName(outPath, shortNameEmbed)  
  
  etsList <- do.call(embedATSList, append(list(atsList), embeddingParameters))
  
  dir.create(outPath)
  saveATSList(etsList, outPath, NULL, singleFiles=TRUE)
  outPath
}

#' The function loads all files in an ATS directory into an ATSList, 
#' applies \code{\link{computePartitions}}, and saves the result to an ATS directory.
#' 
#' @title Partition (to training and test sets) all time series in an ATS directory
#' @param inPath the input path, i.e., the ATS directory
#' @param partitionParameters a list giving all parameters that are passed to \code{\link{computePartitions}}
#' @param outPath the output path. If \code{NULL}, it is generated automatically as a subdirectory of the \code{inPath}
#' @param savePlots if \code{TRUE}, a directory containing plots of all partitions is generated
#' @seealso \code{\link{embedATSDir}}, \code{\link{normalizeATSDir}}
#' @export
partitionATSDir <- function(inPath, 
    partitionParameters=list(type=c("blockedCV"), order=0, numPartitions=5, ratioValSet=0.2),
    outPath=NULL, savePlots=TRUE) {

  atsList <- loadATSDir(inPath, pattern="+\\.RData")
  
  if(missing(outPath)) {
    outPath <- inPath
  }  
  
  outPath <- assemblePathName(outPath, "modSelProc")
  dir.create(outPath, showWarnings = FALSE)
  
  firstRunGenDir <- TRUE
  for(atsName in names(atsList)) {
    
    ats <- atsList[[atsName]]
    
    partitions <- do.call(computePartitions, append(list(ats), partitionParameters))
    
    if(firstRunGenDir) {
      resPartitionParameters <- attr(partitions, "partitionParameters")
      outPath <- assemblePathName(outPath, resPartitionParameters$shortname)      
      dir.create(outPath, showWarnings = FALSE)
      firstRunGenDir <- FALSE
    }
      
    savePartitions(partitions, outPath)
    
    if(savePlots) {
      visPath <- assemblePathName(outPath, "vis")
      dir.create(visPath, showWarnings=FALSE)
      plotPartitionsATS(ats, partitions, visPath)
    }
      
    
  }
  outPath  
}

#' The function loads all files in an ATS directory into an ATSList, 
#' applies \code{\link{normalize}}, and saves the result to an ATS directory.
#' If the ATS directory contains training and test sets, the test sets are normalized
#' using the parameters of the corresponding training sets.
#' 
#' @title Normalize all time series in an ATS directory
#' @param inPath the input path, i.e., the ATS directory
#' @param normParameters a list giving all parameters that are passed to \code{\link{normalize}}
#' @param outPath the output path. If \code{NULL}, it is generated automatically as a subdirectory of the \code{inPath}
#' @seealso \code{\link{partitionATSDir}}, \code{\link{embedATSDir}}
#' @export
normalizeATSDir <- function(inPath, normParameters=list(), outPath=NULL) {

  if(missing(outPath)) {
    outPath <- inPath
  }

  outPath <- assemblePathName(outPath, "norm")
  dir.create(outPath)
  
  useTestData <- TRUE
  
  trainPartitions <- loadATSDir(inPath, pattern="+tra\\.RData")
  testPartitions <- loadATSDir(inPath, pattern="+tst\\.RData")
  
  if(length(testPartitions) == 0) {
    useTestData <- FALSE
    trainPartitions <- loadATSDir(inPath, pattern="+\\.RData")
  }
    
  #normalize training partitions
  normTrainPartitions <- lapply(trainPartitions, normalize)
  
  saveATSList(normTrainPartitions, outPath, name=NULL, singleFiles=TRUE)
  
  if(useTestData) {
    #normalize corresponding test partitions with values of train partitions
    normTestPartitions <- list()
    for(currPartName in names(normTrainPartitions)){
      currTestPart <- testPartitions[[paste(removeFileEnding(currPartName, "tra"), "tst", sep="")]]
      currNormTestPart <- normalize(currTestPart, normParams=getNormParams(normTrainPartitions[[currPartName]]))
      
      normTestPartitions[[getName(currTestPart)]] <- currNormTestPart
      
    }
    
    saveATSList(normTestPartitions, outPath, name=NULL, singleFiles=TRUE)    
  }
  
  outPath
}

## @export
#preprocAndSaveATSList <- function(atsList, dataPath, dataPackageName,
#    partitionParameters=list(type=c("blockedCV"), order=0, numPartitions=5, ratioValSet=0.2), 
#    embeddingParameters=list(lags=c(-4,-3,-2,-1,0)), normParameters=list()) {
#  
##  dataPackagePath <- assemblePathName(dataPath, dataPackageName)  
##  dir.create(dataPackagePath)
##  
##  #dataPackagePathWithName <- assemblePathName(dataPath, dataPackageName)
##  saveATSList(atsList, dataPath, dataPackageName, singleFiles=FALSE)
##  savePlotsATSList(atsList, paste(dataPackagePath, ".pdf", sep=""), acf=TRUE)
##  
##  sumTabDescStat <- summaryTableATSList(atsList, type="DescStat", order=5)
##  write.csv(sumTabDescStat, file = paste(dataPackagePath, "_descStat.csv", sep=""))
##  
#  ##  sumTabStatNonlin <- summaryTableATSList(atsList, type="StatNonlin", order=5)
#  ##  write.csv(sumTabStatNonlin, file = paste(dataPackagePath, "_statNonlin.csv", sep=""))
#  ##  
#  ##  sumTabOtherTests <- summaryTableATSList(atsList, type="OtherTests", order=5, alpha=0.05)
#  ##  write.csv(sumTabOtherTests, file = paste(dataPackagePath, "_otherTests.csv", sep=""))
##  
##  #save original series as single files  
##  saveATSList(atsList, dataPackagePath, NULL, singleFiles=TRUE)
#  
##  shortNameEmbed <- paste("emb-",computeOrder(embeddingParameters$lags), sep="")
##  
##  #save embedded full series
##  etsList <- do.call(embedATSList, append(list(atsList), embeddingParameters))
##  atsPathNameEmb <- assemblePathName(dataPackagePath, shortNameEmbed)
##  dir.create(atsPathNameEmb)
##  saveATSList(etsList, atsPathNameEmb, NULL, singleFiles=TRUE)
#  
#  
##  for(atsName in names(atsList)) {
##    
##    ats <- atsList[[atsName]]
##    
##    atsPathName <- dataPackagePath
##    
##    #generate and save partitions and plots of these
##    modSelTypes <- partitionParameters$types
##    
##    atsAllModSelPathName <- assemblePathName(atsPathName, "modSelProc")
##    dir.create(atsAllModSelPathName, showWarnings = FALSE)
##    #partitions <- computePartitions(ats, types=modSelTypes, order=0, numPartitions=5, ratioValSet=0.2)
##    partitions <- do.call(computePartitions, append(list(ats), partitionParameters))
##    
##    resPartitionParameters <- attr(partitions, "partitionParameters")
##    modSelPathName <- assemblePathName(atsAllModSelPathName, resPartitionParameters$shortname)
##    dir.create(modSelPathName, showWarnings = FALSE)
##    
##    savePartitions(partitions, modSelPathName)
##    plotPartitionsATS(ats, partitions, modSelPathName)
##    
##  }
#  
#  #    subDirs <- list.dirs(atsAllModSelPathName)
#  
#  #-------
#  #embed
#  modSelPartitions <- loadATSDir(modSelPathName)
#  embeddedPartitions <- do.call(embedATSList, append(list(modSelPartitions), embeddingParameters))
#  
#  embeddedPartitionsPathName <- assemblePathName(modSelPathName, shortNameEmbed)
#  dir.create(embeddedPartitionsPathName)
#  saveATSList(embeddedPartitions, embeddedPartitionsPathName, name=NULL, singleFiles=TRUE)
#  
#  #plot embedded partitions (not working so far)
#  #lapply(atsList, plotPartitionsFromFiles, embeddedPartitionsPathName)
#  
#  #----------------------
#  #normalize data
#  
#  trainPartitions <- loadATSDir(embeddedPartitionsPathName, pattern="+tra\\.RData")
#  testPartitions <- loadATSDir(embeddedPartitionsPathName, pattern="+tst\\.RData")
#  
#  #normalize training partitions
#  normTrainPartitions <- lapply(trainPartitions, normalize)
#  
#  #normalize corresponding test partitions with values of train partitions
#  normTestPartitions <- list()
#  for(currPartName in names(normTrainPartitions)){
#    currTestPart <- testPartitions[[paste(removeFileEnding(currPartName, "tra"), "tst", sep="")]]
#    currNormTestPart <- normalize(currTestPart, normParamsValues=getNormParamsValues(normTrainPartitions[[currPartName]]), 
#        normParamsTargets=getNormParamsTargets(normTrainPartitions[[currPartName]]))
#    
#    normTestPartitions[[getName(currTestPart)]] <- currNormTestPart
#    
#  }
#  
#  normEmbeddedPartitionsPathName <- assemblePathName(embeddedPartitionsPathName, "norm")
#  
#  dir.create(normEmbeddedPartitionsPathName)
#  saveATSList(normTrainPartitions, normEmbeddedPartitionsPathName, name=NULL, singleFiles=TRUE)
#  saveATSList(normTestPartitions, normEmbeddedPartitionsPathName, name=NULL, singleFiles=TRUE)
#  
#}

#plotPartitionsFromFiles <- function(aTS, partDir) {
#
#  trainPartitions <- loadATSDir(partDir, pattern=paste(getName(aTS),"+tra\\.RData",sep=""))
#  testPartitions <- loadATSDir(partDir, pattern=paste(getName(aTS),"+tst\\.RData",sep=""))
#  
#  pdf(file=assemblePathName(partDir, paste(getName(aTS), "-partitions.pdf", sep="")),onefile=TRUE,width=20,height=3*length(trainPartitions)) 
#  par(mfrow=c(length(trainPartitions),1))
#  
#  for(currPartName in names(trainPartitions)) {
#    currTestPart <- testPartitions[[paste(removeFileEnding(currPartName, "tra"), "tst", sep="")]]
#    
#    #plot the partitions to a pdf file..
#    plot(aTS)
#    points(trainPartitions[[currPartName]], col="green")
#    points(currTestPart, col="red")
#  }  
#  
#  dev.off()
#  
#}