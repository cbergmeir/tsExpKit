#' Save an aTS time series object to a file in KEEL format
#'  
#' @param aTS the aTS object to save
#' @param filename complete path to the file to write
#' @param useColNames should colnames present in the aTS object be used for attribute naming?
#' @export
writeATSToKeelFile <- function(aTS, filename, useColNames=FALSE) {
  
  timeStamps <- getTimeStamps(aTS)
  data <- coredata(aTS)
  minData <- min(data)
  maxData <- max(data)
  
  if(is.matrix(data)) multiCol <- TRUE
  else multiCol <- FALSE
  
  relation <- getName(aTS)
  
  write(sprintf("@relation %s", relation), filename, ncolumns= 1)

  comment <- getComment(aTS)
  if(!is.null(comment)) 
    write(paste("%", comment, sep=""), filename, ncolumns= 1, append=TRUE)
  
  write(paste("@attribute TimeStamp real [",min(timeStamps),", ", max(timeStamps), "]", sep="")
      , filename, ncolumns= 1, append=TRUE)
  
  if(multiCol) {
    if(useColNames) {
      aNames <- colnames(data)
    } else {
      aNames <- paste("Value", 1:ncol(data), sep="")
    }
  } else {
    aNames <- "Target"
  }
  
  for(attr in aNames)  
    write(paste("@attribute ",attr," real [",minData,", ", maxData, "]", sep=""), filename, ncolumns= 1, append=TRUE)
  
  if(multiCol) {
    write(sprintf("@inputs %s", paste(aNames[1:(length(aNames)-1)], sep="", collapse=", ")), filename, ncolumns= 1, append=TRUE)
    write(sprintf("@outputs %s", aNames[length(aNames)]), filename, ncolumns= 1, append=TRUE)    
  } 

  write("@data", filename, ncolumns= 1, append=TRUE)  
  
  write.table(cbind(timeStamps, as.matrix(data)), filename, quote=FALSE, sep=", ", row.names=FALSE, col.names=FALSE, append=TRUE)
}


#' Write a list of aTS objects to a directory, in KEEL format
#' 
#' @param atsList the atsList to write out
#' @param path the path to write the data to
#' @param useColNames should the column names be used?
#' @param compression the compression to use. Possible values are \code{zipSingle}, \code{zipPackage}, and \code{none}
#' @param compFilename the file name of the compressed output file
#' @export
writeATSListToKeelDirectory <- function(atsList, path, useColNames=FALSE, compression="none", compFilename="series.zip") {
  
  filenames <- list()
  
  for(name in names(atsList))  {
    
    relName <- name
    if(endsWith(relName,".dat") || endsWith(relName,".DAT")) {
      relName <- removeFileEnding(relName, ".dat")
    }

    filename <- paste(relName, ".dat", sep="")
    filePath <- assemblePathName(path, filename)
    
    writeATSToKeelFile(atsList[[name]], filePath, useColNames=useColNames)
    
    if(compression=="zipSingle") {
      
      currWorkDir <- getwd()
      setwd(path)
      try(system(paste("zip \"", relName, ".zip\" \"", relName, ".dat\"", sep=""), intern = TRUE))
      setwd(currWorkDir)
      unlink(filePath)
      
    } else {
      filenames[[relName]] <- filename
    }
      
  }
  
  if(compression=="zipPackage") {
    
    if(!endsWith(compFilename, ".zip")) compFilename <- paste(compFilename, ".zip", sep="")
    
    currWorkDir <- getwd()
    setwd(path)
    try(system(paste("zip \"", compFilename, "\" \"", paste(filenames, collapse="\" \""), "\"", sep=""), intern = TRUE))
    #print(paste("zip \"", compFilename, "\" \"", paste(filenames, collapse="\" \""), "\"", sep=""), intern = TRUE)
    
    setwd(currWorkDir)
    lapply(filenames, function(filename) {
          unlink(assemblePathName(path, filename))      
        })
    
    
  }
  
#  if(tarfile) {
#    tarFile <- paste(name, "-embd.tgz", sep="")
#    currWorkDir <- getwd()
#    setwd(path)
#    tar(tarFile, files=paste(name, "-embd", sep=""), compression="gzip")
#    setwd(currWorkDir)
#    ##delete the original directory..
#    ##this should be used with caution, as a wrong path here could mess things really up..
#    #unlink(evalPath, recursive=TRUE)
#  }

}

#' Load a directory of KEEL dat files into an ATSList
#' 
#' @param path the path of the directory with the dat files
#' @export 
loadKeelDatDirToATSList <- function(path) {
  loadDataDirToATSList(path, pattern="+\\.dat", fileReadFunction=loadKeelFile)
}

#' Write out an XML description for use with KEEL
#' 
#' @param atsList the atsList to use in KEEL
#' @param path the directory where to save the xml file
#' @param filename the filename of the xml file
#' @export 
writeKeelXmlDescription <- function(atsList, path, filename) {
  
  if(!endsWith(filename, ".xml")) filename <- paste(filename, ".xml", sep="")
  
  filePath <- assemblePathName(path, filename)
  
  write('<?xml version="1.0" encoding="UTF-8"?>', filePath, ncolumns= 1)
  write("<dataset_list>", filePath, ncolumns= 1, append=TRUE)
  
  for(tsName in names(atsList)) {
    
    write("<dataset>", filePath, ncolumns= 1, append=TRUE)
    write(paste("<nameAbr>", tsName, "</nameAbr>",sep=""), filePath, ncolumns= 1, append=TRUE)
    write(paste("<nameComplete>", tsName, "</nameComplete>",sep=""), filePath, ncolumns= 1, append=TRUE)
    write("<problemType>Regression</problemType>", filePath, ncolumns= 1, append=TRUE)
    write("<partitions>", filePath, ncolumns= 1, append=TRUE)
    write("<partition>5cfv</partition>", filePath, ncolumns= 1, append=TRUE)
    write("</partitions>", filePath, ncolumns= 1, append=TRUE)
    
    write("<continuous>Yes</continuous>", filePath, ncolumns= 1, append=TRUE)
    write("<integer>No</integer>", filePath, ncolumns= 1, append=TRUE)
    write("<nominal>No</nominal>", filePath, ncolumns= 1, append=TRUE)
    write("<imprecise>No</imprecise>", filePath, ncolumns= 1, append=TRUE)
    write("<missing>No</missing>", filePath, ncolumns= 1, append=TRUE)
    write("<multiclass>Yes</multiclass>", filePath, ncolumns= 1, append=TRUE)
    write("<multioutput>No</multioutput>", filePath, ncolumns= 1, append=TRUE)
    
    write("<percMissingValues>0</percMissingValues>", filePath, ncolumns= 1, append=TRUE)
    write(paste("<nAttributes>",getnValues(atsList[[tsName]]),"</nAttributes>",sep=""), filePath, ncolumns= 1, append=TRUE)
    write(paste("<nInstances>",nSamples(atsList[[tsName]]),"</nInstances>",sep=""), filePath, ncolumns= 1, append=TRUE)
    write("<field>Real</field>", filePath, ncolumns= 1, append=TRUE)
    write("<multiinstance>No</multiinstance>", filePath, ncolumns= 1, append=TRUE)
    write("</dataset>", filePath, ncolumns= 1, append=TRUE)
  }
  
  write("</dataset_list>", filePath, ncolumns= 1, append=TRUE)
  
}

#' Load all files in an ATSDir and save it to a directory of KEEL dat files
#' 
#' @param inPath the input path (ATSDir)
#' @param outPath the output path, where to save the dat files
#' @export
toKeelATSDir <- function(inPath, outPath=NULL){
  
  if(missing(outPath)) {
    outPath <- inPath
  }
  
  outPath <- assemblePathName(outPath, "toKEEL")
  
  atsList <- loadATSDir(inPath, pattern="\\.RData")
  dir.create(outPath)
  writeATSListToKeelDirectory(atsList, outPath, useColNames=TRUE)
  outPath
}

#' Function to export from a time series repository to a format that is used on the department homepage
#' 
#' @param dataPackagePath path of the data package
#' @param dataPackageName name of the data package
#' @param wholeEmbeddedSeriesPath path of the embedded series (unpartitioned)
#' @param partitionsPath path of the partitioned series
#' @param partSuffix a string to append to the filenames
#' @param targetPath the output path
#' @export
exportToKEELDatasetHomepageDir <- function(dataPackagePath, dataPackageName, wholeEmbeddedSeriesPath, partitionsPath, partSuffix="5-fold", targetPath=NULL) {
  
  if(missing(targetPath)) {
    targetPath <- dataPackagePath
  }
  
  targetPath <- assemblePathName(targetPath, paste(dataPackageName, "_toKEELdataset", sep=""))
  
  origSeriesPath <- assemblePathName(dataPackagePath, dataPackageName)
  origSeries <- loadATSDir(origSeriesPath, pattern="\\.RData")
  wholeEmbeddedSeries <- loadATSDir(wholeEmbeddedSeriesPath, pattern="\\.RData")
  partitions <- loadATSDir(partitionsPath, pattern="\\.RData")
  
  groupedPartitions <- groupATSListByPartitions(partitions)
  
  visPath <- assemblePathName(targetPath, "vis")
  dir.create(visPath, showWarnings=FALSE, recursive=TRUE)
  
  savePlotsATSList(origSeries, visPath, acf=TRUE, toSeparateJpegs=TRUE)
  
  modOrigSeries <- list()
  for(tsName in names(origSeries)) {
    newName <- paste(tsName, "-orig", sep="")
    modOrigSeries[[newName]] <- setName(origSeries[[tsName]], newName)
  }
  writeATSListToKeelDirectory(modOrigSeries, targetPath, useColNames=TRUE, compression="zipSingle")
  
  writeATSListToKeelDirectory(wholeEmbeddedSeries, targetPath, useColNames=TRUE, compression="zipSingle")
  writeKeelXmlDescription(wholeEmbeddedSeries, targetPath, paste("_", dataPackageName, "_keelDatasets.xml", sep=""))
  
  for(partTsName in names(groupedPartitions))
    writeATSListToKeelDirectory(groupedPartitions[[partTsName]], targetPath, useColNames=TRUE, 
        compression="zipPackage", compFilename=paste(partTsName, "-", partSuffix, ".zip", sep=""))
  
  targetPath
}

#' Function to initially import an ATSList into a time series repository
#' 
#' @param atsList the time series list
#' @param dataPath path of the data package
#' @param dataPackageName name of the data package
#' @param partSuffix a string to append to the filenames
#' @param embeddingParameters parameters for the embedding
#' @param partitionParametersKeelExp parameters for partitioning in the export to KEEL
#' @param partitionParametersNorm parameters for partitioning in the versions of the series that are normalized
#' @param normParameters parameters of the normalization
#' @param genDescStatTab Should the descriptive statistics table be generated?
#' @param genStatNonlinTab Should the table of the results of the linearity tests be generated? (This may be slow)
#' @param genStatOtherTestsTab Should the table with the other tests be generated? (This also may be slow)
#' @param minLength minimal length of the series to be considered
#' @export 
importATSListIntoDataRepository <- function(atsList, dataPath, dataPackageName, partSuffix="5-fold", embeddingParameters = list(lags=c(-4,-3,-2,-1,0)),
    partitionParametersKeelExp = list(type=c("blockedCV"), order=0, numPartitions=5, ratioValSet=0), 
    partitionParametersNorm = list(type=c("lastBlock"), order=0, ratioLB=0.2, ratioValSet=0),
    normParameters = list(), genDescStatTab=TRUE, genStatNonlinTab=TRUE, genStatOtherTestsTab=FALSE, minLength=10) {
    
  dataPath <- assemblePathName(dataPath, dataPackageName)
  dir.create(dataPath, showWarnings=FALSE)
  
  dataPackagePath <- saveAsDataPackageATSList(atsList, dataPath, dataPackageName, genSummaryPlot=TRUE, 
      genDescStatTab=genDescStatTab, genStatNonlinTab=genStatNonlinTab, genStatOtherTestsTab=genStatOtherTestsTab, 
      genSingeFilesDir=TRUE, minLength=minLength)
  
  if(!is.null(partitionParametersKeelExp)) {
    wholeSeriesEmbedDir <- embedATSDir(dataPackagePath, embeddingParameters=embeddingParameters)  
    bCVDir <- partitionATSDir(dataPackagePath, partitionParameters = partitionParametersKeelExp, savePlots=TRUE)
    bCVEmbedDir <- embedATSDir(bCVDir, embeddingParameters=embeddingParameters)
    exportToKEELDatasetHomepageDir(dataPath, dataPackageName, wholeSeriesEmbedDir, bCVEmbedDir, partSuffix=partSuffix)
  }

  if(!is.null(partitionParametersNorm)) {
    lBDir <- partitionATSDir(dataPackagePath, partitionParameters = partitionParametersNorm, savePlots=TRUE)
    #normLBDir <- lBDir
    normLBDir <- normalizeATSDir(lBDir, normParameters=normParameters)
    embedNormLBDir <- embedATSDir(normLBDir, embeddingParameters=embeddingParameters)      
    toKeelATSDir(embedNormLBDir)    
  }
  
}
