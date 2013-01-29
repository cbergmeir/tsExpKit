#' Use OpenOffice to generate a dif file (without any header, ASCII encoding).
#' Then, use this function to load it to an ATSList
#'  
#' TODO: Before using this function, it should be revised 
#' and updated (e.g. to use \code{\link{assemblePathName}}).
#' 
#' @title Load xls files into an aTS list
#' @param path the path to use
#' @param base the filename (without extension)
#' @return an \code{\link{ATSList}} 
#' @export
# @example convertDifToDat("/home/bergmeir/datasets", "NNGC1_dataset_E1_V1")
loadDifToATSList <- function(path, base)  {
	
	atsList <- list()
	
	pathBase <- sprintf("%s/%s", path, base)
	
	t <-read.DIF(sprintf("%s.dif", pathBase))
	
	for(i in 1:ncol(t))  {
		d <- t[which(!is.na(t[,i])),i]
		
#    filename <- sprintf("%s_%03d.dat", pathBase,i)
#    
#    write(sprintf("@relation %s_%03d", base, i), filename, ncolumns= 1)
#    write("@attribute attribute1 real", filename, ncolumns= 1, append=TRUE)
#    write("@data", filename, ncolumns= 1, append=TRUE)
#    write(d, filename, ncolumns= 1, append=TRUE)
		
		name <- sprintf("%s_%03d", base,i)
		
		atsList[[name]] <- aTS(d, name=name)
	}
	
}

#' @title Load mat files into an aTS list
#' @param filename the file to read in
#' @param filenameIndices of the positive instances
#' @return an \code{\link{ATSList}} 
#' @export
#' @author Mabel
# @example loadMatToATSList("/home/bergmeir/datasets", "NNGC1_dataset_E1_V1")
loadMatToATSList <- function(filename, filenameIndices=NULL)  {
	
	require(R.matlab)
	require(tsExpKit)
	
	#filename <- "/home/ts/datasets/repos/public/semi-supervised/ECG106_training.mat"
	#filename <- assemblePathName(path, base)
	
	atsList <- list()
	
	list.data <- readMat(filename)
	
	m <- list.data[[1]]
	# ahora el objeto m es de tipo matrix
	
	#list.data
	
	if(!is.null(filenameIndices)) {
		
		#	filenameIndices <- "/home/ts/datasets/repos/public/semi-supervised/ECG_Training_Pos_Index.mat"
		list.index <- readMat(filenameIndices)
		li <- list.index[[1]]
		li <- li[1,]
	}
	
	
	for (i in 1:nrow(m)){
		atsList[[i]] <- aTS(m[i,])
		if (!is.null(filenameIndices) && (i %in% li)){ # es una instancia positiva
			
			aTSattr(atsList[[i]], "label") <- "Pos"
			
		}
	}
	
	atsList
	
}
#' This function calls \code{\link{loadDataDirToATSList}} with the parameters \code{pattern="+\\.dat"} 
#' and \code{fileReadFunction=scan}.
#'  
#' @title Load a directory of ".dat" files into an \code{\link{ATSList}}
#' @param path the path of the directory
#' @export
loadDatDirToATSList <- function(path) {
  loadDataDirToATSList(path, pattern="+\\.dat", fileReadFunction=scan)
}

#' This function compiles a list of filenames according to \code{path} and \code{pattern}, 
#' and then uses the \code{fileReadFunction} to read the contents of the files. Then, \code{\link{aTS}}
#' objects are constructed from the file contents and added to an \code{\link{ATSList}}.  
#'  
#' @title Load a directory of files into an \code{\link{ATSList}}
#' @param path the path of the directory
#' @param pattern a pattern, to e.g. choose the file ending of the files
#' @param fileReadFunction the function that is to be used to read single files
#' @usage loadDataDirToATSList(path, pattern="+\\\\.dat", fileReadFunction=scan)
#' @return an \code{\link{ATSList}} containing all the time series that were read
#' @export
loadDataDirToATSList <- function(path, pattern="+\\.dat", fileReadFunction=scan) {
  
  filenames <- list.files(path, pattern=pattern)
  
  atsList <- list()
  
  for(file in filenames)  {
    
    data <- fileReadFunction(assemblePathName(path, file))
    name <- getFilenameWithoutDatEnding(file)
    
    comment <- NULL
    if(!is.null(attr(data, "comment"))) comment <- attr(data, "comment")
    atsList[[name]] <- aTS(data, name=name, comment=comment)
  }
  
  atsList  
}

#' This function loads an ATS directory into an \code{\link{ATSList}}.  
#'  
#' @title Load an ATS directory into an \code{\link{ATSList}}
#' @param path the path of the directory
#' @param pattern a pattern, to e.g. choose the file ending of the files
#' @param ... currently not used
#' @usage loadATSDir(path, pattern="+\\\\.RData", ...)
#' @return an \code{\link{ATSList}} containing all the time series that were read
#' @export
loadATSDir <- function(path, pattern="+\\.RData", ...) {
  
  atsList <- list()
  
  files <- list.files(path,  pattern=pattern)
  
  for(file in files) {
    load(assemblePathName(path, file))
    
    file <- removeFileEnding(file, ".RData")        
    atsList[[file]] <- get(file)
  }
  
  atsList
}

#' This function saves an \code{\link{ATSList}} to an ATS directory.  
#'  
#' @title Save an \code{\link{ATSList}} to an ATS directory
#' @param atsList the \code{\link{ATSList}} to save to disk 
#' @param dataPath the path to which to save to
#' @param name the name under which to save the list
#' @param singleFiles if \code{TRUE}, every series is saved as a single file, using \code{\link{save.aTS}}. 
#' Otherwise, all series are saved in one file, using the normal \code{save} function.
#' @export
# @example
#dataPath <- "/home/bergmeir/datasets/"
#name <- "wmtsaTimeSeries"
#ts <- loadDatDirToATSList(assemblePathName(dataPath, name))
#saveATSList(ts, dataPath, name)
saveATSList <- function(atsList, dataPath, name, singleFiles=FALSE) {
  
  if(singleFiles) {
    lapply(atsList, function(ats) {
          save.aTS(ats, dataPath)
        })  
  } else {
    path <- assemblePathName(dataPath, name)
    assign(name, atsList)
    save(list=name, file = paste(path, ".RData", sep=""))    
  }
  
#  plotSummary=TRUE
#  if(plotSummary)
#    savePlotsATSList(get(name), paste(path, ".pdf", sep=""), acf=TRUE)
  
}

#' This function saves plots of all time series in an \code{\link{ATSList}} to disk.  
#'  
#' @title Save plots of every time series in an \code{\link{ATSList}} to files
#' @param atsList the \code{\link{ATSList}} to use 
#' @param filename the filename or the prefix for the filename
#' @param acf if \code{TRUE}, the acf plot is generated and saved along with the plot of the series
#' @param toSeparateJpegs if \code{TRUE}, every time series plot is saved as a separate jpeg file. Otherwise, 
#' a single ".pdf" file is generated. 
#' @export
savePlotsATSList <- function(atsList, filename, acf=TRUE, toSeparateJpegs=FALSE) {
  
  if(toSeparateJpegs) {
    
    for (atsName in names(atsList))  {
      
      if(acf) rows <- 2
      else rows <- 1
      
      jpgFile <- assemblePathName(filename , paste(atsName, ".jpg", sep=""))
      jpeg(filename=jpgFile,width=800,height=200*rows)
      
      par(mfrow=c(rows,1))
      plot(atsList[[atsName]],type="l", main=atsName, xlab="Time", ylab="Value")
      if(acf) acf(atsList[[atsName]], main="")
      
      dev.off()
    }
    
  } else {
    
    rows <- 1 #length(names(atsList))
    if(acf) rows <- 2*rows
    
    pdf(file=filename,onefile=TRUE,width=20,height=3*rows)
    par(mfrow=c(rows,1))
    
    for (i in names(atsList))  {
      
      #i <- names(exampleSeries)[1]
      plot(atsList[[i]],type="l", xlab=i, ylab="")
      if(acf) acf(atsList[[i]])
    }
    dev.off()    
  }
}

#' This function automatizes the import of an \code{\link{ATSList}} to a data repository as a new data package, 
#' by calling sequentially \code{\link{saveATSList}} with single-file and all-in-one-file mode, \code{\link{savePlotsATSList}}, and \code{\link{summaryTableATSList}} 
#' with the according types.  
#'  
#' @title Save an \code{\link{ATSList}} as a (new) data package
#' @param atsList the \code{\link{ATSList}} to use 
#' @param dataPath the path to which to save the data package to
#' @param dataPackageName the name for the data package
#' @param genSummaryPlot if \code{TRUE}, \code{\link{savePlotsATSList}} is used to generate one pdf containing all series.
#' @param genDescStatTab if \code{TRUE}, the "DescStat" table is generated
#' @param genStatNonlinTab if \code{TRUE}, the "StatNonlin" table is generated
#' @param genStatOtherTestsTab if \code{TRUE}, the "OtherTests" table is generated
#' @param genSingeFilesDir if \code{TRUE}, the series are also saved in single-file mode to disk
#' @param minLength series below this length will be omitted. For some of the measures calculated for the characteristics tables,
#' a certain minimal length of the series might be necessary.
#' @export
saveAsDataPackageATSList <- function(atsList, dataPath, dataPackageName, genSummaryPlot=TRUE, 
    genDescStatTab=TRUE, genStatNonlinTab=FALSE, genStatOtherTestsTab=FALSE, genSingeFilesDir=TRUE, minLength=10) {
  
  dataPackagePath <- assemblePathName(dataPath, dataPackageName)  
  
  saveATSList(atsList, dataPath, dataPackageName, singleFiles=FALSE)
  
  if(genSummaryPlot)
    savePlotsATSList(atsList, paste(dataPackagePath, ".pdf", sep=""), acf=TRUE)
  
  if(genDescStatTab) {
    sumTabDescStat <- summaryTableATSList(atsList, type="DescStat", order=5)
    write.csv(sumTabDescStat, file = paste(dataPackagePath, "_descStat.csv", sep=""))    
  }
  
  if(genStatNonlinTab) {
    sumTabStatNonlin <- summaryTableATSList(atsList, type="StatNonlin", order=5)
    write.csv(sumTabStatNonlin, file = paste(dataPackagePath, "_statNonlin.csv", sep=""))    
  }
  
  if(genStatOtherTestsTab) {
    sumTabOtherTests <- summaryTableATSList(atsList, type="OtherTests", order=5, alpha=0.05)
    write.csv(sumTabOtherTests, file = paste(dataPackagePath, "_otherTests.csv", sep=""))    
  }
  
  for(serName in names(atsList)) {
    if(length(atsList[[serName]]) < minLength) atsList[[serName]] <- NULL
  }
  
  #save original series as single files
  if(genSingeFilesDir) {    
    dir.create(dataPackagePath)      
    saveATSList(atsList, dataPackagePath, NULL, singleFiles=TRUE)    
  }
  dataPackagePath
}
