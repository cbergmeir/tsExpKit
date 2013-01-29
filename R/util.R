#' Determines, of the string \code{myString} ends with the string \code{mySubString}. 
#' The function is useful, e.g., when determining the file ending of a filename.
#' 
#' @title Utility function to find ending of a string 
#' @param myString string to determine ending
#' @param mySubString string to compare to
#' @return \code{TRUE}, if \code{myString} ends with \code{mySubString}, FALSE otherwise
#' @export
endsWith <- function(myString, mySubString) {
  
  l1 <- nchar(myString)
  l2 <- nchar(mySubString)
  if(l1 < l2) return(FALSE)
  s <- substr(myString, l1-l2+1, l1)
  if(s == mySubString) return(TRUE)
  return(FALSE)
  
}


#' OBSOLETE, use system function \code{file.path} instead. 
#' 
#' Assembles two path names, e.g. a path and a filename. Automatically determines, if a backslash is needed. 
#' 
#' @title Utility function to assemble a path name from two path strings 
#' @param path1 string that represents first path
#' @param path2 string that represents second path
#' @return the assembled path name
#' @export
assemblePathName <- function(path1, path2) {
  file.path(path1, path2)
#  
#  if(endsWith(path1, "/")) 
#    path1 <- substr(path1,1,nchar(path1)-1)
#  
#  if(endsWith(path2, "/")) 
#    path2 <- substr(path2,1,nchar(path2)-1)
#  
#  return(paste(path1, "/", path2, sep=""))
}

#' This function is a convenience function that simply does: 
#' \code{substr(file, 0, nchar(file)-nchar(ending))}
#' 
#' @title Remove file ending from a path string
#' @param file a string 
#' @param ending a substring
#' @return the \code{file} string without the last 
#' characters. The amount of characters removed is 
#' determined by the length of \code{ending}.
#' @export
removeFileEnding <- function(file, ending) {
  substr(file, 0, nchar(file)-nchar(ending))
}

#' Remove the string ".dat" or ".DAT" from a filename
#' 
#' @title Remove the string ".dat" or ".DAT" from a filename
#' @param filename a string
#' @return the \code{filename} string, with 
#' ".dat" or ".DAT" removed from the end.
#' @export
getFilenameWithoutDatEnding <- function(filename) {
  
  relName <- filename
  if(endsWith(relName,".dat") || endsWith(relName,".DAT")) {
    relName <- substr(relName,1,nchar(relName)-4)
  }
  relName
}

#' This function splits the input string along slashes ("/") and 
#' returns everything after the last slash it finds. 
#' 
#' @title Get the filename from a full path 
#' @param path a string
#' @return returns a substring of \code{path}
#' @export
getFileNameFromPath <- function(path) {
  
  pathList <- unlist(strsplit(path, "/"))
  pathList[length(pathList)]
}

#' This function calls subsequently \code{\link{removeFileEnding}}, and \code{\link{getFileNameFromPath}}
#' 
#' @title Get the filename without file ending from a path
#' @param path a string
#' @param suffix a substring to remove from 
#' the end of \code{path}
#' @return a substring of \code{path}
#' @export
extractNameFromPath <- function(path, suffix) {
  tempPath <- removeFileEnding(path, suffix)
  getFileNameFromPath(tempPath)  
}

#' This function helps you giving your experiment runs meaningful names. It composes a name from the current data,
#' the name of the machine the experiments were run on, your user name, and a given comment
#' 
#' @title Assemble a path name for an experiment run
#' @param resultsComment a string that will be part of the directory name, naming the experiments
#' @param resultsPath the path where to generate the subdirectory for the results
#' @return a string which is a path of to the directory to be created for the experiments
#' @export
assembleExperimentsPath <- function(resultsComment, resultsPath="/home/ts/results/") {
  
  hostname <- try(system("hostname", intern=TRUE))
  username <- try(system("whoami", intern=TRUE))
  
  paste(resultsPath,format(Sys.time(), "%Y%m%d-%H-%M-%S-"), hostname, "-", username, "-", resultsComment, sep="")
}

#' This function calls \code{list.files}, and filters 
#' the result for directories
#' 
#' @title find all subdirectories in a directory
#' @param path the path to explore
#' @param pattern passed to \code{list.files}
#' @param all.dirs passed to \code{list.files}
#' @param ignore.case passed to \code{list.files}
#' @param recursive explore the directory recursively, passed to \code{list.files}
#' @author The code is taken from a post on stackoverflow:
#' 
#' http://stackoverflow.com/questions/4749783/how-to-obtain-a-list-of-directories-within-a-directory-like-list-files-but-in
#' @export
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE, ignore.case=FALSE, recursive=FALSE) {
  
  all <- list.files(path, pattern, all.dirs,
      full.names=TRUE, recursive=recursive, ignore.case, include.dirs=TRUE)
  all[file.info(all)$isdir]
}

#' This function finds all ".csv" files in a directory and 
#' joins them into one big file, using \code{rbind}.
#' 
#' @title Join all ".csv" files in a directory into one file 
#' @param path the directory where the ".csv" files are
#' @param pattern the file ending. This will usually be "\\.csv"
#' @param targetFilename the target file which contains all files joined together.
#' @usage joinCSVFiles(path, pattern="\\\\.csv", targetFilename) 
#' @export
joinCSVFiles <- function(path, pattern="\\.csv", targetFilename) {
  res <- NULL
  filenames <- list.files(path, pattern=pattern)
  
  for(filename in filenames) {
    res <- rbind(res, read.csv(assemblePathName(path, filename)))
  }
  
  write.csv(res, file=assemblePathName(path, targetFilename))
  #res
}


#' This function is very powerful and an important 
#' utility function of the package. 
#' This helper function implements a special case of \code{rbind}, 
#' where columns in both tables that have the same name are joined,
#' and columns that only exist in one of the two source tables
#' result in "NA" values for the rows in which they don't exist.
#' 
#' @title Row-bind together two tables, taking into account the column names 
#' @param x first table
#' @param y second table
#' @return the two tables rbind-ed together
#' @seealso \code{\link{cBindByRowNames}}
#' @export
rBindByColNames <- function(x,y) {
  
  if(is.null(ncol(x)) || ncol(x)==0) {
    res <- y
  } else if(is.null(ncol(y)) || ncol(y)==0) {
    res <- x
  } else {
    
    rX <- rownames(x)
    rY <- rownames(y)
    cX <- colnames(x)
    cY <- colnames(y)
    
    diffXY <- setdiff(cX, cY)
    diffYX <- setdiff(cY, cX)
    
    if(length(diffXY) != 0) {
      
      newColY <- seq(0,0,length=nrow(y))
      newColY[1:length(newColY)] <- NA
      
      for(i in diffXY)
        y <- cbind(y, newColY)
      
      colnames(y) <- c(cY, diffXY)
      
    }
    
    if(length(diffYX) != 0) {
      
      newColX <- seq(0,0,length=nrow(x))
      newColX[1:length(newColX)] <- NA
      
      for(i in diffYX)
        x <- cbind(x, newColX)
      
      colnames(x) <- c(cX, diffYX)      
    }
    
    x <- x[, match(union(cX,cY),colnames(x)), drop=FALSE]
    y <- y[, match(union(cX,cY),colnames(y)), drop=FALSE]
    
    res <- rbind(x,y)
    rownames(res) <- c(rX, rY)
  }
  res
}

#' The function is the pendant to \code{\link{rBindByColNames}}. 
#' See there for more detailed explanations.
#' 
#' @title Column-bind together two tables, taking into account the row names 
#' @param x first table
#' @param y second table
#' @return the two tables cbind-ed together
#' @seealso \code{\link{rBindByColNames}}
#' @export
cBindByRowNames <- function(x,y) {
  
  if(is.null(nrow(x)) || nrow(x)==0) {
    res <- y
  } else if(is.null(nrow(y)) || nrow(y)==0) {
    res <- x
  } else {
    
    rX <- rownames(x)
    rY <- rownames(y)
    cX <- colnames(x)
    cY <- colnames(y)
    
    diffXY <- setdiff(rX, rY)
    diffYX <- setdiff(rY, rX)
    
    if(length(diffXY) != 0) {
      
      newRowY <- seq(0,0,length=ncol(y))
      newRowY[1:length(newRowY)] <- NA
      
      for(i in diffXY)
        y <- rbind(y, newRowY)
      
      rownames(y) <- c(rY, diffXY)
      
    }
    
    if(length(diffYX) != 0) {
      
      newRowX <- seq(0,0,length=ncol(x))
      newRowX[1:length(newRowX)] <- NA
      
      for(i in diffYX)
        x <- rbind(x, newRowX)
      
      rownames(x) <- c(rX, diffYX)      
    }
    
    x <- x[match(union(rX,rY),rownames(x)), , drop=FALSE]
    y <- y[match(union(rX,rY),rownames(y)), , drop=FALSE]
    
    #x <- x[union(rX,rY), ]
    #y <- y[union(rX,rY), ]
    res <- cbind(x,y)
    colnames(res) <- c(cX, cY)
  }
  res
}


#' This function is a helper function to solve the following problem: 
#' You have two variables that you multiply, and you know for each variable
#' the interval of values it can take. Then, this function calculates the 
#' interval of values the product can take.
#' 
#' @title Determine new boundaries of a multiplied interval
#' @param minX the lower bound of the first interval
#' @param maxX the upper bound of the first interval
#' @param minY the lower bound of the second interval
#' @param maxY the upper bound of the second interval
#' @examples 
#' multiplyIntervalBoundaries(0, 25, -2, 4.5)
#' @export
multiplyIntervalBoundaries <- function(minX, maxX, minY, maxY) {
    
    a <- minX
    b <- maxX
    c <- minY
    d <- maxY
    
  if(a >= 0) {
    if(c >= 0) {
      e <- a * c
     f <- b * d
    } else if(d <= 0) {
      e <- b * c
      f <- a * d
    } else {
      e <- b * c
      f <- b * d
    } 
  } else if(b <= 0) {
    if(c >= 0) {
      e <- a * d
      f <- b * c
    } else if(d <= 0) {
      e <- b * d
      f <- a * c    
   } else {
      e <- a * d
      f <- a * c    
    } 
  } else {
    if(c >= 0) {
      e <- a * d
      f <- b * d
    } else if(d <= 0) {
      e <- b * c
      f <- a * c    
   } else {
      e <- min(a * d, b * c)
      f <- max(a * c, b * d)    
   } 
  } 
  return(c(min=e, max=f))
}
 
#' TODO: This function should be renamed to \code{firstColToRowNames}
#' 
#' @title Use the first column of a matrix to set its row names
#' @param x a matrix
#' @return the matrix, but with the first column removed and set to rownames
#' @export
firstColToColname <- function(x) {
  
  rownames(x) <- x[,1]
  x <- x[,2:ncol(x), drop=FALSE]
  x
}
