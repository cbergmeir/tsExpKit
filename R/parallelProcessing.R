#' This function returns default settings for parallelization, 
#' to be used with \code{\link{initParallelMode}}. The default 
#' is no parallelization. 
#' 
#' @title Get the default settings of parallelization config
#' @return the default settings
#' @seealso \code{\link{initParallelMode}}, \code{\link{getConfigHercules}}
#' @export
getConfigDefault <- function() {
  data.frame(default=c("default","none",1), row.names=c("hostname", "parallelMode", "maxJobs"), stringsAsFactors=FALSE)  
}

#' This function returns the current settings for parallelization for "hercules", 
#' our SGE cluster here at DiCITS. This function is intended to be used with 
#' \code{\link{initParallelMode}}.
#' 
#' @title Get the settings of parallelization for hercules
#' @return the parallelization settings for hercules
#' @seealso \code{\link{initParallelMode}}, \code{\link{getConfigDefault}}
#' @export
getConfigHercules <- function() {
  data.frame(hercules=c("hercules","sge",30), row.names=c("hostname", "parallelMode", "maxJobs"), stringsAsFactors=FALSE)  
}

# @export
#cleanUpParallelMode <- function() {
#  if(parallelMode == "snow")   stopCluster(cl)
#}

#' This function can be used to initialize and/or auto-determine the 
#' apply function to be used, in order to run parallel executions.
#' The function checks the hostname of the machine it is running on,
#' and then chooses from the config the apropriate apply function, and
#' initializes it if necessary. E.g., if running on hercules, the apply
#' function to be used is one from the \code{Rsge} package, to send parallel
#' jobs to hercules' SGE queue. If run on a multicore linux system, a version
#' of apply from the \code{multicore} package is used. For debugging, it is
#' often necessary to turn off parallel execution. 
#' 
#' TODO: Parallel execution using the
#' \code{snow} package is implemented but probably currently not working.
#' 
#' @title This function determines and initializes automatically the adequate
#' apply function for parallel processing 
#' @param config the known configurations. See \code{\link{getConfigDefault}} and \code{\link{getConfigHercules}}
#' for examples.
#' @param packagesToLoad the packages that have to be loaded for versions of applies that launch new R processes.
#' @param sgeQueueName for SGE: the name of the queue to use.
#' @param sgeRemoveFiles for SGE: should the temporary files be deleted?
#' @export
initParallelMode <- function(config, packagesToLoad=c("tsExpKit"), sgeQueueName="muylarga", sgeRemoveFiles=TRUE) {
  
  cl <- NULL
  
  hostname <- try(system("hostname", intern=TRUE))
  
  x <- which(config["hostname",] == hostname)
  if(length(x) == 0) x <- which(config["hostname",] == "default")
  
  parallelMode <- config["parallelMode",x]
  maxJobs <- config["maxJobs",x]
  
  #choose the apply functions
  tsApply <- lapply
  
  if(parallelMode=="multi") {
    
    require("multicore")
    
    options(cores=maxJobs)
    
    tsApply <- mclapply
    
  } else if(parallelMode=="snow") {
    
    #TODO: Snow is currently not used/tested.
    
    require("snow")
    
    cl <- makeCluster(maxJobs, type = "SOCK")
    
    # Here, the packagesToLoad has to be used, instead of the hard-coded parameter
    clusterEvalQ(cl, require(tsExpKit))
    
    mySnowApply <- function(...) {
      clusterApply(cl, ...)
    }
    
    tsApply <- mySnowApply
    
  } else if(parallelMode=="sge") {
    
    require("Rsge")
    
    sge.options(sge.save.global=TRUE)
    sge.options(sge.user.options=paste("-l qname=", sgeQueueName, " -S /bin/bash", sep=""))
    sge.options(sge.trace=TRUE)
    sge.options(sge.remove.files=sgeRemoveFiles)
    
	 
    mySgeApply <- function(...) {
      sge.parLapply(..., packages=packagesToLoad, batch.size=1)
      #sgeSubmitLApply(..., packages=c("tsExpKit"), maxJobs=maxJobs, sleep=5, trace=TRUE)
    }
    
    tsApply <- mySgeApply
  }
  
  tsApply
} 

# This function checks for finished jobs and sends new ones, so that always the maximal
# number of jobs is executed. As this all is handled directly by SGE, it is better to send
# all jobs directly, without using this function. Furthermore, when using this function,
# considerable load is generated on the main machine, which is apparently not a good thing.
# So, this function shouldn't be used.
# @export
sgeSubmitLApply <- function(X, FUN, ..., maxJobs=20, sleep=1, trace=TRUE) {
  
  require(Rsge)
  
  l1 <- list()
  r1 <- list()
  
  nJobs <- 0
  i <- 1
  
  while((length(r1) < length(X)) || !all(r1 == 0)) {
    
    if(nJobs < maxJobs && i <= length(X)) {
      while(nJobs < maxJobs) {
        if(i <= length(X)) {
          l1[[i]] <- sge.submit(FUN, X[[i]], ...)
          i <- i + 1
          nJobs <- nJobs + 1
        } else break;
      }
    }
    
    Sys.sleep(sleep)
    
    r1 <- lapply(l1, sge.job.status)
    nJobs <- length(which(unlist(r1) == 1))
    
    if(trace) {
      cat("nJobs: ", nJobs, "\n")
      cat("i: ", i, "\n")
      cat("length(X): ", length(X), "\n")
      cat("length(r1): ", length(r1), "\n\n")
    }
  }
  
  res <- lapply(l1, sge.list.get.result)
  res
}