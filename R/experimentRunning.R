#' Function to generate the \code{usedData} structure, which determines the data to be used during the experimentation.
#' 
#' @title Function to generate the \code{usedData} structure
#' @param useCases the use cases to add
#' @param onlyUseStationarySeries should the files be filtered for stationary series?
#' @param removeDuplicatesByDescStatTable should duplicate series be determined by statistical properties and be removed?
#' @param useSelectionFile should a selection file be used to determine, which series actually to use?
#' @param selectionFile if \code{useSelectionFile} is TRUE, here the filename of the selection file has to be given
#' @export 
genUsedData <- function(useCases, onlyUseStationarySeries=TRUE, removeDuplicatesByDescStatTable=TRUE, useSelectionFile=FALSE, selectionFile="") {
  
  usedData <- lapply(useCases, function(useCase) {
        dataPackagePath <- assemblePathName(assemblePathName(useCase$dataRepository, useCase$dataPackage), useCase$dataPackage)
        dataPath <- assemblePathName(dataPackagePath, useCase$partPath)
        filenames <- list.files(dataPath, pattern="+tra\\.RData") 
        
        excludedSeriesNames <- NULL
        
        if(onlyUseStationarySeries) {
          
          charTable <- try(read.csv(paste(dataPackagePath, "_statNonlin.csv", sep=""), stringsAsFactors=FALSE))
          if(!inherits(charTable, "try-error")) {
            seriesNames <- charTable[, 1]
            excludedSeriesNames <- append(excludedSeriesNames, seriesNames[which(!charTable[, "stAdf"])])
            #print(nonStatSeriesNames)
          }        
        } 
        
        if(removeDuplicatesByDescStatTable) {
          
          charTable <- try(read.csv(paste(dataPackagePath, "_descStat.csv", sep=""), stringsAsFactors=FALSE))
          if(!inherits(charTable, "try-error")) {
            dupInd <- which(duplicated(charTable[,2:ncol(charTable)]))
            seriesNames <- charTable[dupInd, 1]
            excludedSeriesNames <- append(excludedSeriesNames, seriesNames)
            #print(nonStatSeriesNames)
          }       
        }
        
        partitions <- NULL
        for(file in filenames) {
          
          setName <- removeFileEnding(file, "tra.RData")
          
          if(!(removeFileEnding(setName, "-1-1") %in% excludedSeriesNames)) {
            
            md5SumTra <- md5sum(assemblePathName(dataPath, paste(setName, "tra.RData", sep="")))
            names(md5SumTra) <- NULL
            md5SumTst <- md5sum(assemblePathName(dataPath, paste(setName, "tst.RData", sep="")))
            names(md5SumTst) <- NULL
            
            partitions <- rbind(partitions, data.frame(setName=setName, md5SumTra=md5SumTra, md5SumTst=md5SumTst, stringsAsFactors=FALSE))         
          }        
        }
        useCase[["partitions"]] <- partitions
        useCase
      })
  
  if(useSelectionFile) {
    
    selectedTS <- try(read.csv(selectionFile, , header=FALSE, stringsAsFactors=FALSE))
    
    if(!inherits(selectedTS, "try-error")) {
      
      selectedTSListRaw <- strsplit(selectedTS[,1], "-")
      
      selectedTSList <- list()
      for(i in 1:length(selectedTSListRaw)) {
        
        #substr(selectedTS[i,1], length(selectedTSListRaw[[i]][1])+1, length(selectedTS[i,1]))
        
        seriesName <- substr(selectedTS[i,1], nchar(selectedTSListRaw[[i]][1])+2, nchar(selectedTS[i,1]))
        seriesName <- paste(removeFileEnding(seriesName, "1-1tra"), "-1-1", sep="")
        selectedTSList[[selectedTSListRaw[[i]][1]]] <- append(selectedTSList[[selectedTSListRaw[[i]][1]]], seriesName)
      }
      
      #ind <- 1
      for(ind in 1:length(usedData)) {
        
        dataPackageName <- usedData[[ind]]$dataPackage
        selSeriesNames <- selectedTSList[[dataPackageName]]
        selRows <- match(selSeriesNames, usedData[[dataPackageName]][["partitions"]][,"setName"])
        selRows <- selRows[!is.na(selRows)]
        
        if(length(selRows) != 0)
          usedData[[dataPackageName]][["partitions"]] <- usedData[[dataPackageName]][["partitions"]][selRows,]
      }        
    } 
    
  }
  
  usedData
}

#' This is one of the most important functions of the package. It uses the data defined by \code{usedData}, 
#' a structure which you can generate using the \code{\link{genUsedData}} function, takes the defined methods 
#' in the \code{methodsDefinitions} structure, and applies all methods defined to all data defined. When giving 
#' a parallel apply function in \code{tsApply}, the experiments are run in parallel. All outputs of the experiments 
#' are written to the output directory defined in \code{expPath}.
#' 
#' TODO: Describe in more detail how to create the \code{methodsDefinitions} structure.
#' 
#' @title Start a defined set of experiments. 
#' @param usedData a structure which defines the data to use for the experiments. Usually generated with the 
#' \code{\link{genUsedData}} function.
#' @param expPath the path to write the experiments to. A reasonable path can be generated using the 
#' function \code{\link{assembleExperimentsPath}}.
#' @param methodsDefinitions a structure that defines all the prediction methods you want to use. See the demo code for how to do it.
#' @param tsApply the apply function to be used. If using the script on different machines with different parallelization capabilities, 
#' you probably want to automatically choose this function using \code{\link{initParallelMode}}.
#' @param seed this seed value is set for every execution of a predictor.
#' @param predictFunc the predict function to be used. Here, you can add special processing of some of the predictors don't use the 
#' standard format of predict functions. 
#' @return the results of all experiments are returned in a list, but -- more important -- they are saved to disk. The 
#' structure in the newly created directory \code{expPath} is:
#' \item{usedData}{The usedData structure is saved both in plain text and in the ".RData" format. This structure contains information on all input data used, including md5 hashes of the input data files.}
#' \item{models}{a subdirectory with an ".RData" file for every model trained. Take into account that this model may or 
#' may not be used for further predictions. This depends on the implementation of the model (if it is implemented in pure R or in C/C++)}
#' \item{predictions}{an ".RData" file for every prediction of every model, containing the predictions on the respective test set for this model}
#' \item{results}{tables calculated using the predictions and the respective reference and benchmark values: All available error measures are calculated both for the training set (fit, "modEval") and the test set ("predEval"). For the test set, there are also
#' denormalized versions calculated "Denorm". Furthermore, there may be some special tables as the value containing the R package and version for each of the models (or the hash of the git commit, if available).} 
#' 
#' @export 
runExperiments <- function(usedData, expPath, methodsDefinitions, tsApply=lapply, seed=5, predictFunc=predict) {
  
  #Creating the folder for the experiment run, and saving the usedData structure
  dir.create(expPath, showWarnings=FALSE, recursive=TRUE)  
  save(usedData, file = assemblePathName(expPath, "usedData.RData"))  
  sink(assemblePathName(expPath, "usedData.txt"))
  print(usedData)
  sink()
  
  #Creating the folder for the models
  modelsPath <- assemblePathName(expPath, "models")
  dir.create(modelsPath, showWarnings=FALSE, recursive=TRUE)
  predictionsPath <- assemblePathName(expPath, "predictions")
  dir.create(predictionsPath, showWarnings=FALSE)
  
  #For each useCase(Dataset)
  experimentRuns <- list()
  for(useCase in usedData) {
    #Create folder for the UseCases
    dataPath <- assemblePathName(assemblePathName(assemblePathName(useCase$dataRepository, useCase$dataPackage), useCase$dataPackage), useCase$partPath)
    dir.create(assemblePathName(modelsPath,useCase$name), showWarnings=FALSE)
    useCasePredictionsPath <- assemblePathName(predictionsPath, useCase$name)
    dir.create(useCasePredictionsPath, showWarnings=FALSE, recursive=TRUE)
    #Preparing the experiments data structure for methods
    for(trainSetName in paste(useCase[["partitions"]][["setName"]], "tra", sep=""))
      for(m in names(methodsDefinitions)) {
        experimentRuns[[paste(useCase$name, trainSetName, "-", m, sep="")]] <- list()
        experimentRuns[[paste(useCase$name, trainSetName, "-", m, sep="")]][["qualifiedTrainSetName"]] <- assemblePathName(dataPath, trainSetName)
        experimentRuns[[paste(useCase$name, trainSetName, "-", m, sep="")]][["qualifiedTestSetName"]] <- assemblePathName(dataPath, paste(removeFileEnding(trainSetName,"tra"),"tst",sep=""))
        experimentRuns[[paste(useCase$name, trainSetName, "-", m, sep="")]][["methodDefinition"]] <- methodsDefinitions[[m]]
        experimentRuns[[paste(useCase$name, trainSetName, "-", m, sep="")]][["useCaseName"]] <- useCase$name
      }
  } 
  
  #Running the experiments
  res <- tsApply(experimentRuns, function(experimentRun) {
        
        #Getting main attributes
        qualifiedTrainSetName <- experimentRun$qualifiedTrainSetName
        qualifiedTestSetName <- experimentRun$qualifiedTestSetName
        methodDefinition <- experimentRun$methodDefinition
        
        #---------------------
        
        if(methodDefinition[["funcname"]] == "nnet") require("nnet")
        else if (methodDefinition[["funcname"]] == "svm") require("e1071")
        else if (methodDefinition[["funcname"]] == "lars") require("lars")
        else if (methodDefinition[["funcname"]] == "mlp" || methodDefinition[["funcname"]] == "elman" || methodDefinition[["funcname"]] == "jordan") require("RSNNS")
        #---------------------
        
        
        
        if(is.null(methodDefinition[["version"]])) {
          packageName <- environmentName(environment(get(methodDefinition[["funcname"]])))
          pack <- installed.packages()
          if(packageName %in% rownames(pack)) {
            methodDefinition[["version"]] <- paste("Rpkg", packageName, pack[packageName, "Version"], sep="-")
          } 
          
        }
        
        #Loading training data 
        trainFilename <- paste(qualifiedTrainSetName, ".RData", sep="")
        load(trainFilename)
        trainData <- get(getFileNameFromPath(qualifiedTrainSetName))
        
        #Loading test data
        testFilename <- paste(qualifiedTestSetName, ".RData", sep="")
        load(testFilename)
        testData <- get(getFileNameFromPath(qualifiedTestSetName))
        
        #Defines if the method needs one (values + targets) or two input parameters (values, target)
        resPerMethod <- list()
        if(is.null(methodDefinition[["trainDataType"]])) 
          methodDefinition[["trainDataType"]] <- "combined"
        
        if(methodDefinition[["trainDataType"]] == "separated") {
          parameters <- append(list(getValues(trainData), getTargets(trainData)), methodDefinition[["params"]])
        }
        else if (methodDefinition[["trainDataType"]] == "vector"){
          parameters <- append(list(ts(getTargets(trainData))), methodDefinition[["params"]])
        }
        else
          parameters <- append(list(trainData), methodDefinition[["params"]])                 
        
        #setting the seed

        if(!is.null(methodDefinition[["seed"]])) {
          seed <- methodDefinition[["seed"]]
        }

        set.seed(seed)
        
        #Training the method
        resPerMethod[["trainedModel"]] <- try(do.call(methodDefinition[["funcname"]], parameters))  
        #Applying the method to train and test sets
        
        if (methodDefinition[["trainDataType"]] == "vector"){
          fittedValues <- try(getTargets(trainData) + resPerMethod[["trainedModel"]]$residuals)
          testValues <- try(predictFunc(resPerMethod[["trainedModel"]], getTargets(testData), getTargets(trainData)))
          
        }else{
          fittedValues <- try(predictFunc(resPerMethod[["trainedModel"]], getValues(trainData)))
          testValues <- try(predictFunc(resPerMethod[["trainedModel"]], getValues(testData)))
        }	
        
        name <- paste(getName(trainData), "-", methodDefinition[["shortname"]], sep="")
        resPath <- assemblePathName(modelsPath, experimentRun$useCaseName)
        
        #Adding the attributes and saving the method if everthing went well, otherwise show errors and continue 
        if(inherits(resPerMethod[["trainedModel"]], "try-error") || 
            inherits(fittedValues, "try-error") || inherits(testValues, "try-error")) {
          errFile <- assemblePathName(resPath, paste(name, "_err.txt", sep=""))
          sink(errFile)
          print(methodDefinition)
          print(resPerMethod[["trainedModel"]])
          sink()
        } else {
          resPerMethod[["methodDefinition"]] <- methodDefinition
          resPerMethod[["trainData"]] <- trainData
          resPerMethod[["testData"]] <- testData
          resPerMethod[["useCaseName"]] <- experimentRun$useCaseName
          #
          resPerMethod[["fittedValues"]] <- fittedValues
          resPerMethod[["testValues"]] <- testValues
          resPerMethod[["fittedResiduals"]] <- getTargets(trainData) - fittedValues
          resPerMethod[["testResiduals"]] <- getTargets(testData) - testValues
          
          
          assign(name, resPerMethod)
          save(list=name, file = assemblePathName(resPath, paste(name, ".RData", sep="")))   
          
          
          attr(testValues, "setName") <- aTSattr(resPerMethod$trainData, "shortname")
          attr(testValues, "tsName") <- aTSattr(resPerMethod$trainData, "tsname")
          attr(testValues, "modName") <- resPerMethod$methodDefinition$shortname
          attr(testValues, "useCaseName") <- resPerMethod$useCaseName
          currModelName <- paste(aTSattr(resPerMethod$trainData, "name"),"-",resPerMethod$methodDefinition$shortname, sep="")
          namePred <- paste(currModelName, "-pred", sep="")
          
          assign(namePred, testValues)
          save(list=namePred, file = assemblePathName(assemblePathName(predictionsPath, experimentRun$useCaseName), paste(namePred, ".RData", sep="")))
        }
        resPerMethod
      })
  
  res
}


#' This function uses a bunch of models, created with \code{\link{runExperiments}}, 
#' and input data defined in \code{usedData}, together with a prediction function to 
#' calculate predictions for all data (in the test sets) applying all the trained models.
#' The difference between this function and the function \code{\link{runPredictions}} is, that the models
#' are not saved on disk and then reloaded. Depending on the implementation of the models,
#' this may cause problems. 
#'
#' TODO: I think this function is not needed/used any more, as the predictions are calculated directly in 
#' \code{\link{runExperiments}} after model building.
#' 
#' @title Calculates predictions from a list of models and input data, models are in memory 
#' @param usedData the usedData structure to use (see \code{\link{genUsedData}})
#' @param model a list of all the models (usually, the output of \code{\link{runExperiments}})
#' @param expPath the output path, see \code{\link{assembleExperimentsPath}} and \code{\link{runExperiments}}
#' @param predictFunc the function that is applied to the model to get the prediction
#' @seealso \code{\link{genUsedData}}, \code{\link{runExperiments}}, \code{\link{runExperiments}}, \code{\link{runPredictions}}
#' @export 
runPrediction <- function(usedData, model, expPath, predictFunc=predict) {
  
  predictionsPath <- assemblePathName(expPath, "predictions")
  dir.create(predictionsPath, showWarnings=FALSE)
  
  res <- lapply(usedData, function(useCase) {
        dataPath <- assemblePathName(assemblePathName(assemblePathName(useCase$dataRepository, useCase$dataPackage), useCase$dataPackage), useCase$partPath)
        
        useCasePredictionsPath <- assemblePathName(predictionsPath, useCase$name)
        denormPredictionsPath <- assemblePathName(useCasePredictionsPath, "denorm")
        
        dir.create(denormPredictionsPath, showWarnings=FALSE, recursive=TRUE)
        
        testPartitions <- loadATSDir(dataPath, pattern="+tst\\.RData")
        
        
        predictions <- list()
        for(currModel in model) {	
          currTestPart <- testPartitions[[paste(removeFileEnding(getName(currModel$trainData), "tra"), "tst", sep="")]]
          currModelName <- paste(aTSattr(currModel$trainData, "name"),"-",currModel$methodDefinition$shortname, sep="")
          name <- paste(currModelName, "-pred", sep="")
          
          pred <- try(predictFunc(currModel$trainedModel, getValues(currTestPart)))
          
          if(inherits(pred, "try-error")) {
            errFile <- assemblePathName(useCasePredictionsPath, paste(name, "_err.txt", sep=""))
            sink(errFile)
            sink()          
          } else {
            
            attr(pred, "setName") <- aTSattr(currModel$trainData, "shortname")
            attr(pred, "tsName") <- aTSattr(currModel$trainData, "tsname")
            attr(pred, "modName") <- currModel$methodDefinition$shortname
            attr(pred, "useCaseName") <- currModel$useCaseName
            
            assign(name, pred)
            save(list=name, file = assemblePathName(useCasePredictionsPath, paste(name, ".RData", sep="")))
            
#						if(!is.null(getNormParams(currTestPart))) {
#							predDenorm <- denormalizeData(pred, getNormParams(currTestPart))    
#							
#							attr(predDenorm, "setName") <- aTSattr(currModel$trainData, "shortname")
#							attr(predDenorm, "tsName") <- aTSattr(currModel$trainData, "tsname")
#							attr(predDenorm, "modName") <- currModel$methodDefinition$shortname
#							attr(predDenorm, "useCaseName") <- currModel$useCaseName
#							
#							assign(name, predDenorm)
#							save(list=name, file = assemblePathName(denormPredictionsPath, paste(name, ".RData", sep="")))
#							
#						}
            
            predictions[[currModelName]] <- pred
            
          }
          
        }
      })
  res
}

#' This function uses a bunch of models, created with \code{\link{runExperiments}}, 
#' and input data defined in \code{usedData}, together with a prediction function to 
#' calculate predictions for all data (in the test sets) applying all the trained models.
#' The difference between this function and the function \code{\link{runPrediction}} is, that the models
#' are loaded from disk. Thus, it can be ran indepentdently of the experiments, and e.g., can also be used
#' if the experiments haven't terminated yet, were aborted, etc. The drawback is, that not all 
#' models survive a save/load cycle without problems.
#' 
#' TODO: This function is not needed/used any more, as the predictions are calculated directly in 
#' \code{\link{runExperiments}} after model building. Maybe there are still some cases, when for some reason
#' prediction failed, but models are there, so that this function can be used to calculate them. 
#' 
#' @title Calculates predictions from a list of models and input data, models are on disk 
#' @param usedData the usedData structure to use (see \code{\link{genUsedData}})
#' @param expPath both the output path, and the path where the models are loaded from. 
#' See \code{\link{assembleExperimentsPath}} and \code{\link{runExperiments}}
#' @param predictFunc the function that is applied to the model to get the prediction
#' @seealso \code{\link{genUsedData}}, \code{\link{runExperiments}}, \code{\link{runExperiments}}, \code{\link{runPrediction}}
#' @export 
runPredictions <- function(usedData, expPath, predictFunc=predict) {
  
  modelsPath <- assemblePathName(expPath, "models")
  predictionsPath <- assemblePathName(expPath, "predictions")
  dir.create(predictionsPath, showWarnings=FALSE)
  
  res <- lapply(usedData, function(useCase) {
        
        dataPath <- assemblePathName(assemblePathName(assemblePathName(useCase$dataRepository, useCase$dataPackage), useCase$dataPackage), useCase$partPath)
        useCaseModelsPath <- assemblePathName(modelsPath, useCase$name)
        
        useCasePredictionsPath <- assemblePathName(predictionsPath, useCase$name)
        denormPredictionsPath <- assemblePathName(useCasePredictionsPath, "denorm")
        
        dir.create(denormPredictionsPath, showWarnings=FALSE, recursive=TRUE)
        
        testPartitions <- loadATSDir(dataPath, pattern="+tst\\.RData")
        
        modelFilenames <- list.files(useCaseModelsPath, pattern="+\\.RData")
        
        predictions <- list()
        for(modelFilename in modelFilenames) {
          
          load(assemblePathName(useCaseModelsPath, modelFilename))
          
          currModelName <- removeFileEnding(modelFilename, ".Rdata")
          currModel <- get(currModelName)
          
          currTestPart <- testPartitions[[paste(removeFileEnding(getName(currModel$trainData), "tra"), "tst", sep="")]]
          #getName(currModel$trainedModel$str)
          
          name <- paste(currModelName, "-pred", sep="")
          
#    if(is.list(currModel$trainedModel)) {
          pred <- try(predictFunc(currModel$trainedModel, getValues(currTestPart)))
          
          if(inherits(pred, "try-error")) {
            errFile <- assemblePathName(useCasePredictionsPath, paste(name, "_err.txt", sep=""))
            sink(errFile)
            print(pred)
            sink()          
          } else {
            
            attr(pred, "setName") <- aTSattr(currModel$trainData, "shortname")
            attr(pred, "tsName") <- aTSattr(currModel$trainData, "tsname")
            attr(pred, "modName") <- currModel$methodDefinition$shortname
            attr(pred, "useCaseName") <- currModel$useCaseName
            
            assign(name, pred)
            save(list=name, file = assemblePathName(useCasePredictionsPath, paste(name, ".RData", sep="")))
            
            if(!is.null(getNormParams(currTestPart))) {
              predDenorm <- denormalizeData(pred, getNormParams(currTestPart))    
              
              attr(predDenorm, "setName") <- aTSattr(currModel$trainData, "shortname")
              attr(predDenorm, "tsName") <- aTSattr(currModel$trainData, "tsname")
              attr(predDenorm, "modName") <- currModel$methodDefinition$shortname
              attr(predDenorm, "useCaseName") <- currModel$useCaseName
              
              #nameDenorm <- paste(currModelName, "-pred", sep="")
              assign(name, predDenorm)
              save(list=name, file = assemblePathName(denormPredictionsPath, paste(name, ".RData", sep="")))
              
            }
            
            predictions[[currModelName]] <- pred
            
          }
          
          #plot(getTimeStamps(currTestPart), pred)  
          
#    } else {
#      #if the "model" is e.g. only an error message, return NAs 
#      pred <- seq(0,0,length=nSamples(currTestPart))
#      pred[1:length(pred)] <- NA   
#      predDenorm <- NA
#    }
          
        }
      })
  res
  
}


#' This function is used to generate from a bunch of models saved as ".RData" files
#' evaluations in form of tables which contain all training/fitting errors of the models and, 
#' depending on the models, some other tables with model characteristics.
#' 
#' @title Function to evaluate models built during an experiment run
#' @param qualifiedModelFileNames a list of file names with paths to all the model files to use
#' @param calcDenorm if \code{TRUE}, all error measures are also calculated for denormalized model 
#' fits, along with the measures for normalized fits that are always calculated
#' @export 
evaluateModels <- function(qualifiedModelFileNames, calcDenorm=TRUE) {
  
  models <- list()
  for (modelFileName in qualifiedModelFileNames) {
    
    errorOccured <- FALSE
    
    tryCatch(load(modelFileName), error = function(e) {errorOccured <<- TRUE})
    
    if(errorOccured) next
    
    #load(modelFileName)
    
    modelName <- extractNameFromPath(modelFileName, ".RData")
    model <- get(modelName)
    
    tsName <- paste(model$useCaseName, "-", aTSattr(model$trainData, "tsname"), sep="") #removeFileEnding(modelName, paste("-", aTSattr(model$trainData, "shortname"), "-", model$methodDefinition$shortname, sep=""))
    models[[tsName]][[aTSattr(model$trainData, "shortname")]][[model$methodDefinition$shortname]] <- model
  }
  
  res <- list()
  for (tsName in names(models)) {
    
    resPerSeries <- list()
    #resPerSeries2 <- list()
    
    for(setType in names(models[[tsName]])) {
      
      resPerSet <- lapply(models[[tsName]][[setType]], function(mod){
            
            res <- list()
            
            if(!is.null(mod$trainedModel$noRegimes))  res[["noRegimes"]] <- mod$trainedModel$noRegimes
            if(!is.null(mod$trainedModel$fnEval)) res[["fnEval"]] <- mod$trainedModel$fnEval
            if(!is.null(mod$trainedModel$grEval)) res[["grEval"]] <- mod$trainedModel$grEval
            
            if(!is.null(mod$trainedModel$fitted.values)) {
              #if(!is.null(mod$trainedModel$residuals)) {
              
              prediction <- mod$trainedModel$fitted.values
              reference <- getTargets(mod$trainData)
              benchmark <- getNaiveForecast(mod$trainData)
              
              #browser()
              
              #res[["rmseFit"]] <- sqrt(mean((mod$trainedModel$residuals)^2))
              
              allErrors <- measureErrorAll(prediction = prediction, reference = reference, benchmark = benchmark)
              
              names(allErrors) <- paste("fit_", names(allErrors), sep="")
              
              res <- append(res, allErrors)
              
              if(calcDenorm) {
                
                deNormParams <- getNormParams(mod$trainData)
                denormPrediction <- denormalizeData(prediction, deNormParams)
                denormReference <- denormalizeData(reference, deNormParams)
                denormBenchmark <- denormalizeData(benchmark, deNormParams)
                
                allErrorsDenorm <- measureErrorAll(prediction = denormPrediction, reference = denormReference, benchmark = denormBenchmark)
                
                names(allErrorsDenorm) <- paste("fit_denorm_", names(allErrorsDenorm), sep="")
                
                res <- append(res, allErrorsDenorm)
                
              }
              
            }
            
            if(!is.null(mod$methodDefinition$version)) res[["version"]] <- mod$methodDefinition$version
#              
#            if(class(mod$trainedModel)[1]=="genericStar") {
#              #return(mod$trainedModel$noRegimes)
#              return(data.frame(noRegimes=mod$trainedModel$noRegimes, 
#                      fnEval=mod$trainedModel$model.specific$fnEval, 
#                      grEval=mod$trainedModel$model.specific$grEval, 
#                      rmseFit=sqrt(sum((mod$trainedModel$residuals)^2))), 
#                      version=mod$methodDefinition$version)
#            } else {
#
#              if(!is.null(mod$trainedModel$residuals)) 
#                rmseFit <- crossprod(mod$trainedModel$residuals)
#              else 
#                rmseFit <- NA
#              
#              return(data.frame(noRegimes=NA, fnEval=NA, grEval=NA, rmseFit=rmseFit))
#            }
            res
          })
      
      for(modName in names(resPerSet)) {
        for (measure in names(resPerSet[[modName]])) {
          resPerSet[[modName]][[measure]] <- as.data.frame(resPerSet[[modName]][[measure]])
          rownames(resPerSet[[modName]][[measure]]) <- modName
          colnames(resPerSet[[modName]][[measure]]) <- paste(tsName, setType, sep="")
          resPerSeries[[measure]] <- rBindByColNames(resPerSeries[[measure]], resPerSet[[modName]][[measure]])          
          
          #rBindByColNames(resPerSet[["lasso"]][["version"]], resPerSet[["AR"]][["version"]])
          
          # x <- resPerSeries[[measure]]
          # y <- resPerSet[[modName]][[measure]]
          
          # x <- res[[measure]]
          # y <- t(resPerSeries[[measure]])
          
          #resPerSeries2[[measure]] <- rbind(resPerSeries2[[measure]], resPerSet[[modName]][[measure]])
          #rownames(resPerSeries2[[measure]])[nrow(resPerSeries2[[measure]])] <- modName
          
#          resPerSeries[[measure]] <- rbind(resPerSeries[[measure]], resPerSet[[modName]][[measure]])
#          rownames(resPerSeries[[measure]])[nrow(resPerSeries[[measure]])] <- modName
        }
      }
      
    }
    
    for(measure in names(resPerSeries)) {
      #colnames(resPerSeries[[measure]]) <- paste(tsName, setType, sep="")
      
      #res[[measure]] <- cBindByRowNames(res[[measure]], resPerSeries[[measure]])
      res[[measure]] <- rBindByColNames(res[[measure]], t(resPerSeries[[measure]]))
    }
    
  }
  res
} 


#' This function is used to generate from predictions and the respective "real" reference values 
#' (determined by \code{usedData}) evaluations in form of tables which contain all errors of these 
#' predictions (i.e., errors on the test set).
#' 
#' @title Function to evaluate predictions
#' @param qualifiedPredictionFileNames a list of file names with paths to all the prediction files to use
#' @param usedData the \code{usedData} structure which defines the input files. From here, the reference and benchmark
#' values are extracted.
#' @param denorm if \code{TRUE}, all error measures are also calculated using denormalized predictions and
#' denormalized reference values and benchmarks.
#' @export
evaluatePredictions <- function(qualifiedPredictionFileNames, usedData, denorm=FALSE) {
  
  #referenceData <- loadATSDir(referenceDataDir, pattern="+tst\\.RData")
  
  models <- list()
  errorMeasures <- list()
  for (predictionFileName in qualifiedPredictionFileNames) {
    
    load(predictionFileName)
    
    predictionName <- extractNameFromPath(predictionFileName, ".RData")
    prediction <- get(predictionName)
    
    tsName <- attr(prediction, "tsName")#removeFileEnding(predictionName, paste("-", aTSattr(model$trainData, "shortname"), "-", model$methodDefinition$shortname, sep=""))
    setName <- attr(prediction, "setName")
    modName <- attr(prediction, "modName")
    useCaseName <- attr(prediction, "useCaseName")
    #models[[tsName]][[aTSattr(model$trainData, "shortname")]][[model$methodDefinition$shortname]] <- model
    #print(tsName)
    #print(shortname)
    #print(modname)
    
    useCase <- usedData[[useCaseName]]
    dataPath <- assemblePathName(assemblePathName(assemblePathName(useCase$dataRepository, useCase$dataPackage), useCase$dataPackage), useCase$partPath)
    
    
    refDataName <- paste(removeFileEnding(paste(tsName, "-", setName,sep=""), "tra"), "tst", sep="")
    load(assemblePathName(dataPath, paste(refDataName, ".RData",sep="")))
    
    #reference <- referenceData[[refDataName]]
    reference <- get(refDataName)
    #values <- getValues(reference)
    
    targets <-  getTargets(reference)
    benchmark <- getNaiveForecast(reference) #values[,ncol(values)]
    
    if(denorm) {
      
      deNormParams <- getNormParams(reference)
      prediction <- denormalizeData(prediction, deNormParams)
      targets <- denormalizeData(targets, deNormParams)
      benchmark <- denormalizeData(benchmark, deNormParams)
    }
    
    refDataName <- paste(useCaseName, "-", refDataName, sep="")
    
    errorMeasures[[refDataName]][[modName]] <- measureErrorAll(prediction = prediction, reference = targets, benchmark = benchmark)
    errorMeasures[[refDataName]][[modName]] <- t(errorMeasures[[refDataName]][[modName]])
    #data.frame(res)    
  }
  errorMeasures <- lapply(errorMeasures, as.data.frame)
  
  perMeasureErrors <- list()
  
  for (refName in names(errorMeasures)) {
    
    for(indMeasure in 1:length(rownames(errorMeasures[[refName]]))) {
      measure <- rownames(errorMeasures[[refName]])[indMeasure]
      tempDF <- as.data.frame(errorMeasures[[refName]][indMeasure, , drop=FALSE])
      rownames(tempDF) <- c(refName)
      perMeasureErrors[[measure]] <- rBindByColNames(perMeasureErrors[[measure]], tempDF)
    }
  }
  perMeasureErrors
}


#' This function is used to plot predictions as overlay on the original time series.
#' 
#' TODO: Why does this function take directory names, and \code{\link{evaluatePredictions}}
#' takes a \code{usedData} structure?
#' 
#' @title Function to plot predictions
#' @param qualifiedPredictionFileNames a list of file names with paths to all the prediction files to use
#' @param referenceDataDir the directory where the reference data is located (test sets)
#' @param originalDataDir the directory where the original data is located (full time series)
#' @param targetDir directory to which to save to the plots
#' @export
plotPredictions <- function(qualifiedPredictionFileNames, referenceDataDir, originalDataDir, targetDir) {
  
  originalData <- loadATSDir(originalDataDir, pattern="+\\.RData")
  
  referenceData <- loadATSDir(referenceDataDir, pattern="+tst\\.RData")
  
  for (predictionFileName in qualifiedPredictionFileNames) {
    
    load(predictionFileName)
    
    predictionName <- extractNameFromPath(predictionFileName, ".RData")
    prediction <- get(predictionName)
    
    tsName <- attr(prediction, "tsname")
    setName <- attr(prediction, "setname")
    modName <- attr(prediction, "modname")
    
    refDataName <- paste(removeFileEnding(paste(tsName, "-", setName,sep=""), "tra"), "tst", sep="")
    reference <- referenceData[[refDataName]]
    
    origTS <- originalData[[tsName]]
    
    jpgFile <- assemblePathName(targetDir , paste(predictionName, ".jpg", sep=""))
    jpeg(filename=jpgFile,width=800,height=200)
    
    plot(origTS, ylim=c(min(min(origTS), min(prediction)), max(max(origTS), max(prediction))))
    lines(getTimeStamps(reference), prediction, col="red")
    
    dev.off()
  }
}


