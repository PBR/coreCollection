setOldClass("dist")

.showCoreCollection <- function(object, title) {
  cat("==========\n")
  cat(paste0(title,"\n"))
  cat(paste0("- distanceMatrix for ",attr(object$distanceMatrix,"Size")," items\n"))
  cat(paste0("- required core size is ",object$n))
  lengthPreselected <- length(object$preselected);
  if(lengthPreselected>0) {
    cat(paste0(" with ",lengthPreselected," fixed item",ifelse(lengthPreselected>1,"s","")," to include\n"))
  } else {
    cat("\n");
  }
  cat(paste0("- random selected items: ",nrow(object$randomSelected),"\n"))
  if(lengthPreselected>0) {
    cat(paste0("- adjusted selected items: ",nrow(object$adjustedSelected),"\n"))
  }
  cat(paste0("- core items: ",nrow(object$core),"\n"))
  cat(paste0("- seed: ",object$seed,"\n"))
  if(lengthPreselected>0) {
    cat(paste0("The adjustedGroupMethod is '",object$adjustedGroupMethod,"' and the "))
  } else {
    cat("The ")
  }
  cat(paste0("coreSelectMethod is '",object$coreSelectMethod,"'\n"))
  cat(paste0("Applied algorithm is '",object$algorithm,"'\n"))
  cat("==========\n")
}

.CoreCollectionClass <- R6Class(
  classname="coreCollection",
  private = list(
    initialised = FALSE,
    constantAvailableCoreSelectMethods = c("A-NE", "E-NE", "E-E"),
    constantAvailableAlgorithms = c("randomDescent"),
    constantAvailableAdjustedGroupMethods = c("split", "recompute"),
    variableDistanceMatrix = NULL,
    variableN = NULL,
    variablePreselected = NULL,
    variableRandomSelected = NULL,
    variableRandomBasedGroups = NULL,
    variableAdjustedGroupMethod = NULL,
    variableAdjustedSelected = NULL,
    variableAdjustedBasedGroups = NULL,
    variableCoreSelectMethod = NULL,
    variableCoreSelected = NULL,
    variableAlgorithm = NULL,
    variableSeed = NULL,
    variableRandomSeed = NULL,
    setDistanceMatrix=function(value) {
      if(!missing(value) && !is.null(value)) {
        distanceMatrix <- as.dist(value)
        if(attr(distanceMatrix,"Size")<=1) {
          stop("distanceMatrix size should be > 1")
        }
        private$variableDistanceMatrix <- distanceMatrix
      }
    },
    setN=function(value) {
      if(!missing(value) && !is.null(value)) {
        n <- as.integer(value);
        if(is.na(n) | n<1) {
          stop(paste0("n (",n,") should be a postive integer"))
        } else {
          sizeDistanceMatrix <- ifelse(is.null(private$variableDistanceMatrix),0,attr(private$variableDistanceMatrix,"Size"));
          if(n>=sizeDistanceMatrix) {
            stop(paste0("n (",n,") should be less than the size of the distanceMatrix (",sizeDistanceMatrix,")"))
          }
        }
        private$variableN <- n;
      }
    },
    setPreselected=function(value) {
      if(!missing(value) && !is.null(value)) {
        preselected <- unique(as.vector(value, mode="character"))
        l <- length(preselected)
        if(l>=private$variableN) {
          stop(paste0("preselected should not contain more items (",l,") than the requested core size (",private$variableN,")"))
        } else if(l>0) {
          labels <- labels(private$variableDistanceMatrix)
          diff <- setdiff(preselected,labels)
          if(length(diff)>0) {
            stop(paste0("preselected should not not contain items not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
          } else {
            private$variablePreselected <- preselected
          }
        } else {
          private$variablePreselected <- c()
        }
      } else {
        private$variablePreselected <- c()
      }
    },
    setCoreSelectMethod=function(value) {
      if(!missing(value) && !is.null(value)) {
        method <- as.character(value)
        if(length(method)==1 && method %in% private$constantAvailableCoreSelectMethods) {
          private$variableCoreSelectMethod <- method
        } else {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as coreSelectMethod, allowed: '",paste(private$constantAvailableCoreSelectMethods, collapse="', '"),"'"))
        }
      } else {
        private$variableCoreSelectMethod <- private$constantAvailableCoreSelectMethods[1];
      }
    },
    setAlgorithm=function(value) {
      if(!missing(value) && !is.null(value)) {
        algorithm <- as.character(value)
        if(length(algorithm)==1 && algorithm %in% private$constantAvailableAlgorithms) {
          private$variableAlgorithm <- algorithm
        } else {
          stop(paste0("can't use '",paste(algorithm,collapse=", "),"' as algorithm, allowed: '",paste(private$constantAvailableAlgorithms, collapse="', '"),"'"))
        }
      } else {
        private$variableAlgorithm <- private$constantAvailableAlgorithms[1];
      }
    },
    setSeed=function(value) {
      if(!missing(value) && !is.null(value)) {
        seed <- as.integer(value)
        if(!is.na(seed)) {
          private$variableSeed <- seed
          set.seed(private$variableSeed)
        } else {
          stop(paste0("can't use '",paste(value,collapse=", "),"' as seed, use integer"))
        }
      } else if(is.null(value)) {
        private$variableRandomSeed=round((as.numeric(Sys.time())*1000)%%.Machine$integer.max)
        set.seed(private$variableRandomSeed)
      } else {
        private$variableSeed <- NULL
      }
    },
    setAdjustedGroupMethod=function(value) {
      if(!missing(value) && !is.null(value)) {
        method <- as.character(value)
        if(length(method)==1 && method %in% private$constantAvailableAdjustedGroupMethods) {
          private$variableAdjustedGroupMethod <- method
        } else {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as adjustedGroupMethod, allowed: '",paste(private$constantAvailableAdjustedGroupMethods, collapse="', '"),"'"))
        }
      } else {
        private$variableAdjustedGroupMethod <- private$constantAvailableCoreSelectMethods[1];
      }
    },
    setRandomSelected=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("random selected set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          randomSelectedTable <- table(result)
          #set randomSelected set including frequencies
          private$variableRandomSelected <- data.frame(contains=as.numeric(randomSelectedTable))
          rownames(private$variableRandomSelected) <- rownames(randomSelectedTable)
          if(length(private$variablePreselected)>0) {
            preselectedTable <- table(result[match(private$variablePreselected, labels)])
            private$variableRandomSelected[,"preselects"] <- 0
            private$variableRandomSelected[rownames(preselectedTable),"preselects"] <- as.numeric(preselectedTable)
            private$variableRandomSelected[,"preselected"] <- (rownames(private$variableRandomSelected) %in% private$variablePreselected)
          } else {
            private$variableRandomSelected[,"preselects"] <- 0
            private$variableRandomSelected[,"preselected"] <- FALSE
          }
          private$variableRandomSelected[,"random"] <- TRUE
          private$variableRandomBasedGroups <- result
          names(private$variableRandomBasedGroups) <- labels
        }
      } else {
        private$variableRandomSelected <- NULL
        private$variableRandomBasedGroups <- NULL
      }
    },
    setAdjustedSelected=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("adjusted selected set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          adjustedSelectedTable <- table(result)
          #set adjustedSelected set including frequencies
          private$variableAdjustedSelected <- data.frame(contains=as.numeric(adjustedSelectedTable))
          rownames(private$variableAdjustedSelected) <- rownames(adjustedSelectedTable)
        }
        if(length(private$variablePreselected)>0) {
          preselectedTable <- table(result[match(private$variablePreselected, labels)])
          private$variableAdjustedSelected[,"preselects"] <- 0
          private$variableAdjustedSelected[rownames(preselectedTable),"preselects"] <- as.numeric(preselectedTable)
          private$variableAdjustedSelected[,"preselected"] <- (rownames(private$variableAdjustedSelected) %in% private$variablePreselected)
        } else {
          private$variableAdjustedSelected[,"preselects"] <- 0
          private$variableAdjustedSelected[,"preselected"] <- FALSE
        }
        private$variableAdjustedSelected[,"random"] <- (rownames(private$variableAdjustedSelected) %in% rownames(private$variableRandomSelected))
        private$variableAdjustedBasedGroups <- result
        names(private$variableAdjustedBasedGroups) <- labels
      } else {
        private$variableAdjustedSelected <- NULL
        private$variableAdjustedBasedGroups <- NULL
      }
    },
    setCore=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("core set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          coreSelectedTable <- table(result)
          #set coreSelected set including frequencies
          private$variableCoreSelected <- data.frame(contains=as.numeric(coreSelectedTable))
          rownames(private$variableCoreSelected) <- rownames(coreSelectedTable)
        }
        if(length(private$variablePreselected)>0) {
          preselectedTable <- table(result[match(private$variablePreselected, labels)])
          private$variableCoreSelected[,"preselected"] <- (rownames(private$variableCoreSelected) %in% private$variablePreselected)
        } else {
          private$variableCoreSelected[,"preselected"] <- FALSE
        }
        private$variableCoreSelected$contains <- NULL
      } else {
        private$variableCoreSelected <- NULL
      }
    },
    setInitialised=function(title) {
      if(!private$initialised) {
        #random result
        rawRandomResult <- .computeRandomSelectionCoreCollection(self)
        randomResult <- labels(distanceMatrix)[as.numeric(rawRandomResult)+1]
        names(randomResult) <- labels(distanceMatrix)
        private$setRandomSelected(randomResult)
        #adjusted result
        rawAdjustedResult <- .computeAdjustedSelectionCoreCollection(self, rawRandomResult)
        adjustedResult <- labels(distanceMatrix)[as.numeric(rawAdjustedResult)+1]
        names(adjustedResult) <- labels(distanceMatrix)
        private$setAdjustedSelected(adjustedResult)
        #create core
        rawCoreResult <- .computeCoreSelectionCoreCollection(self)
        coreResult <- labels(distanceMatrix)[as.numeric(rawCoreResult)+1]
        private$setCore(coreResult)
        #finished
        private$initialised = TRUE
        .showCoreCollection(self, title)
      } else {
        cat("CoreCollection not initialised!\n")
      }
    }
  ),
  public = list(
    recompute = function() {
      if(private$initialised) {
        private$initialised = FALSE
        private$setSeed(private$variableSeed)
        private$setInitialised("Recompute Core Collection Object")
      }
    },
    alternativeCore = function(n) {
      if(private$initialised) {
        if(!missing(n) && !is.null(n)) {
          return(.computeAlternativeCore(self, n))
        } else {
          stop("n is required")
        }
      }
    },
    initialize = function(distanceMatrix, n, preselected, coreSelectMethod, adjustedGroupMethod, algorithm, seed) {
      if(!private$initialised) {
        if(!missing(distanceMatrix) && !is.null(distanceMatrix)) {
          private$setDistanceMatrix(distanceMatrix)
        } else {
          stop("distanceMatrix is required")
        }
        if(!missing(n) && !is.null(n)) {
          private$setN(n)
        } else {
          stop("n is required")
        }
        if(!missing(preselected)) {
          private$setPreselected(preselected)
        } else {
          private$setPreselected()
        }
        if(!missing(coreSelectMethod)) {
          private$setCoreSelectMethod(coreSelectMethod)
        } else {
          private$setCoreSelectMethod()
        }
        if(!missing(adjustedGroupMethod)) {
          private$setAdjustedGroupMethod(adjustedGroupMethod)
        } else {
          private$setAdjustedGroupMethod()
        }
        if(!missing(algorithm)) {
          private$setAlgorithm(algorithm)
        } else {
          private$setAlgorithm()
        }
        if(!missing(seed)) {
          private$setSeed(seed)
        } else {
          private$setSeed()
        }
        N <- attr(private$variableDistanceMatrix,"Size")
        #initialize
        private$setInitialised("Created new Core Collection Object")
      } else {
        cat("CoreCollection already initialised!\n")
      }
      invisible(self)
    },
    print = function(...) {
      .showCoreCollection(self,"CoreCollection object")
      invisible(self)
    },
    measure = function(value) {
      if(missing(value)) {
        method <- private$variableCoreSelectMethod
      } else {
        method <- as.character(value)
        if(length(method)!=1 | !(method %in% private$constantAvailableCoreSelectMethods)) {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as distance method, allowed: '",paste(private$constantAvailableCoreSelectMethods, collapse="', '"),"'"))
        }
      }
      return(.computeMeasure(self, method))
    },
    measures = function(...) {
      m <- c()
      for(method in private$constantAvailableCoreSelectMethods) {
        m <- c(m, .computeMeasure(self, method))
      }
      result <- data.frame(measure=m)
      rownames(result) <- private$constantAvailableCoreSelectMethods
      return(result)
    }
  ),
  active = list(
    distanceMatrix = function(value) {
      if(missing(value)) {
        return(private$variableDistanceMatrix)
      } else {
        cat("Changing distanceMatrix not allowed\n")
      }
    },
    n = function(value) {
      if(missing(value)) {
        return(private$variableN)
      } else {
        cat("Changing n not allowed\n")
      }
    },
    preselected = function(value) {
      if(missing(value)) {
        return(private$variablePreselected)
      } else {
        cat("Changing preselected not allowed\n")
      }
    },
    coreSelectMethod = function(value) {
      if(missing(value)) {
        return(private$variableCoreSelectMethod)
      } else {
        cat("Changing coreSelectMethod not allowed\n")
      }
    },
    adjustedGroupMethod = function(value) {
      if(missing(value)) {
        return(private$variableAdjustedGroupMethod)
      } else {
        cat("Changing adjustedGroupMethod not allowed\n")
      }
    },
    algorithm = function(value) {
      if(missing(value)) {
        return(private$variableAlgorithm)
      } else {
        cat("Changing algorithm not allowed\n")
      }
    },
    randomSelected = function(value) {
      if(missing(value)) {
        return(private$variableRandomSelected)
      } else {
        cat("Changing random selected set not allowed\n")
      }
    },
    adjustedSelected = function(value) {
      if(missing(value)) {
        return(private$variableAdjustedSelected)
      } else {
        cat("Changing adjusted selected set not allowed\n")
      }
    },
    core = function(value) {
      if(missing(value)) {
        return(private$variableCoreSelected)
      } else {
        cat("Changing core not allowed\n")
      }
    },
    randomBasedGroups = function(value) {
      if(missing(value)) {
        df <- data.frame(random=private$variableRandomBasedGroups)
        groups <- split(df,df$random)
        for(groupName in names(groups)) {
          groups[[groupName]] <- rownames(groups[[groupName]])
        }
        return(groups)
      } else {
        cat("Changing random selected groups not allowed\n")
      }
    },
    adjustedBasedGroups = function(value) {
      if(missing(value)) {
        df <- data.frame(adjusted=private$variableAdjustedBasedGroups)
        groups <- split(df,df$adjusted)
        for(groupName in names(groups)) {
          groups[[groupName]] <- rownames(groups[[groupName]])
        }
        return(groups)
      } else {
        cat("Changing adjusted selected groups not allowed\n")
      }
    },
    pop = function(value) {
      if(missing(value)) {
        result <- c()
        labels <- labels(private$variableDistanceMatrix)
        coreLabels <- rownames(private$variableCoreSelected)
        preselectedLabels <- private$variablePreselected
        for(item in labels) {
          if(item %in% coreLabels) {
            if(!is.null(preselectedLabels) && (item %in% preselectedLabels)) {
              result <- c(result, "core - preselected")
            } else {
              result <- c(result, paste0("core - ",private$variableCoreSelectMethod))
            }
          } else {
            result <- c(result, "other")
          }
        }
        return(result)
      } else {
        cat("Changing pop not allowed\n")
      }
    },
    seed = function(value) {
      if(missing(value)) {
        if(!is.null(private$variableSeed)) {
          return(private$variableSeed)
        } else {
          return(private$variableRandomSeed)
        }
      } else {
        cat("Changing seed not allowed\n")
      }
    }

  )
)

CoreCollection <- function(distanceMatrix, n, preselected=c(), coreSelectMethod="A-NE", adjustedGroupMethod="split", algorithm="randomDescent", seed=NULL) {
  return(.CoreCollectionClass$new(distanceMatrix, n, preselected, coreSelectMethod, adjustedGroupMethod, algorithm, seed))
}

setMethod(
  f="summary",
  signature="coreCollection",
  definition=function(object) {
    .showCoreCollection(object, "Summary Core Collection Object")
  }
)




