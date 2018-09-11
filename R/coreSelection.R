.computeRandomSelectionCoreCollection <- function(object) {
  #get data
  dm <- as.matrix(object$distanceMatrix)
  preselected <- match(object$preselected, labels(object$distanceMatrix))-1
  n <- object$n
  return(computeRandomSelection(dm, n, preselected))
}

.computeAdjustedSelectionCoreCollection <- function(object, groups) {
  #compute adjusted group with correct method
  dm <- as.matrix(object$distanceMatrix)
  if(object$adjustedGroupMethod=="split") {
    preselected <- match(object$preselected, labels(object$distanceMatrix))-1
    return(computeAdjustedSelectionUsingSplitMethod(dm, groups, preselected))
  } else if(object$adjustedGroupMethod=="recompute") {
    adjustedSelected <- rownames(object$randomSelected[which(object$randomSelected[,"preselects"]==0),])
    adjustedSelected <- c(adjustedSelected, object$preselected)
    adjustedSelected <- match(adjustedSelected, labels(object$distanceMatrix))-1
    return(computeAdjustedSelectionUsingRecomputeMethod(dm, adjustedSelected))
  } else {
    stop(paste0("unknown adjustedGroupMethod ",object$adjustedGroupMethod))
  }
}

.computeCoreSelectionCoreCollection <- function(object) {
  dm <- as.matrix(object$distanceMatrix)
  rawGroups <- object$adjustedBasedGroups
  for(groupName in names(rawGroups)) {
    if(groupName %in% object$preselected) {
      rawGroups[[groupName]] <- match(c(groupName), labels(object$distanceMatrix))-1
    } else {
      rawGroups[[groupName]] <- match(rawGroups[[groupName]], labels(object$distanceMatrix))-1
    }
  }
  names(rawGroups) <- match(names(rawGroups), labels(object$distanceMatrix))-1
  result <- computeCore(object$coreSelectMethod,dm,rawGroups)
  if(is.null(result)) {
    stop(paste0("no result, possibly unknown coreSelectMethod ",object$coreSelectMethod))
  }
  return(as.numeric(result))
}

.computeDistance <- function(object, method) {
  if(!missing(method) && !is.null(method)) {
    dm <- as.matrix(object$distanceMatrix)
    entries <- match(rownames(object$core), labels(object$distanceMatrix))-1
    return(computeDistance(method, dm, entries))
  } else {
    return(paste0("couldn't compute distance for method",method))
  }
}
