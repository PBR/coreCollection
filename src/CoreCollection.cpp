#include <Rcpp.h>
#include "selection/CoreSelection.h"
#include "method/CoreMethodAccessionNearestEntry.h"
#include "method/CoreMethodEntryNearestEntry.h"
#include "algorithm/CoreAlgorithm.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerVector computeRandomSelection(Rcpp::NumericMatrix & dist, int requiredN, Rcpp::IntegerVector & preselected) {
  return CoreSelection::computeRandomSelection(dist, requiredN, preselected);
}

// [[Rcpp::export]]
Rcpp::IntegerVector computeAdjustedSelectionUsingRecomputeMethod(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & adjustedSelected) {
  return CoreSelection::createSelectionResult(dist, adjustedSelected);
}

// [[Rcpp::export]]
Rcpp::IntegerVector computeAdjustedSelectionUsingSplitMethod(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & groups, Rcpp::IntegerVector & preselected) {
  if(preselected.length()>0) {
    int i,j,nl,selectedGroup;
    //full number
    int N = dist.nrow();
    //initialise new
    Rcpp::IntegerVector newGroups(N);
    double newDistances[N], newDistance;
    for(i=0; i<N; i++) {
      newGroups[i] = groups[i];
      newDistances[i] = 0;
    }
    //split existing groups
    int preselectedLength = preselected.length();
    for(j=0; j<preselectedLength; j++) {
      selectedGroup = groups[preselected[j]];
      nl=0;
      for(i=0; i<N; i++) {
        if(i==preselected[j]) {
          newGroups[i]=preselected[j];
          newDistances[i] = 0;
        } else if(groups[i]==selectedGroup) {
          newDistance = dist[nl+preselected[j]];
          if(newGroups[i]==selectedGroup || newDistance<newDistances[i]) {
            newGroups[i]=preselected[j];
            newDistances[i]=newDistance;
          }
        }
        nl+=N;
      }
    }
    return newGroups;
  } else {
    return groups;
  }
}


// [[Rcpp::export]]
Rcpp::IntegerVector computeCore(std::string method, Rcpp::NumericMatrix & dist, Rcpp::List & groups) {
  CoreAlgorithm a;
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    CoreMethodAccessionNearestEntry m(dist, groups);
    m.getInitial();
    return a.getCore(m);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    CoreMethodEntryNearestEntry m(dist, groups);
    return a.getCore(m);
  } else {
    return NULL;
  }
}

// [[Rcpp::export]]
double computeDistance(std::string method, Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & entries) {
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    return CoreMethodAccessionNearestEntry::distance(dist,entries);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    return CoreMethodEntryNearestEntry::distance(dist,entries);
  } else {
    return 0;
  }
}
