#include <Rcpp.h>
#include "selection/CoreSelection.h"
#include "method/CoreMethodAccessionNearestEntry.h"
#include "method/CoreMethodEntryNearestEntry.h"
#include "method/CoreMethodEntryEntry.h"
#include "algorithm/CoreAlgorithmRandomDescent.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerVector computeRandomSelection(Rcpp::NumericMatrix & dist, int requiredN, Rcpp::IntegerVector & preselected) {
  CoreSelection::initialise();
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
Rcpp::IntegerVector computeCore(std::string algorithm, std::string method, Rcpp::NumericMatrix & dist, Rcpp::List & groups) {
  CoreAlgorithm* a;
  CoreMethod* m;
  if(algorithm==ALGORITHM_RANDOM_DESCENT) {
    a = new CoreAlgorithmRandomDescent();
  } else {
    return NULL;
  }
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    m = new CoreMethodAccessionNearestEntry(dist, groups);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    m = new CoreMethodEntryNearestEntry(dist, groups);
  } else if(method==METHOD_ENTRY_ENTRY) {
    m = new CoreMethodEntryEntry(dist, groups);
  } else {
    return NULL;
  }
  return a->getCore(*m);
}

// [[Rcpp::export]]
double computeMeasure(std::string method, Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & c) {
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    return CoreMethodAccessionNearestEntry::measure(dist,c);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    return CoreMethodEntryNearestEntry::measure(dist,c);
  } else if(method==METHOD_ENTRY_ENTRY) {
    return CoreMethodEntryEntry::measure(dist,c);
  } else {
    return 0;
  }
}
