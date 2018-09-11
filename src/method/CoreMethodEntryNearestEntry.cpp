#include "CoreMethodEntryNearestEntry.h"
using namespace Rcpp;

const std::string METHOD_ENTRY_NEAREST_ENTRY = "E-NE";

//TODO: improve speed
double CoreMethodEntryNearestEntry::distance (Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & entries) {
  double d, nd, sumOfDistances = 0;
  int nl, N = dist.nrow(), l=entries.length();
  for(int i=0; i<l; i++) {
    d = 0;
    nl = entries[i]*N;
    for(int j=0;j<l; j++) {
      nd = dist[nl+entries[j]];
      if((j==0) || (d<nd)) {
        d = nd;
      }
    }
    sumOfDistances+=d;
  }
  return(sumOfDistances/l);
}


