#include "CoreMethodAccessionNearestEntry.h"
using namespace Rcpp;

const std::string METHOD_ACCESSION_NEAREST_ENTRY = "A-NE";

//TODO: improve speed
double CoreMethodAccessionNearestEntry::distance (Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & entries) {
  double d, nd, sumOfDistances = 0;
  int nl = 0, N = dist.nrow();
  for(int i=0; i<N; i++) {
    d = 0;
    for(int j=0;j<entries.length(); j++) {
      nd = dist[nl+entries[j]];
      if((j==0) || (d<nd)) {
        d = nd;
      }
    }
    sumOfDistances+=d;
    nl+=N;
  }
  return(sumOfDistances/N);
}






