#include "CoreMethodEntryNearestEntry.h"
using namespace Rcpp;

const std::string METHOD_ENTRY_NEAREST_ENTRY = "E-NE";

double CoreMethodEntryNearestEntry::measure (Rcpp::IntegerVector & c) {
  return CoreMethodEntryNearestEntry::measure(distanceMatrix, c);
}

//TODO: improve speed
double CoreMethodEntryNearestEntry::measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c) {
  double d, nd, sumOfDistances = 0;
  int nl, N = dm.nrow(), l=c.length();
  for(int i=0; i<l; i++) {
    d = 0;
    nl = c[i]*N;
    for(int j=0;j<l; j++) {
      nd = dm[nl+c[j]];
      if((j==0) || (d<nd)) {
        d = nd;
      }
    }
    sumOfDistances+=d;
  }
  return(sumOfDistances/l);
}

bool CoreMethodEntryNearestEntry::improvement (double m1, double m2) {
  return (m1<m2);
}

