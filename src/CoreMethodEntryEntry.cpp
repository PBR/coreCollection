#include "CoreMethodEntryEntry.h"
using namespace Rcpp;

const std::string METHOD_ENTRY_ENTRY = "E-E";

double CoreMethodEntryEntry::measure (Rcpp::IntegerVector & c) {
  return CoreMethodEntryEntry::measure(distanceMatrix, c);
}

//TODO: improve speed
double CoreMethodEntryEntry::measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c) {
  double d, nd, sumOfDistances = 0;
  int nl, N = dm.nrow(), l=c.length();
  for(int i=0; i<l; i++) {
    nl = c[i]*N;
    for(int j=0;j<l; j++) {
      sumOfDistances+=dm[nl+c[j]];
    }
  }
  return(sumOfDistances/(l*(l-1)));
}

bool CoreMethodEntryEntry::improvement (double m1, double m2) {
  return (m1<m2);
}

