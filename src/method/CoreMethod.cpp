#include "CoreMethod.h"

using namespace Rcpp;

std::string CoreMethod::getMethod() {
  return method;
}

CoreMethod::CoreMethod(std::string m, Rcpp::NumericMatrix & dm, Rcpp::List & g) {
  method = m;
  distanceMatrix = dm;
  groups = g;
  //administration
  Rcpp::IntegerVector srp(0);
  Rcpp::IntegerVector sfp(0);
  for (int i = 0; i < g.length(); ++i) {
    Rcpp::NumericVector col = g[i];
    if(col.length()>1) {
      srp.push_back(i);
    } else {
      sfp.push_back(i);
    }
  }
  selectedRandomPositions = srp;
  selectedFixedPositions = sfp;
}

Rcpp::IntegerVector CoreMethod::getRandomNeighbour(Rcpp::IntegerVector coreInstance) {
  return coreInstance;
}

