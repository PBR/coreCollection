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
  accessionNumber = dm.nrow();
  coreNumber = g.length();
  fixedCoreNumber = sfp.length();
  randomCoreNumber = srp.length();
}

Rcpp::IntegerVector CoreMethod::getRandomNeighbour(Rcpp::IntegerVector c) {
  int i, j, m, n = selectedRandomPositions.length();
  Rcpp::NumericVector col;
  if(n==0) {
    return c;
  } else {
    i = rand() % n;
    col = groups[selectedRandomPositions[i]];
    m = col.length();
    j = rand() % m;
    c[selectedRandomPositions[i]] = col[j];
  }
  return c;
}

Rcpp::IntegerVector CoreMethod::getRandom() {
  Rcpp::IntegerVector c(groups.length());
  int i, j, n=selectedRandomPositions.length(), m=selectedFixedPositions.length();
  Rcpp::NumericVector col;
  for(i=0;i<n;i++) {
    col = groups[selectedRandomPositions[i]];
    j = rand() % col.length();
    c[selectedRandomPositions[i]] = col[j];
  }
  for(i=0;i<m;i++) {
    col = groups[selectedFixedPositions[i]];
    c[selectedFixedPositions[i]] = col[0];
  }
  return c;
}

