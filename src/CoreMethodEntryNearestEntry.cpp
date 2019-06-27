#include "CoreMethodEntryNearestEntry.h"
using namespace Rcpp;

const std::string METHOD_ENTRY_NEAREST_ENTRY = "E-NE";

Rcpp::IntegerVector CoreMethodEntryNearestEntry::adjustRandomNeighbour (Rcpp::IntegerVector c, int i) {
  int nl, n = selectedRandomPositions.length(), N = distanceMatrix.nrow(), l=c.length();
  Rcpp::IntegerVector cn;
  Rcpp::NumericVector col;
  double nd, njd, d = 0;
  //find nearest entry of i, store it as k
  int k = i;
  for(int j=0; j<n; j++) {
    if(j!=i) {
      nl = c[i]*N;
      for(int j=i+1;j<(i+l); j++) {
        nd = distanceMatrix[nl+c[(j % l)]];
        if((j==i+1) || (d>nd)) {
          d = nd;
          k = j;
        }
      }
    }
  }
  d = 0;
  cn = clone(c);
  Rcpp::NumericVector coli = groups[selectedRandomPositions[i-1]];
  Rcpp::NumericVector colk = groups[selectedRandomPositions[k-1]];
  Rcpp::NumericVector colj;
  int li = coli.length();
  int lk = colk.length();
  for(int ni=0; ni<li; ni++) {
    nl = coli[ni]*N;
    for(int nk=0; nk<lk; nk++) {
      nd = distanceMatrix[nl+colk[nk]];
      if(nd>d) {
        d = nd;
        cn[i] = coli[ni];
        cn[k] = colk[nk];
      }
    }
  }
  //check other distances
  for(int j=0; j<n; j++) {
    if(j!=(i-1) && j!=(k-1)) {
      colj = groups[selectedRandomPositions[j]];
      d = 0;
      int lj = colj.length();
      for(int nj=0; nj<lj; nj++) {
        njd = distanceMatrix[cn[i]*N+colj[nj]];
        if(njd>d) {
          d = njd;
          cn[j] = colj[nj];
        }
      }
    }
  }
  //final check
  d = measure(c);
  nd = measure(cn);
  if(improvement(d,nd)) {
    c = clone(cn);
  }
  return c;
}

double CoreMethodEntryNearestEntry::measure (Rcpp::IntegerVector & c) {
  return CoreMethodEntryNearestEntry::measure(distanceMatrix, c);
}

//TODO: improve speed
double CoreMethodEntryNearestEntry::measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c) {
  double d, nd, sumOfDistances = 0.0;
  int nl, N = dm.nrow(), l=c.length();
  for(int i=0; i<l; i++) {
    d = 0.0;
    nl = c[i]*N;
    for(int j=i+1;j<(i+l); j++) {
      nd = dm[nl+c[(j % l)]];
      if((j==i+1) || (d>nd)) {
        d = nd;
      }
    }
    sumOfDistances+=d;
  }
  return(sumOfDistances/(double)l);
}

bool CoreMethodEntryNearestEntry::improvement (double m1, double m2) {
  return (m1<m2);
}

