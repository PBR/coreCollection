#ifndef CORE_METHOD_ACCESSION_NEAREST_ENTRY_H
#define CORE_METHOD_ACCESSION_NEAREST_ENTRY_H

#include "CoreMethod.h"

extern const std::string METHOD_ACCESSION_NEAREST_ENTRY;

class CoreMethodAccessionNearestEntry: public CoreMethod {
  private:
    Rcpp::IntegerVector getLocalInitial() {
      return getDefaultInitial();
    }
  public:
    static double distance (Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & entries);
    Rcpp::IntegerVector getInitial() {
      return getLocalInitial();
    }
    CoreMethodAccessionNearestEntry(Rcpp::NumericMatrix & dm, Rcpp::List & g) : CoreMethod(METHOD_ACCESSION_NEAREST_ENTRY, dm, g) {
    };
};

#endif
