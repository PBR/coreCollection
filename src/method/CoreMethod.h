#ifndef CORE_METHOD_H
#define CORE_METHOD_H

#include <string>
#include <Rcpp.h>

class CoreMethod {
  protected:
    std::string method;
    Rcpp::NumericMatrix distanceMatrix;
    Rcpp::List groups;
    Rcpp::IntegerVector selectedFixedPositions;
    Rcpp::IntegerVector selectedRandomPositions;
    Rcpp::IntegerVector getDefaultInitial() {
      Rcpp::Environment base("package:base");
      Rcpp::Function numeric = base["as.integer"];
      return numeric(Rcpp::_["x"] = groups.names());
    }
  public:
    static double distance (Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & entries);
    std::string getMethod();
    virtual Rcpp::IntegerVector getInitial() {
      std::cout << "Call to default getInitial, should not happen!" << std::endl;
      return NULL;
    };
    Rcpp::IntegerVector getRandomNeighbour(Rcpp::IntegerVector coreInstance);
    CoreMethod(std::string m, Rcpp::NumericMatrix & dm, Rcpp::List & g);
};

#endif
