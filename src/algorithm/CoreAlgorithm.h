#ifndef CORE_ALGORITHM_H
#define CORE_ALGORITHM_H

#include "../method/CoreMethod.h"
#include <Rcpp.h>

class CoreAlgorithm {
  public:
    virtual Rcpp::IntegerVector getCore (CoreMethod &m) {
      std::cout << "Call to default getCore, should not happen!" << std::endl;
      return NULL;
    }
    CoreAlgorithm();
};

#endif
