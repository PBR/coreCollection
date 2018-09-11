#ifndef CORE_ALGORITHM_H
#define CORE_ALGORITHM_H

#include "../method/CoreMethod.h"
#include <Rcpp.h>

class CoreAlgorithm {
  public:
    static Rcpp::IntegerVector getCore (CoreMethod &m);
    CoreAlgorithm();
};

#endif
