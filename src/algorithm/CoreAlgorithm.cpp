#include "CoreAlgorithm.h"

using namespace Rcpp;

CoreAlgorithm::CoreAlgorithm() {

}

Rcpp::IntegerVector CoreAlgorithm::getCore (CoreMethod &m) {
  return m.getInitial();
}

