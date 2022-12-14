// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// computeRandomSelection
Rcpp::IntegerVector computeRandomSelection(Rcpp::NumericMatrix& dist, int requiredN, Rcpp::IntegerVector& preselected, int seed);
RcppExport SEXP _coreCollection_computeRandomSelection(SEXP distSEXP, SEXP requiredNSEXP, SEXP preselectedSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dist(distSEXP);
    Rcpp::traits::input_parameter< int >::type requiredN(requiredNSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type preselected(preselectedSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(computeRandomSelection(dist, requiredN, preselected, seed));
    return rcpp_result_gen;
END_RCPP
}
// computeAdjustedSelectionUsingRecomputeMethod
Rcpp::IntegerVector computeAdjustedSelectionUsingRecomputeMethod(Rcpp::NumericMatrix& dist, Rcpp::IntegerVector& adjustedSelected);
RcppExport SEXP _coreCollection_computeAdjustedSelectionUsingRecomputeMethod(SEXP distSEXP, SEXP adjustedSelectedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dist(distSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type adjustedSelected(adjustedSelectedSEXP);
    rcpp_result_gen = Rcpp::wrap(computeAdjustedSelectionUsingRecomputeMethod(dist, adjustedSelected));
    return rcpp_result_gen;
END_RCPP
}
// computeAdjustedSelectionUsingSplitMethod
Rcpp::IntegerVector computeAdjustedSelectionUsingSplitMethod(Rcpp::NumericMatrix& dist, Rcpp::IntegerVector& groups, Rcpp::IntegerVector& preselected);
RcppExport SEXP _coreCollection_computeAdjustedSelectionUsingSplitMethod(SEXP distSEXP, SEXP groupsSEXP, SEXP preselectedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dist(distSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type preselected(preselectedSEXP);
    rcpp_result_gen = Rcpp::wrap(computeAdjustedSelectionUsingSplitMethod(dist, groups, preselected));
    return rcpp_result_gen;
END_RCPP
}
// computeCore
Rcpp::IntegerVector computeCore(std::string algorithm, std::string method, Rcpp::NumericMatrix& dist, Rcpp::List& groups);
RcppExport SEXP _coreCollection_computeCore(SEXP algorithmSEXP, SEXP methodSEXP, SEXP distSEXP, SEXP groupsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type algorithm(algorithmSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dist(distSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type groups(groupsSEXP);
    rcpp_result_gen = Rcpp::wrap(computeCore(algorithm, method, dist, groups));
    return rcpp_result_gen;
END_RCPP
}
// computeMeasure
double computeMeasure(std::string method, Rcpp::NumericMatrix& dist, Rcpp::IntegerVector& c);
RcppExport SEXP _coreCollection_computeMeasure(SEXP methodSEXP, SEXP distSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dist(distSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(computeMeasure(method, dist, c));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coreCollection_computeRandomSelection", (DL_FUNC) &_coreCollection_computeRandomSelection, 4},
    {"_coreCollection_computeAdjustedSelectionUsingRecomputeMethod", (DL_FUNC) &_coreCollection_computeAdjustedSelectionUsingRecomputeMethod, 2},
    {"_coreCollection_computeAdjustedSelectionUsingSplitMethod", (DL_FUNC) &_coreCollection_computeAdjustedSelectionUsingSplitMethod, 3},
    {"_coreCollection_computeCore", (DL_FUNC) &_coreCollection_computeCore, 4},
    {"_coreCollection_computeMeasure", (DL_FUNC) &_coreCollection_computeMeasure, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_coreCollection(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
