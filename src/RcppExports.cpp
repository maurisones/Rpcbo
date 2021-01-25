// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// pcbo
NumericVector pcbo(NumericVector n, int ncpus, int minsupport);
RcppExport SEXP _Rpcbo_pcbo(SEXP nSEXP, SEXP ncpusSEXP, SEXP minsupportSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type ncpus(ncpusSEXP);
    Rcpp::traits::input_parameter< int >::type minsupport(minsupportSEXP);
    rcpp_result_gen = Rcpp::wrap(pcbo(n, ncpus, minsupport));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Rpcbo_pcbo", (DL_FUNC) &_Rpcbo_pcbo, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_Rpcbo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}