// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// amortize
List amortize(double principal, int term, float rate);
RcppExport SEXP _mortgage_amortize(SEXP principalSEXP, SEXP termSEXP, SEXP rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type principal(principalSEXP);
    Rcpp::traits::input_parameter< int >::type term(termSEXP);
    Rcpp::traits::input_parameter< float >::type rate(rateSEXP);
    rcpp_result_gen = Rcpp::wrap(amortize(principal, term, rate));
    return rcpp_result_gen;
END_RCPP
}
// pmt
float pmt(double principal, int term, float rate);
RcppExport SEXP _mortgage_pmt(SEXP principalSEXP, SEXP termSEXP, SEXP rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type principal(principalSEXP);
    Rcpp::traits::input_parameter< int >::type term(termSEXP);
    Rcpp::traits::input_parameter< float >::type rate(rateSEXP);
    rcpp_result_gen = Rcpp::wrap(pmt(principal, term, rate));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mortgage_amortize", (DL_FUNC) &_mortgage_amortize, 3},
    {"_mortgage_pmt", (DL_FUNC) &_mortgage_pmt, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_mortgage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}