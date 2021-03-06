# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

amortize <- function(principal, term, rate) {
    .Call('_mortgage_amortize', PACKAGE = 'mortgage', principal, term, rate)
}

pmt <- function(principal, term, rate) {
    .Call('_mortgage_pmt', PACKAGE = 'mortgage', principal, term, rate)
}

check <- function(balance, payment, term, rate) {
    .Call('_mortgage_check', PACKAGE = 'mortgage', balance, payment, term, rate)
}

solve_rate <- function(balance, payment, term, start = .05) {
    .Call('_mortgage_solve_rate', PACKAGE = 'mortgage', balance, payment, term, start)
}

WAC <- function(balance, rate) {
    .Call('_mortgage_WAC', PACKAGE = 'mortgage', balance, rate)
}

