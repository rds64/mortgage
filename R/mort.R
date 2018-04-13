#' @title Calculate the a Missing Loan Term
#' @family mort
#' @description  the mort function will return the missing term from a loan
#' 
#' The mort function will return the missing term from a loan. 
#' The function will solve for whichever argument is left NA.
#' 
#' Formula: P = (Pv*R) / [1 - (1 + R)^(-n)]
#' P = Monthly Payment
#' Pv = Present Value (starting value of the loan)
#' R = Periodic Interest Rate = APR/number of interest periods per year
#' n = Total number of interest periods (interest periods per year * number of years)
#' 
#' @param balance the initial loan amount
#' @param payment the periodic loan payment amount
#' @param rate the periodic interest rate on the loan
#' @param term the term of the loan in periods
#' @param initial this is the inital rate used in order to numerically 
#' solve for rate using the Newton-Ralphson method. 0.0041 is the default initial value
#' which is 5% over 12 months. You should only need to change this to speed up
#' convergence when the rate is very different than 5%
#' @examples
#' mort(balance = 100000, rate = .04/ 12, term = 360)
#' mort(payment = 477.415, rate = .04 / 12, term = 360)
#' mort(balance = 100000, rate = .04 / 12, payment = 477.415)
#' mort(balance = 100000, payment = 477.415, term = 360) 
#'
#' 
#' @useDynLib mortgage
#' @importFrom Rcpp sourceCpp
#' @export
mort <- function(balance = NA, payment = NA, rate = NA, term = NA, initial = 0.0041) {
    if (sum(is.na(c(balance, payment, rate, term))) > 1) {
        warning("mort requires at least 3 full arguments!")
        return(NA)
    } else if (missing(balance)) {
        balance <- original_balance(payment, rate, term)
        return(balance)
    } else if (missing(payment)) {
        payment <- payment(balance, rate, term)
        return(payment)
    } else if (missing(rate)) {
       rate <- .Call("_mortgage_solve_rate", balance, payment, term, initial)
       return(rate)
    } else if (missing(term)) {
        term <- original_term(balance, payment, rate)
        return(term)
    } else {
        warning("No Missing Arguments Detected!")
        return(NA)
    }
}

#'
#' @name payment
#' @family mort
#' P = (Pv*R) / [1 - (1 + R)^(-n)] 
payment <- function(balance, rate, term) {
    (balance * rate) / (1 - (1 + rate)^-term)
}
#'
#' @name original_balance
#' @family mort
#' (P/i)[1 − (1+i)^-N]
original_balance <- function(payment, rate, term) {
    (payment / rate) * (1 - (1 + rate)^-term)
}
#' @example 
#' original_balance(400, .04, 360)
#' 
#' @name original_term
#' @family mort
#' −log(1−iA/P) / log(1+i)
original_term <- function(balance, payment, rate) {
    as.integer(round(-log(1 - rate * balance / payment) / log(1 + rate)))
}
#' @example 
#' original_term(100000, 477.4169, .04/12)
#' 

#'        f(i) = P − P (1+i)^-N − iA
#'  i - ------------------------------
#'        f′(i) = N P (1+i)^(-N-1) − A
 

