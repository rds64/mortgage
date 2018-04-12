#' @title Calculate the a Missing Loan Term
#' @useDynLib mortgage
#' @importFrom Rcpp sourceCpp
#' @export
mort <- function(balance = NA, payment = NA, rate = NA, term = NA, initial = .05) {
    if (sum(is.na(c(balance, payment, rate, term))) > 1) {
        warning("mort requires at least 3 full arguments")
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

#' @example 
#' mort(balance = 100000, rate = (.04), term = 360)
#' mort(payment = 477.415, rate = .04 / 12, term = 360)
#' mort(balance = 100000, rate = .04 / 12, payment = 477.415)
#' mort(balance = 100000, payment = 477.415, term = 360)
#' mort()

# Formula: P = (Pv*R) / [1 - (1 + R)^(-n)]
# P = Monthly Payment
# Pv = Present Value (starting value of the loan)
# APR = Annual Percentage Rate
# R = Periodic Interest Rate = APR/number of interest periods per year
# n = Total number of interest periods (interest periods per year * number of years)

PMT <- function(rate, term, balance, end_balance, due = c("end", "beg")) {
    due <- match.arg(due, c("end", "beg"))
    if (due == "end") return(((balance) * m_rate) / (1 - (1 + m_rate)^-term))
    if (due == "beg") return()
}
#' @description Same a Excel's PMT function  
#'
#' 
# P = (Pv*R) / [1 - (1 + R)^(-n)] 
payment <- function(balance, rate, term) {
    (balance * m_rate) / (1 - (1 + m_rate)^-term)
}
# (P/i)[1 − (1+i)^-N]
original_balance <- function(payment, rate, term) {
    (payment / rate) * (1 - (1 + rate)^-term)
}
#' @example 
#' original_balance(400, .04, 360)
#' 
#' 
# −log(1−iA/P) / log(1+i)
original_term <- function(balance, payment, rate) {
    -log(1 - rate * balance / payment) / log(1 + rate)
}
#' @example 
#' original_term(100000, 477.4169, .04/12)
#' 


