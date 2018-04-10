
weighted_avg <- function(weight, value) {
    value * (weight / sum(weight))
}

WAC <- function(balances = balance, rates = rate) {
    weighted_avg(balances, rates)
}

WAM <- function(balances, term) {
    weighted_avg(balances, term)
}

WALA <- function(balances, age) {
    weighted_avg(balances, age)
}

WAL <- function(balance, ...) {
    
}