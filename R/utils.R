#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @export
pmt <- function(balance, term, rate) {
    .Call("_mortgage_pmt", balance, term, rate)
}

is.rate <- function(x) {
    if (x >= 1) {
        message(paste0(x, " interperted as ", x,"%"))
        x / 100
    } else {
        x
    }
}

WAC <- function(pool) {
    balance <- do.call("sum", Map(function(x) x@Principal, pool@Mortages))
    do.call("sum", Map(function(x) x@Rate * (x@Principal / balance), pool@Mortages))
}

WAM <- function(pool) {
    balance <- do.call("sum", Map(function(x) x@Principal, pool@Mortages))
    do.call("sum", Map(function(x) x@Term * (x@Principal / balance), pool@Mortages))
}

Count <- function(pool) {
    types <-  sort(do.call('rbind', Map(function(x) class(x)[1], pool@Mortages)))
    type_factors <- factor(types)
    levels(type_factors) <- c(1,2)
    counts <- tabulate(type_factors)
    names(counts) <- unique(types)
    counts
}

vbind <- function(x) {
    width <- max(sapply(x, function(x) length(x)))
    lapply(x, function(x) c(x, rep(0, width - length(x))))
}

# x <- c(1:5)
# y <- c(1:8)

vbind <- function(x) {
    width  <- max(vapply(x, function(x) length(x), FUN.VALUE = c(0), USE.NAMES = TRUE))
    length <- length(x) 
    vapply(x, function(x) c(x, rep(0, width - length(x))), FUN.VALUE = c(rep(0, width)), USE.NAMES = TRUE)
}

# test =  vbind(list(x,y)) 