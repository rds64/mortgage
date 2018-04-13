#' @title Amortize One Or a Pool of Loans
#' @useDynLib mortgage
#' @importFrom Rcpp sourceCpp
#' @importFrom tidyselect vars_pull
#' @importFrom rlang enquo
#' @import tibble
#' @imoortFrom purr unnest
#' @export
#' 
amortize <- function(data = NULL, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
        UseMethod("amortize", data)
    # add arm_options to method dispach so that null options will trigger regular amortization
    # can I use two object determinations methods   
}
#.
#' @export
amortize.default <- function(data = NULL, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
    
    if (is.null(id)) {
        id <- c(1:length(principal))
        message("adding id column 1...", length(principal))
    } else {
        id <- id
    }
    
    amort_df <- do.call(rbind, Map(pryr::partial(.Call, "_mortgage_amortize"), principal, term, rate))
    amort_df <- cbind(id, tibble::as.tibble(amort_df))
    tibble::as.tibble(amort_df)
}
#'
#' @export
amortize.data.frame <- function(data, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
    principal <- tidyselect::vars_pull(names(data), !! rlang::enquo(principal))
    term      <- tidyselect::vars_pull(names(data), !! rlang::enquo(term))   
    rate      <- tidyselect::vars_pull(names(data), !! rlang::enquo(rate))   
    if (!missing(id)) id <- tidyselect::vars_pull(names(data), !! rlang::enquo(id)) 
    
       
    if (!is.null(id)) {
        id <- data[[id]]
    } else if (is.null(id)) {
        id <- c(1:nrow(data))
        message("adding id column 1...", nrow(data))
    } else {
        id <- id
    }
     
    amort_df <- do.call(
        rbind, 
        Map(pryr::partial(.Call, "_mortgage_amortize"), data[[principal]], data[[term]], data[[rate]])
    )
    amort_df <- cbind(id, as.tibble(amort_df))
    tibble::as.tibble(amort_df)
}


