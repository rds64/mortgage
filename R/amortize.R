#' @title  Create a Mortgage Class Object
#' @useDynLib mortgage
#' @importFrom Rcpp sourceCpp
#' @importFrom tidyselect vars_pull
#' @importFrom rlang enquo
#' @import tibble
#' @imoortFrom purr unnest
#' @export
#' 
amortize <- function(.data = NULL, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
        UseMethod("amortize", .data)
}
#.
#' @export
amortize.default <- function(.data = NULL, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
    
    if (is.null(id)) {
        id <- c(1:length(principal))
        message("adding id column 1...", length(principal))
    } else {
        id <- id
    }
    
    amort_df <- do.call(rbind, Map(pryr::partial(.Call, "_mortgage_amortize"), principal, term, rate / 12))
    amort_df <- cbind(id, tibble::as.tibble(amort_df))
    tibble::as.tibble(amort_df)
}
#'
#' @export
amortize.data.frame <- function(.data, principal = 'principal', term = 'term', rate = 'rate', id = NULL) {
    principal <- tidyselect::vars_pull(names(.data), !! rlang::enquo(principal))
    term      <- tidyselect::vars_pull(names(.data), !! rlang::enquo(term))   
    rate      <- tidyselect::vars_pull(names(.data), !! rlang::enquo(rate))   
    
    if (is.null(id)) {
        id <- c(1:nrow(.data))
        message("adding id column 1...", nrow(.data))
    } else {
        id <- tidyselect::vars_pull(names(.data), !! rlang::enquo(id))   
    }
     
    amort_df <- do.call(rbind, Map(pryr::partial(.Call, "_mortgage_amortize"), .data[[principal]], .data[[term]], .data[[rate]] / 12))
    amort_df <- cbind(id, tibble::as.tibble(amort_df))
    tibble::as.tibble(amort_df)
}