#' @title Setting Options on ARM loan
#' 
#' @export
arm_opts <- function(fixed_term = 'fixed_term' , period = 'period', lifetime_cap = 'lifetime_cap', lifetime_floor = 'lifetime_floor', periodic_cap = 'periodic_cap', periodic_floor = 'periodic_floor', margin = 'margin', index = c(...)) {
   terms <- list(
       fixed_term = fixed_term, 
       period = period,
       lifetime_cap = lifetime_cap, 
       lifetime_floor = lifetime_floor,
       periodic_cap = periodic_cap,
       periodic_floor = periodic_floor,
       margin = margin, 
       index = index
   ) 
   return(terms)
}
#'
#' @example
#' arm_opts(index = 'TSY1)
#'  
#' @export
arm_values <- function(data = NULL, arm_opts = arm_opts(...)) {
    UseMethod('arm_values', data)    
}
#' @export
arm_values.default <- function(data = NULL, arm_opts = arm_opts(...)) {
    arm_opts
}
#' @export
arm_values.data.frame <- function(data = NULL, arm_opts = arm_opts(...)) {
    fixed_term     <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$fixed_term))
    period         <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$period))
    lifetime_cap   <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$lifetime_cap))
    lifetime_floor <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$lifetime_floor))
    periodic_cap   <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$periodic_cap))
    periodic_floor <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$periodic_floor))
    margin      <- tidyselect::vars_pull(names(data), !! rlang::enquo(arm_opts$margin))
}
