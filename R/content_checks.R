#' Contents of Vector Checks
#'
#' Misc/useful functions to easily determine what is contained in a vector.
#'
#' @param x object to be tested
#'
#' @return a logical value
#'
#' @name content_checks
NULL

#' @rdname content_checks
#' @export
is.constant <- function(x){
  n_unique_cc(x) <= 1
}

#' @rdname content_checks
#' @export
is.binary <- function(x){
  n_unique_cc(x) <= 2
}

