#' Contents of Vector Checks
#'
#' @description
#' Misc/useful functions to easily determine what is contained in a vector.
#'
#' @param x object to be tested
#'
#' @return a logical value
#'
#' @examples
#' x <- rep(1, 10)
#' is.constant(x)
#' is.binary(x)
#'
#' x <- c(rep_len(1:2, 10), NA)
#' is.constant(x)
#' is.binary(x)
#'
#' x <- c(rep_len(1:3, 10), NA)
#' is.constant(x)
#' is.binary(x)
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

