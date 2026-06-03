#' Vector content checks
#'
#' @description
#' Quick checks for what a vector contains. \code{is.constant()} is \code{TRUE}
#' when \code{x} holds at most one unique value (ignoring \code{NA}), and
#' \code{is.binary()} is \code{TRUE} when it holds at most two.
#'
#' @param x object to be tested
#'
#' @return A logical value.
#'
#' @examples
#' is.constant(c(1, 1, 1))     # TRUE
#' is.constant(c(1, 2, 1))     # FALSE
#' is.binary(c("a", "b", NA))  # TRUE
#' is.binary(c("a", "b", "c")) # FALSE
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

