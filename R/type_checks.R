#' Type Checks
#'
#' @description
#' Misc/useful type checks to prevent duplicated code.
#'
#' @param x object to be tested
#' @param y object to be tested
#' @param na.rm If true, NA values aren't considered bad for calculations
#' @param ... values to be tested
#'
#' @return a logical value
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @name type_checks
NULL

#' @rdname type_checks
#' @export
is.scalar <- function(x){
  is.atomic(x) && (length(x) == 1L)
}

#' @rdname type_checks
#' @export
is.scalar_or_null <- function(x){
  is.scalar(x) || is.null(x)
}

#' @rdname type_checks
#' @export
is.numeric_or_null <- function(x){
  is.numeric(x) || is.null(x) || all(is.na(x))
}

#' @rdname type_checks
#' @export
is.character_or_null <- function(x){
  is.character(x) || is.null(x) || all(is.na(x))
}

#' @rdname type_checks
#' @export
is.logical_or_null <- function(x){
  is.logical(x) || is.null(x) || all(is.na(x))
}

#' @rdname type_checks
#' @export
is.df_or_null <- function(x){
  is.data.frame(x) || is.null(x)
}

#' @rdname type_checks
#' @export
is.list_or_null <- function(x){
  is.list(x) || is.null(x)
}

#' @rdname type_checks
#' @export
is.atomic_nan <- function(x){
  if(!is.atomic(x)){
    out <- rep(FALSE, length(x))
  } else{
    out <- is.nan(x)
  }

  return(out)
}

#' @rdname type_checks
#' @export
is.irregular_list <- function(x){
  is.list(x) && !is.data.frame(x)
}

#' @rdname type_checks
#' @export
is.bad_for_calcs <- function(x){
  if(!is.atomic(x) || !length(x)){
    return(TRUE)
  } else{
    return(is.na(x) | is_nan(x) | is.na(as.numeric(x)) | is.factor(x) | is.infinite(x) )
  }
}

#' @rdname type_checks
#' @export
any_bad_for_calcs <- function(x, ..., na.rm = FALSE){
  x <- c(x, ...)
  any(is.bad_for_calcs(x, na.rm))
}

#' @rdname type_checks
#' @export
all_good_for_calcs <- function(x, ..., na.rm = FALSE){
  x <- c(x, ...)
  all(!is.bad_for_calcs(x, na.rm))
}


#' @rdname type_checks
#' @export
is.bad_for_indexing <- function(x){
  !is.scalar(x) || any_bad_for_calcs(x)
}

#' @rdname type_checks
#' @export
is.good_for_indexing <- function(x){
  is.scalar(x) || all_good_for_calcs(x)
}


#' @rdname type_checks
#' @export
is.bad_and_equal <- function(x, y){

  # checking whether x/y are nan (if they are atomic)
  nan_x <- is.nan(x)
  nan_y <- is.nan(y)

  # checking whether x/y are na
  na_x  <- is.na(x) & !nan_x
  na_y  <- is.na(y) & !nan_y

  # comparing
  (na_x & na_y) | (nan_x & nan_y)
}

#' @rdname type_checks
#' @export
is.bad_for_calcs <- function(x, na.rm = FALSE){
  if(!is.atomic(x) || !length(x)){
    return(TRUE)
  } else{
    return(is.nan(x) | is.infinite(x) | (is.na(x) * isTRUE(na.rm)))
  }
}

#' @rdname type_checks
#' @export
is.good_for_calcs <- function(x, na.rm = FALSE){
  return(!is.bad_for_calcs(x, na.rm))
}

#' @rdname type_checks
#' @export
is.null_or_na <- function(x){
  out <- is.null(x) | is.na(x)
  out[length(out)==0] <- TRUE
  out
}
